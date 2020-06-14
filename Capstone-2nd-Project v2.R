##############################################################################
# TERRY MCLAUGHLIN - CAPSTONE INDIVIDUAL PROJECT
##############################################################################

#R Script used for Capstone Individual Project

##########################################################################################################
# Install Packages if not there and Load libraries needed for project
##########################################################################################################
if(!require(dslabs)) 
  install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) 
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) 
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(formattable)) 
  install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) 
  install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) 
  install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Amelia)) 
  install.packages("Amelia", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) 
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(productplots)) 
  install.packages("productplots", repos = "http://cran.us.r-project.org")
if(!require(ggmosaic)) 
  install.packages("ggmosaic", repos = "http://cran.us.r-project.org")


library(dslabs)
library(tidyverse)
library(lubridate)
library(caret)
library(data.table)
library(dplyr)
library(kableExtra)
library(formattable)
library(ggrepel)
library(randomForest)
library(Amelia)
library(ggplot2)
library(productplots)
library(ggmosaic)

####################################################################################
# Download dataset
####################################################################################
if(!require(tidyverse)) 
      install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
      install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
      install.packages("data.table", repos = "http://cran.us.r-project.org")

# Champaign Jail Dataset
# https://data.illinois.gov/dataset/jail-booking-data
url <- "https://data.illinois.gov/dataset/820f5916-069f-41e8-afc2-815a26770bea/resource/dfcad952-67cc-4ff3-8be0-0312c25ec8e1/download/chm_jail_data.csv"
jaildata <- read_csv(url)
download.file(url, "chm_jail_data.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
jaildata_tbl <- read_csv(tmp_filename)
file.remove(tmp_filename)

#let's look at the tibble
head(jaildata_tbl)
class(jaildata_tbl)
str(jaildata_tbl)


#read file in locally (to speed up testing) 
jaildata_tbl <- read_csv("C:/Users/terry.mclaughlin/OneDrive/Documents/Capstone-2nd-Project/chm_jail_data.csv")
jaildata_tbl
####################################################################################
# Clean the dataset - prepare for machine learning algorithm
####################################################################################
#first convert jaildata to a dataframe 
chmJailDataRaw <- as.data.frame(jaildata_tbl)
head(chmJailDataRaw)
class(chmJailDataRaw)
names(chmJailDataRaw)
dim(chmJailDataRaw)

#look at the missing values
missmap(chmJailDataRaw, main = "Missing Values vs Observed")

#first - pull out columns we want and add column for over 30,  booking year, 
#and crime severity
chmJailData <- chmJailDataRaw %>% select(c(booking_date, 
                                           jacket_number, age_at_arrest,
                                           marital_status, race, sex, 
                                           military, prisoner_type, crime, 
                                           superhighlevel)) %>%
         mutate(booking_year = as.integer(str_sub(chmJailDataRaw$booking_number,1,4)), 
         over30 = factor(ifelse(age_at_arrest <= 30, "<=30", ">30")) , 
         crime_severity = ifelse(superhighlevel == "Other", 10, 5) )


#get rid of null rows for and remove the 7 rows that are prior to 2012
chmJailData <- chmJailData %>% filter(!is.na(age_at_arrest) & 
                                        !is.na(superhighlevel) & 
                                        !is.na(marital_status) & 
                                        !is.na(race) & 
                                        !is.na(sex) &
                                        !is.na(military) & 
                                        !is.na(crime) & 
                                        !is.na(prisoner_type)
                                      & booking_year >= 2012)

#####################Block to add age category by decade   ##################
#start at 10 and end at 90 as 16 is youngest and 85 is oldest
ageDecade <- c(paste(seq(10, 85, by = 10), seq(10 + 10 - 1, 90 - 1, by = 10),
                     sep = "-"))
ageDecade

class(chmJailData$age_at_arrest)
chmJailData['age_group']  <- cut(chmJailData$age_at_arrest, 
                                 breaks = c(seq(10,85, by = 10), Inf), 
                                 labels = ageDecade, right = FALSE)


########Block to change the crime_severity  to an integer value  ############


########Block to change the crime_severity  to an integer value  ############
gsr <- function(Source, Search, Replace)
{if (length(Search) != length(Replace))
    stop("Search and Replace Must Have Equal Number of Items\n")
  
  Changed <- as.character(Source)
  
  for (i in 1:length(Search))
  {cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
    Changed <- replace(Changed, Changed == Search[i], Replace[i])}
  cat("\n")
  Changed}

chmJailData$crime_severity <- gsr(chmJailData$superhighlevel, 
                                  c("Domestic Violence","Drug",
                                    "DUI","Other", "Property",
                                    "Public Order","Sex",
                                    "Traffic","Violent"),
                                  c("7", "5", "4", "9", "2", 
                                    "3", "6", "1", "8"))

unique(chmJailData$crime_severity)

###########end block on crime severity ####################

#Change certain columns to factors or numeric - needed for machine learning model
chmJailData$marital_status <- as.factor(chmJailData$marital_status)
chmJailData$race <- as.factor(chmJailData$race)
chmJailData$sex <- as.factor(chmJailData$sex)
chmJailData$military <- as.factor(chmJailData$military)
chmJailData$superhighlevel <- as.factor(chmJailData$superhighlevel)
chmJailData$prisoner_type <- as.factor(chmJailData$prisoner_type)
chmJailData$jacket_number <- as.factor(chmJailData$jacket_number)
chmJailData$crime_severity <- as.numeric(chmJailData$crime_severity)

summary(chmJailData)
str(chmJailData)
dim(chmJailData)
################################################################################
# Examine the dataset - General Statistics
################################################################################
#Pull out key info
##add as.character to control decimal points displayed
chmJailData_summary <- chmJailData %>%
  summarize(Number_Of_Rows = as.character(nrow(chmJailData)), 
            Number_Of_Columns = as.character(ncol(chmJailData)),
            No_PeopleArrested = as.character(n_distinct(jacket_number)),
            No_Different_Crimes = as.character(n_distinct(crime)), 
            Minimum_age = min(age_at_arrest,na.rm = TRUE),
            Maximum_age = max(age_at_arrest,na.rm = TRUE),
            Average_age = round(mean(age_at_arrest,na.rm = TRUE),2), 
            First_Year_Of_Data = as.character(min(booking_year)), 
            Last_Year_Of_Data = as.character(max(booking_year)), 
            Percent_Males = round(sum(chmJailData$sex=='Male',na.rm = TRUE)/n()*100, 2), 
            Percent_Females = round(sum(chmJailData$sex=='Female',na.rm = TRUE)/n()*100, 2))

#transpose data for better readablilty
transposeSummary <- data.frame(t(chmJailData_summary))
transposeSummary
############################################################################
# Display Statistic Outputs
############################################################################
#Transformed Data Header
head(chmJailData)

#Display Summary
kable(transposeSummary, align = c("c"), 
      col.names = c("Value")) %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  kable_styling(latex_options = "basic")

####################################################################################

####################################################################################
# Data Visualization
####################################################################################

####################################################################################
# SUMMARY CHARTS
####################################################################################
#Age Distribution Histogram
chmJailData %>% 
  group_by(jacket_number) %>% 
  summarize(avg = mean(age_at_arrest)) %>% 
  ggplot(aes(avg)) + 
  geom_histogram(bins = 10, color = "gray77", fill = "lightsteelblue4") +
  geom_vline(xintercept=30.7, linetype="dashed", color = "coral",size=1) +
  ggtitle("Average Age at Arrest") +
  xlab("Age at Arrest") +
  ylab("Number of Bookings") 

# crimes per year
chmJailData %>% 
  group_by(booking_year) %>% 
  summarize(Total=n())  %>%
  ggplot(aes(x=booking_year, y=Total)) +
  geom_line(color = "blue4", size=1) +
  ggtitle("Total Number of Bookings by Year") + 
  xlab("Year")+
  ylab("Number of Bookings")  


# Crimes by type
chmJailData %>% 
  group_by(superhighlevel)%>%
  summarize(Total = n()) %>%
  ggplot(aes(x= reorder(superhighlevel, Total), y = Total)) + 
  geom_bar(stat = "identity", color = "#aeb6bf", fill = "#34495e") +
  ggtitle("Number of Crimes by Type") + 
  xlab("Type of Crime")+
  ylab("Number of Crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

####################################################################################
# DEMOGRAPHIC CHARTS
####################################################################################

####################################################################################
# MARITAL STATUS
####################################################################################
#Crimes by Marital Status 
chmJailData %>% 
  group_by(marital_status) %>%
  summarize(Total = n()) %>%
  ggplot(aes(x= reorder(marital_status, Total), y = Total)) + 
  geom_bar(stat = "identity", color = "#aeb6bf", fill = "#34495e") +
  ggtitle("Crimes by Marital Status") + 
  xlab("Marital Status")+
  ylab("Number of Crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

#Proportion by Marital Status
chmJailData %>% 
  ggplot(aes(fct_infreq(superhighlevel))) +
  geom_bar(aes(fill=marital_status), color = "gray77") +
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Crime Distribution by Marital Status") +
  xlab("Crime Type")+
  ylab("Number of Crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


####################################################################################
# RACE
####################################################################################
#Crimes by Race
chmJailData %>% 
  group_by(race)%>%
  summarize(Total = n()) %>%
  ggplot(aes(x= reorder(race, Total), y = Total)) + 
  geom_bar(stat = "identity", color = "#aeb6bf", fill = "#34495e") +
  ggtitle("Crimes by Race") + 
  xlab("Marital Status")+
  ylab("Number of Crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

#Crime Distribution by Race
chmJailData %>% 
  ggplot(aes(superhighlevel)) +
  geom_bar(aes(fill=race), color = "gray77") +
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Crime Distribution by Race")+
  xlab("Crime Type")+
  ylab("Number of Crimes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~sex)


#mosaic by race, crime type, faceted by <=30 and >30
chmJailData %>%
  mutate(CrimeType = superhighlevel)  %>%
  ggplot()+
  geom_mosaic(aes(x = product(race, CrimeType), 
                  fill = race)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("f(Crime, Race | Over or Under 30)") +
  xlab("Crime Type") +
  facet_grid(over30~.)



####################################################################################
# CHARTS NOT USED IN REPORT
####################################################################################

#Age Distribution for Crimes
chmJailData %>% 
  ggplot(aes(superhighlevel)) +
  geom_bar(aes(fill=age_group), color = "gray77") +
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Crime Distribution by Age Group and Sex")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~sex)


#Proportion by Type of crime
chmJailData %>% 
  ggplot(aes(superhighlevel, fill = over30)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2")+
  ggtitle("Over 30 Distribution by Type of Crime")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#density plots
summaryPlot <-chmJailData %>% group_by(marital_status, age_group) %>%
    summarize(Total = n() /nrow(chmJailData))
summaryPlot
chmJailData %>% 
  filter(race != "Unknown") %>% 
  ggplot(aes(x = age_at_arrest, 
             fill = race)) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Age distribution by Race")+
  xlab("Age at Arrest")

chmJailData %>% filter(marital_status != "Unknown") %>%
  ggplot(aes(x = age_at_arrest, 
             fill = marital_status)) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Age distribution by Race")+
  xlab("Age at Arrest")

####################################################################################
# MOSAIC CHARTS NOT USED IN REPORT
####################################################################################

ggplot(data = chmJailData) +
  geom_mosaic(aes(x = product( race, age_group), 
        fill = race)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Age_Group ", title="f(Race, Age_Group)")

ggplot(data = chmJailData) +
  geom_mosaic(aes(x = product(race, superhighlevel), 
                  fill = race)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crime Type ", title="f(Crime, Race | Over or Under 30)") +
  facet_grid(over30~.)

ggplot(data = chmJailData) +
  geom_mosaic(aes(x = product(age_group, superhighlevel), 
                  fill = age_group)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Crime Type ", title="f(Crime, Age_Group)") +
  facet_grid(sex~.)


####################################################################################
# Split the dataset to create train set, test set, and a validation set
####################################################################################
#first let's define predictor and oucome

# Validation set will be 20% of the data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(chmJailData$over30, times = 1, p = 0.2, list = FALSE)
jail_train <- chmJailData[-test_index,] 
validation <- chmJailData[test_index,] 
summary(validation)

#Split further for a test set
#now y is the train data age
y<- jail_train$over30
#split again to obtain a test set so we can test various models - 
#    need to see if we change the seed
set.seed(10, sample.kind="Rounding")
test_indexsplit <- createDataPartition(jail_train$over30, times = 1, p = 0.2, list = FALSE)
training <- jail_train[-test_indexsplit,] #train
test <- jail_train[test_indexsplit,] #test

head(training)
str(training)
str(test)
nearZeroVar(training, saveMetrics = TRUE)

####################################################################################
# Basic lm linear regression Model - not used
####################################################################################

#fit_lm <- lm(over30 ~ superhighlevel, data = training)
#fit_lm$coefficients

####################################################################################
# Models
####################################################################################


############################################################################
# Models 1-4 Generative Models
############################################################################

############################################################################
# Model 1 - Naive Bayes
############################################################################
set.seed(20, sample.kind="Rounding")
train_nb <- train(over30 ~ crime_severity + race + 
                    marital_status + sex + military + 
                    booking_year,
                  method = "naive_bayes", data = training)
train_nb$results
nb_test_accuracy <- confusionMatrix(predict(train_nb, test), 
                                   test$over30)$overall["Accuracy"]
nb_test_accuracy
#display the importance of the variables
varImp(train_nb)

#nb on validation
train_nb_val <- train(over30 ~ crime_severity + race + 
                         marital_status + sex + military + 
                         booking_year,
                       method = "naive_bayes", data = jail_train)
nb_val_accuracy <- confusionMatrix(predict(train_nb_val, validation), 
                                   validation$over30)$overall["Accuracy"]
nb_val_accuracy
#display the importance of the variables
varImp(train_nb_val)

model_results = ''
model_results <- tibble(method = "Model 1 - Naive Bayes", 
                        TestAcc = nb_test_accuracy, ValAcc =nb_val_accuracy)
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 1, color = "#fbfcfc", background = "#7b241C") %>%
   kable_styling(latex_options = "basic")


############################################################################
# Model 2 - QDA
############################################################################
train_qda <- train(over30 ~ crime_severity + race + 
                     marital_status + sex + military + 
                     booking_year, 
                   method = "qda", data = training)
train_qda$results
qda_test_accuracy <- confusionMatrix(predict(train_qda, test), 
                                    test$over30)$overall["Accuracy"]
qda_test_accuracy
#display the importance of the variables
varImp(train_qda)

#qda on validation
train_qda_val <- train(over30 ~ crime_severity + race + 
                         marital_status + sex + military + 
                         booking_year,
                       method = "qda", data = jail_train)
qda_val_accuracy <- confusionMatrix(predict(train_qda_val, validation), 
                                    validation$over30)$overall["Accuracy"]
qda_val_accuracy
#display the importance of the variables
varImp(train_qda_val)

#add rows to a running table to summarize all model results
model_results <- bind_rows(model_results,
                          tibble(method="Model 2 - QDA",
                                 TestAcc = qda_test_accuracy, 
                                 ValAcc = qda_val_accuracy))
model_results
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 3, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 2, color = "#fbfcfc", background = "#7b241C") %>%
  kable_styling(latex_options = "basic")

############################################################################
# Model 3 - LDA
############################################################################
train_lda <- train(over30 ~ crime_severity + race + 
                     marital_status + sex + military + 
                     booking_year, 
                   method = "lda", data = training)
train_lda$results
lda_test_accuracy <- confusionMatrix(predict(train_lda, test), 
                                     test$over30)$overall["Accuracy"]
lda_test_accuracy
#display the importance of the variables
varImp(train_lda)

#lda on validation
train_lda_val <- train(over30 ~ crime_severity + race + 
                         marital_status + sex + military + 
                         booking_year,
                       method = "lda", data = jail_train)
lda_val_accuracy <- confusionMatrix(predict(train_lda_val, validation), 
                                    validation$over30)$overall["Accuracy"]
lda_val_accuracy
#display the importance of the variables
varImp(train_lda_val)

#add rows to a running table to summarize all model results
model_results <- bind_rows(model_results,
                           tibble(method="Model 3 - LDA",
                                  TestAcc = lda_test_accuracy, 
                                  ValAcc = lda_val_accuracy))
model_results
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 3, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 3, color = "#fbfcfc", background = "#7b241C") %>%
  kable_styling(latex_options = "basic")

############################################################################
# Model 4 - GLM
############################################################################
train_glm <- train(over30 ~ crime_severity + race + 
                     marital_status + sex + military + 
                     booking_year,
                   method = "glm", data = training)
train_glm$results
glm_test_accuracy <- confusionMatrix(predict(train_glm, test), 
                                     test$over30)$overall["Accuracy"]
glm_test_accuracy
#display the importance of the variables
varImp(train_glm)

#glm on validation
train_glm_val <- train(over30 ~ crime_severity + race + 
                     marital_status + sex + military + 
                     booking_year,
                   method = "glm", data = jail_train)
glm_val_accuracy <- confusionMatrix(predict(train_glm_val, validation), 
                                    validation$over30)$overall["Accuracy"]
glm_val_accuracy
#display the importance of the variables
varImp(train_glm_val)

#add rows to a running table to summarize all model results
model_results <- bind_rows(model_results,
                           tibble(method="Model 4 - GLM",
                                  TestAcc = glm_test_accuracy, 
                                  ValAcc = glm_val_accuracy))
model_results
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 3, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 4, color = "#fbfcfc", background = "#7b241C") %>%
  kable_styling(latex_options = "basic")

############################################################################
# Models 5 and 6 - Ensembles
############################################################################


############################################################################
# Ensembles using test set
############################################################################
#now let's try ensemble to see if we can improve our accuracy
#example taken from quiz in course materials section 6.1
#Ensembles combine multiple machine learning algorithms into one model to improve predictions

#train all the models
models <- c("naive_bayes", "qda", "lda", "glm")

fits <- lapply(models, function(model){ 
  print(model)
  train(over30 ~ crime_severity + race + 
          marital_status + sex + military + 
          booking_year, 
          method = model, data = training)
}) 

names(fits) <- models

#generate matrix of predictions
pred <- sapply(fits, function(object) 
  predict(object, newdata = test))
dim(pred)

#compute Accuracy
acc <- colMeans(pred == test$over30)
acc
mean(acc)

#build ensemble to build prediction - over30 and under 30
over30Results <- rowMeans(pred == ">30")
y_hat <- ifelse(over30Results > 0.5, ">30", "<=30")
mean(y_hat == test$over30)

#calculation mean of accuracy
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
ens_all_accuracy <- mean(acc_hat)
ens_all_accuracy

#only consider high accuracy
ind <- acc_hat >= 0.7
over30Reults2 <- rowMeans(pred[,ind] == ">30")
y_hat2 <- ifelse(over30Reults2>=0.5, ">30", "<=30")
mean(y_hat2 == test$over30)
ens_ldaglm_accuracy <- mean(y_hat2 == test$over30)
ens_ldaglm_accuracy


############################################################################
# Ensembles using validation set - don't display code in pdf
############################################################################
#train all the models
models <- c("naive_bayes", "qda", "lda", "glm")

fits <- lapply(models, function(model){ 
  print(model)
  train(over30 ~ crime_severity + race + 
          marital_status + sex + military + 
          booking_year,
          method = model, data = jail_train)
}) 

names(fits) <- models
names(fits)

#generate matrix of predictions
predv <- sapply(fits, function(object) 
  predict(object, newdata = validation))
dim(predv)

#compute Accuracy
accv <- colMeans(predv == validation$over30)
accv
mean(accv)

#build ensemble to build prediction - over30 and under 30
over30Resultsv <- rowMeans(predv == ">30")
y_hatv <- ifelse(over30Resultsv > 0.5, ">30", "<=30")
mean(y_hatv == validation$over30)

#calculation mean of accuracy
acc_hatv <- sapply(fits, function(fit) min(fit$results$Accuracy))
ens_all__val_accuracy <- mean(acc_hatv)
ens_all__val_accuracy

#only consider high accuracy
ind <- acc_hatv >= 0.7
over30Reults2v <- rowMeans(predv[,ind] == ">30")
y_hat2v <- ifelse(over30Reults2v >=0.5, ">30", "<=30")
mean(y_hat2v == validation$over30)
ens_ldaglm__val_accuracy <- mean(y_hat2v == validation$over30)
ens_ldaglm__val_accuracy

#############display results of ensemble models ###################
#add rows to a running table to summarize all model results
model_results <- bind_rows(model_results,
                           tibble(method="Model 5 - Ensemble with Naive Bayes, QDA, LDA, and GLM",
                                  TestAcc = ens_all_accuracy, 
                                  ValAcc = ens_all__val_accuracy))
model_results
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 3, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 5, color = "#fbfcfc", background = "#7b241C") %>%
  kable_styling(latex_options = "basic")


#add rows to a running table to summarize all model results
model_results <- bind_rows(model_results,
                           tibble(method="Model 6 - Ensemble with LDA and GLM",
                                  TestAcc = ens_ldaglm_accuracy, 
                                  ValAcc = ens_ldaglm__val_accuracy))
model_results
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 3, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 6, color = "#fbfcfc", background = "#7b241C") %>%
  kable_styling(latex_options = "basic")


############################################################################
# Models 7 and 8 - Trees and Forests
############################################################################


############################################################################
# Model 7 - Classification Tree (rPart)
############################################################################

#check code link https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+1T2020/courseware/3f70797a568946e195863415b94d25fd/78cfd6af400d452c9aa9f8070403e55c/?child=first
set.seed(30, sample.kind="Rounding")
train_rpart <- train(over30 ~ crime_severity + race + 
                     marital_status + sex + military + 
                     booking_year, 
                     method = "rpart", 
                     tuneGrid = data.frame(cp=seq(0,0.01, len=100)),
                     data = training)

train_rpart$bestTune
ggplot(train_rpart, highlight = TRUE)
rpart_accuracy <- confusionMatrix(predict(train_rpart, test), test$over30)$overall["Accuracy"]
rpart_accuracy


#rPart on validation
val_rpart <- train(over30 ~ crime_severity + race + 
                       marital_status + sex + military + 
                       booking_year, 
                     method = "rpart", 
                     tuneGrid = data.frame(cp=seq(0,0.01, len=100)),
                     data = jail_train)
val_rpart$bestTune
ggplot(val_rpart, highlight = TRUE)
rpart_val_accuracy <- confusionMatrix(predict(val_rpart, validation), 
                                      validation$over30)$overall["Accuracy"]
rpart_val_accuracy
varImp(val_rpart)

#add rows to a running table to summarize all model results
model_results <- bind_rows(model_results,
                           tibble(method="Model 7 - Classification Tree",
                                  TestAcc = rpart_accuracy, 
                                  ValAcc = rpart_val_accuracy))
model_results
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 3, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 7, color = "#fbfcfc", background = "#7b241C") %>%
  kable_styling(latex_options = "basic")



############################################################################
# Model 8 - Random Forest
############################################################################
# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+1T2020/courseware/3f70797a568946e195863415b94d25fd/78cfd6af400d452c9aa9f8070403e55c/?child=first


set.seed(40, sample.kind="Rounding")

rf_train <- randomForest(over30 ~ crime_severity + race + 
                         marital_status + sex + military + 
                         booking_year,
                         data = training)
rf_accuracy <- confusionMatrix(predict(rf_train, test), 
                               test$over30)$overall["Accuracy"]
rf_accuracy
varImp(train_rf)

#Test RF on validation set
set.seed(50, sample.kind="Rounding")
rf_validation <- randomForest(over30 ~ crime_severity + race + 
                                marital_status + sex + military + 
                                booking_year,
                                data = jail_train)

rf_val_accuracy <- confusionMatrix(predict(rf_validation, validation), 
                                   validation$over30)$overall["Accuracy"]
#add rows to a running table to summarize all model results
model_results <- bind_rows(model_results,
                           tibble(method="Model 8 - Random Forest",
                                  TestAcc = rf_accuracy, 
                                  ValAcc = rf_val_accuracy))
model_results
#display results
kable(model_results, 
      align = c("l","c", "c"), 
      col.names = c("Method", "Test Set Accuracy", "Validation Set Accuracy")) %>%
  column_spec(column = 3, color = "#fbfcfc", background = "#21618c") %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  row_spec(row = 8, color = "#fbfcfc", background = "#7b241C") %>%
  kable_styling(latex_options = "basic")


############################################################################
# 
############################################################################


###################################################### END  ###################################


