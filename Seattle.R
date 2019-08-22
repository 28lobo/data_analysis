rm(list = ls())
Sea<- read.csv("Seattle.csv",header = TRUE)
str(Sea)
names(Sea)
library(dplyr)
names(features1)
table(Sea$preapproval_name)
table(Sea$purchaser_type_name)

summary(Sea$population)
summary(Sea$minority_population)
summary(Sea$loan_amount_000s)
summary(Sea$applicant_income_000s)
Sea$missing_inc<-ifelse(is.na(Sea$applicant_income_000s),
                        "Y","N")
features2<- c("population","minority_population","loan_amount_000s",
              "applicant_income_000s","loan_purpose_name",
              "applicant_sex_name","applicant_race_name_1",
              "action_taken_name","purchaser_type_name","property_type_name",
              "preapproval_name","loan_type_name","loan_purpose_name",
              "agency_name","county_name","missing_inc")
Sea<-Sea[,features2]
str(Sea)
names(Sea)
library(caret)
#missing data
dummy_vars<- dummyVars(~., data = Sea[,-8])
Sea_dummy<- predict(dummy_vars,Sea[,-8])
View(Sea_dummy)
#impute
pre_process<- preProcess(Sea_dummy, method = "medianImpute")
?preProcess
imputed_data<-predict(pre_process,Sea_dummy)
?knnImpute
library(RANN)
View(imputed_data)
is.data.frame(imputed_data)
names(imputed_data)
Sea$applicant_income_000s<-imputed_data[,4]
# remove categorical variables
library(e1071)
library(ggplot2)
income<-ggplot(Sea,aes(Sea$applicant_income_000s))
income+
  geom_histogram(binwidth = 50)
Sea$Black[Sea$applicant_race_name_1=="Black or African American"]="1"
is.factor(Sea$Black)
Sea$Black[Sea$applicant_race_name_1=="American Indian or Alaska Native"]="0"
Sea$Black[Sea$applicant_race_name_1=="White"]="0"
Sea$Black[Sea$applicant_race_name_1=="Asian"]= "0"
Sea$Black[Sea$applicant_race_name_1 == "Native Hawaiian or Other Pacific Islander"]= "0"
Sea$Denied[Sea$action_taken_name=="Application denied by financial institution"]="0"
Sea$Denied[Sea$action_taken_name=="Loan originated"]="1"
Sea$Denied<-as.factor(Sea$Denied)
Sea$Black<-as.factor(Sea$Black)
#Data Splitting
prop.table(table(Sea$Denied))
set.seed(2637)
indexes<- createDataPartition(Sea$Denied,
                              times = 1,
                              p = 0.7,
                              list = FALSE)
sea.train<- Sea[indexes,]
sea.test<- Sea[-indexes,]
prop.table(table(Sea$Denied))
prop.table(table(sea.train$Denied))
prop.table(table(sea.test$Denied))
?train
library(Boruta)
library(MASS)
Sea$Denied<- as.factor(Sea$Denied)
initial<- glm(Denied~ trans + population + minority_population + loan_amount_000s+ Black + Asian + AIAN + NHPI, data = Sea, family = "binomial")
stepAIC(initial,direction ="both" )

logReg1<- train(Denied~ Black, data = sea.train,
                method = "glm",
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5))
summary(logReg1)
logReg1
summary(sea.train$applicant_income_000s)
logReg2<- train(Denied~ Black+applicant_income_000s, data = sea.train,
               method = "glm",
               trControl = trainControl(method = "repeatedcv",
                                        repeats = 5))
summary(logReg2)
logReg2
logReg3<- train(Denied~ trans + loan_amount_000s + Black + Asian + AIAN +
                 NHPI, data = sea.train, method = "glm",
               trControl = trainControl(method = "repeatedcv", repeats = 5))
logReg3
preds<- predict(logReg3, sea.test)
confusionMatrix(preds,sea.test$Denied)
is.factor(sea.test$Denied)
is.factor(sea.test$Denied)
names(sea.train)
is.factor(sea.train$Black)
is.factor(Sea$Black)
sea.train$Denied<-as.factor(sea.train$Denied)

summary(logReg3)

logReg4<- train(Denied~ trans + loan_amount_000s + Black, data = sea.train, method = "glm", trControl = trainControl(method = "repeatedcv", repeats = 5))
logReg4
preds2<- predict(logReg4,sea.test)
confusionMatrix(preds2,sea.test$Denied)
summary(logReg4)
