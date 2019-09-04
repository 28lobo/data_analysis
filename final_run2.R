rm(list = ls())
tac<-read.csv("Tacoma.csv",header = T)
library(MASS)
library(caret)
library(car)
library(ggplot2)
library(dplyr)
library(past)
library(psych)
library(QuantPsyc)

names(tac)
tac<-tac[,-48]
describe(tac$applicant_income_000s, na.rm = T)
qplot(sample = tac$applicant_income_000s, stat ="qq", na.rm =T)
describe(tac$loan_amount_000s, na.rm = T)
qplot(sample = tac$loan_amount_000s, stat ="qq")
describe(tac$population, na.rm= T)
qplot(sample = tac$population, na.rm = T)
describe(tac$minority_population, na.rm = T)
describe(tac$tract_to_msamd_income, na.rm = T)


#missing values
summary(tac)
tac$missing_inc<-ifelse(is.na(tac$applicant_income_000s),
                      "Y","N")
names(tac)
features_1<- c("tract_to_msamd_income","population","minority_population",
               "loan_amount_000s","applicant_income_000s","applicant_race_name_1",
               "applicant_sex_name", "action_taken_name")
tac<-tac[,features_1]
# imputation of missing values
names(tac)
dummy_vars<- dummyVars(~., data = tac[,-8])
tac_dummy<- predict(dummy_vars,tac[,-8])
View(tac_dummy)
names(tac_dummy)
#Imputed data
pre_process<- preProcess(tac_dummy, method = "medianImpute")
imputed_data<-predict(pre_process,tac_dummy)
View(imputed_data)

tac$applicant_income_000s<- imputed_data[,5]
summary(tac$applicant_income_000s)
# transformations
tac<- mutate(tac, log_income = log(applicant_income_000s) )
tac<- mutate(tac, log_LA = log(loan_amount_000s) )
summary(tac$log_LA)
summary(tac$log_income)
describe(tac$log_income)
hist(tac$log_income)
describe(tac$log_LA)
hist(tac$log_LA)

#feature engineering

tac$BA[tac$applicant_race_name_1=="Black or African American"]="1"
tac$BA[tac$applicant_race_name_1=="American Indian or Alaska Native"]="0"
tac$BA[tac$applicant_race_name_1=="White"]="0"
tac$BA[tac$applicant_race_name_1=="Asian"]= "0"
tac$BA[tac$applicant_race_name_1 == "Native Hawaiian or Other Pacific Islander"]= "0"
tac$Loan_Status[tac$action_taken_name=="Application denied by financial institution"]="0"
tac$Loan_Status[tac$action_taken_name=="Loan originated"]="1"
View(tac)
tac<-mutate(tac, Female = ifelse(applicant_sex_name =="Female",1,0))
tac<-mutate(tac, AIAN = ifelse(applicant_race_name_1=="American Indian or Alaska Native",1,0))
tac<-mutate(tac, Asian = ifelse(applicant_race_name_1=="Asian",1,0))
tac<-mutate(tac, NHPI = ifelse(applicant_race_name_1=="Native Hawaiian or Other Pacific Islander",1,0))

tac$Loan_Status<- as.factor(tac$Loan_Status)
tac$BA<-as.factor(tac$BA)
tac$Female<-as.factor(tac$Female)
tac$AIAN<-as.factor(tac$AIAN)
tac$Asian<-as.factor(tac$Asian)
tac$NHPI<-as.factor(tac$NHPI)
str(tac)
tac[tac$Loan_Status== 0, "Loan_Status"]
table(tac$action_taken_name,tac$applicant_race_name_1)
table(tac$Female)
names(tac)
tac<- tac[,c(1,2,3,9,10,11,12,13,14,15,16)]
View(tac)

#Data partitioning
set.seed(327)
index <- createDataPartition(tac$Loan_Status, p = 0.7, list = FALSE)
train<-tac[index,]
test<-tac[-index,]
train_control<- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 10, 
                             verboseIter = FALSE,
                             sampling = "smote")
?trainControl
set.seed(327)
model<- train(Loan_Status~.-population-tract_to_msamd_income-minority_population, data = train,method = "glm",trControl =train_control)
View(train)
model
summary(model)
PredTrain<- predict(model, newdata=train)
confusionMatrix(predict(model,test),test$Loan_Status)
model1<-glm(Loan_Status~ log_income+log_LA+BA+Female+AIAN+Asian,data = train,
            family ="binomial")
summary(model1)
PredTrain1<- predict(model, newdata=train)
confusionMatrix(predict(model,test),test$Loan_Status)
View(train)











