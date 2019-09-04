t<- read.csv("Tacoma.csv", header = T)
names(t)
library(ggplot2)
library(gmodels)
cleanup<-theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(color = "black"))
appr_den<- ggplot(t, aes(action_taken_name,fill = action_taken_name))
appr_den+
  geom_bar(color = "black")+
  scale_fill_manual(values = c("#99d8c9","#2ca25f"))+
  scale_x_discrete(labels = c("Denied","Approved"))+
  ylab("Count")+
  xlab("Action Taken by Fiancial Institutions")+
  labs(fill="Action Taken by Financial Institutions")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup

by_gender<- ggplot(t, aes(applicant_sex_name,fill =action_taken_name))
by_gender+
  geom_bar(color = "black")+
  scale_fill_manual(values = c("#99d8c9","#2ca25f"))+
  scale_x_discrete(labels = c("Female","Male"))+
  ylab("Count")+
  xlab("Action Taken by Fiancial Institutions")+
  labs(fill="Action Taken")+
  ggtitle("Loan Approval and Denial Rates by Gender")+
  cleanup

#Cross-tab
?CrossTable()
CrossTable(t$action_taken_name,t$applicant_sex_name,
           prop.t = TRUE, fisher = TRUE)

by_race<- ggplot(t, aes(applicant_race_name_1,fill =action_taken_name))
by_race+
  geom_bar(color = "black")+
  scale_fill_manual(values = c("#99d8c9","#2ca25f"))+
  scale_x_discrete(labels = c("AIAN","A","BA","NHPI","W"))+
  ylab("Count")+
  xlab("Action Taken by Fiancial Institutions")+
  labs(fill="Action Taken")+
  ggtitle("Loan Approval and Denial Rates by By Race and Income Levels")+
  cleanup
quantile(t$applicant_income_000s, probs = c(.25,.50,.75))
b<-c(-Inf,60,82,Inf)
names<-c("Low","Medium","High")
t$inc.cat<-cut(t$applicant_income_000s,breaks = b,
             labels = names)
View(inc.cat)
summary(inc.cat)
t<-inc.cat
names(inc.cat)<- c("inc_cat")
is.data.frame(inc.cat)
as.data.frame(inc.cat)
colnames(inc.cat)[colnames(inc.cat)=="V1"] <- "inc_cat)"
inc_cat<- inc.cat
str(inc.cat)
str(t)

inc<-ggplot(t)
inc+
  geom_bar(aes(inc.cat, fill = action_taken_name))+
  facet_wrap(applicant_race_name_1 ~.,scales = "free")+
  cleanup
?ftable

library(caret)
names(t)
features<- c("action_taken_name","applicant_income_000s","applicant_race_name_1",
             "applicant_sex_name")
t<-t[,features]
names(t)
dummy_vars<- dummyVars(~., data = t[,-1])
t_dummy<- predict(dummy_vars,t[,-1])
View(t_dummy)
names(t_dummy)

pre_process<- preProcess(t_dummy, method = "medianImpute")
imputed_data<-predict(pre_process,t_dummy)
View(imputed_data)

t$applicant_income_000s<- imputed_data[,1]
summary(t$applicant_income_000s)

inc<-ggplot(t)
inc+
  geom_bar(aes(inc.cat, fill = action_taken_name),color = "black")+
  facet_wrap(applicant_race_name_1 ~.,scales = "free")+
  scale_fill_manual(values = c("#99d8c9","#2ca25f"))+
  ylab("Count")+
  xlab("Action Taken by Fiancial Institutions")+
  labs(fill="Action Taken")+
  ggtitle("Loan Approval and Denial Rates by By Race and Income Levels")+
  cleanup
# 3 way table

mytable<- prop.table(t$action_taken_name,t$applicant_race_name_1,t$inc.cat)
prop.table(ftable(mytable))
chisq.test(mytable)
mantelhaen.test(mytable,t$inc.cat)
?mantelhaen.test
# marginal frequencies
margin.table(mytable,1)
margin.table(mytable,2)
ftable(round(prop.table(mytable),2)) # cell %
ftable(round(prop.table(mytable,1),2)) # row %
ftable(round(prop.table(mytable,2),2)) # column %
nrow(t)
