rm(list = ls())
library(ggplot2)
t1<- read.csv("Tacoma.csv",header = T)
cleanup<-theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(color = "black"))
appr_den<- ggplot(t1, aes(action_taken_name,fill = action_taken_name))
appr_den+
  geom_bar()+
  ylab("Action Taken Count")+
  xlab("Action Taken")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup
table(t1$action_taken_name)
# the average denial rate in Tacoma is 15.67 percent.
prop.table(table(t1$action_taken_name))
?labs
table(t1$applicant_race_name_1)
prop.table(table(t1$applicant_race_name_1))*100
?stat

#by gender
appr_den<- ggplot(t1, aes(applicant_sex_name,fill = action_taken_name))
appr_den+
  geom_bar()+
  ylab("Action Taken Count")+
  xlab("Action Taken")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup
gender_tab <- table(t1$applicant_sex_name,t1$action_taken_name)
gender_tab

round(prop.table(gender_tab),2)*100 # cell %
round(prop.table(gender_tab,1),2) # row %
round(prop.table(gender_tab,2),2) # column %
#chisq
chisq.test(gender_tab)
ct3<- table(t1$applicant_sex_name,t1$applicant_race_name_1,t1$action_taken_name)
ct3
names(t1)
appr_den<- ggplot(t1, aes(applicant_race_name_1,fill = action_taken_name))
appr_den+
  geom_bar()+
  ylab("Action Taken Count")+
  xlab("Action Taken")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup+
  coord_flip()
race_table<- table(t1$applicant_race_name_1,t1$action_taken_name)
race_table
denial_reason<-t1[t1$denial_reason_name_1=="Collateral",]
dr<- ggplot(denial_reason, aes(action_taken_name))
dr+
  geom_bar()
appr_den<- ggplot(t1, aes(denial_reason_name_1,fill = applicant_race_name_1))
appr_den+
  geom_bar()+
  ylab("Action Taken Count")+
  xlab("Action Taken")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup+
  coord_flip()
#by agency
appr_den<- ggplot(t1, aes(agency_name,fill = action_taken_name))
appr_den+
  geom_bar()+
  ylab("Action Taken Count")+
  xlab("Action Taken")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup+
  coord_flip()
prop.table(table(t1$agency_name,t1$action_taken_name))*100
# by type of loan
appr_den<- ggplot(t1, aes(loan_type_name,fill = action_taken_name))
appr_den+
  geom_bar()+
  ylab("Action Taken Count")+
  xlab("Action Taken")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup+
  coord_flip()
prop.table(table(t1$loan_type_name,t1$action_taken_name))* 100

appr_den<- ggplot(t1, aes(applicant_race_name_1,fill = action_taken_name))
appr_den+
  geom_bar()+
  facet_wrap(~applicant_sex_name)+
  ylab("Action Taken Count")+
  xlab("Race")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup+
  coord_flip()
library(dplyr)

t1<- t1 %>% 
  mutate(hud_income_000s=(hud_median_family_income/1000))
t1<- t1 %>%
  mutate(I_HMI= (applicant_income_000s/hud_income_000s))

appr_den<- ggplot(t1, aes(I_HMI,applicant_income_000s))
appr_den+
  geom_bar(stat = "identity")+
  ylab("Action Taken Count")+
  xlab("Action Taken")+
  ggtitle("Loan Approval and Denial Rates")+
  cleanup+
  coord_flip()








