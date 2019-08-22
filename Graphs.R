Sea2<-read.csv("Seattle.csv", header = T)
View(Sea2)
names(Sea2)
library(ggplot2)
cleanup<-theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(color = "black"))
approved<-ggplot(Sea2,aes(action_taken_name))
approved+
  geom_bar()

approved<-ggplot(Sea2,aes(action_taken_name))
approved+
  geom_bar()+
  cleanup+
  labs(y= "Action Taken Count",
       title = "Loan Approval Rate")

approved<-ggplot(Sea2,aes(action_taken_name))
approved+
  geom_bar()+
  cleanup+
  labs(y= "Action Taken Count",
       title = "Loan Approval Rate")
approved<-ggplot(Sea2,aes(applicant_race_name_1, fill=action_taken_name))
approved+
  geom_bar()+
  coord_flip()+
  cleanup+
  labs(y= "Action Taken Count",
       x= "Applicant's Race",
       title = "Loan Approval Rates ")
approved<-ggplot(Sea2,aes(applicant_race_name_1, fill=action_taken_name))
approved+
  geom_bar()+
  facet_wrap(~applicant_sex_name)+
  cleanup+
  labs(y= "Action Taken Count",
       x= "Applicant's Race",
       title = "Action Taken by Race and Gender ")

approved<-ggplot(Sea2,aes(applicant_income_000s, fill= action_taken_name))
approved+
  geom_density(alpha =.5)+
  facet_wrap(applicant_sex_name~ applicant_race_name_1)+
  cleanup




