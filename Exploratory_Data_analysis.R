#libraries
library(ggplot2)
library(dplyr)
library(plyr)

#data
claims <- read.csv("Claims_Y1.csv")
members <- read.csv("Members_Y1.csv")
DIH <- read.csv("DayInHospital_Y2.csv")

#join members and DIH
DIH_members <- join(members, DIH, by = "MemberID")

#join DIH_members and claims
claims_data <- join(claims, DIH_members, by = "MemberID")

summary(claims_data)

#drop memberID, providerID, year, pcp, vendor
final_data <- select(claims_data, -c("MemberID","ProviderID","vendor","pcp","Year"))
write.csv(final_data, file = "finalData.csv")

#data cleaning
#make copy - check point
df <- final_data
str(df)

#changing 19-Oct to 10-19
df$AgeAtFirstClaim <- as.character(df$AgeAtFirstClaim)
df$AgeAtFirstClaim[df$AgeAtFirstClaim == "19-Oct"] <- "10-19"
df$AgeAtFirstClaim <- as.factor(df$AgeAtFirstClaim)
write.csv(df, file = "newfinalData.csv")
summary(df)

#univariate analysis
#specialty
ggplot(data = df, aes(specialty)) + geom_bar(fill = "steelblue")+
  ggtitle("Specialty")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#placesvc
ggplot(data = df, aes(placesvc)) + geom_bar(fill = "steelblue")+
  ggtitle("Place of Service")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#CharlsonIndex
ggplot(data = df, aes(CharlsonIndex)) + geom_bar(fill = "steelblue")+
  ggtitle("Charlson Comorbidity Index")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#CharlsonIndex + sex
ggplot(data = df, aes(CharlsonIndex)) + geom_bar(aes(fill = sex), position = "dodge")+
  ggtitle("CharlsonIndex risk in Gender")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#gender
ggplot(data = df, aes(sex)) + geom_bar(fill = "steelblue")+
  ggtitle("Gender distribution of Claims")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#AgeAtFirstClaim
ggplot(data = df, aes(AgeAtFirstClaim)) + geom_bar(fill = "steelblue")+
  ggtitle("Age at First claim")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#AgeAtFirstClaim + sex
ggplot(data = df, aes(AgeAtFirstClaim)) + geom_bar(aes(fill = sex), position = "dodge")+
  ggtitle("AgeAtFirstClaim wrt Gender")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#PrimaryConditionGroup
ggplot(data = df, aes(PrimaryConditionGroup)) + geom_bar(fill = "steelblue")+
  ggtitle("Primary Condition Group")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#PrimaryConditionGroup + sex
ggplot(data = df, aes(PrimaryConditionGroup)) + geom_bar(aes(fill = sex), position = "dodge")+
  ggtitle("Primary Condition Group wrt Gender")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))

#length of stay
ggplot(data = df, aes(LengthOfStay)) + geom_bar(fill = "steelblue")+
  ggtitle("Length of stay")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))
