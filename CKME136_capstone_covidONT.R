setwd("C:/Users/Colbert/OneDrive/Documents/CKME_136")
data <- read.csv(file = "conposcovidloc.csv", header = TRUE, sep = ",")
install.packages("ggplot2")
library(ggplot2)
dim(data)
summary(data)


str(data)

clean<-data[!grepl("Not Resolved", data$Outcome1),]
clean<-clean[!grepl("Unknown", clean$Age_Group),]
clean<-clean[!grepl("UNKNOWN", clean$Client_Gender),]
clean<-clean[!grepl("OTHER", clean$Client_Gender),]
clean<-clean[!grepl("TRANSGENDER", clean$Client_Gender),]

names(clean)[2] <- "Date"
names(clean)[3] <- "Age"
names(clean)[4] <- "Gender"
names(clean)[5] <- "Info"
names(clean)[6] <- "Outcome"
names(clean)[8] <- "PHU"

ggplot(clean, aes(y=PHU)) + 
  geom_bar(aes(fill = Gender))

ggplot(clean, aes(x=Age)) + 
  geom_bar(aes(fill = Gender))

ggplot(clean, aes(y=Date)) + 
  geom_bar(aes(fill = Gender))

ggplot(clean, aes(y=Date)) + 
  geom_bar(aes(fill = Outcome))

ggplot(clean, aes(y=PHU)) + 
  geom_bar(aes(fill = Outcome))

ggplot(clean, aes(x=Gender)) + 
  geom_bar(aes(fill = Outcome))

ggplot(clean, aes(x=Age)) + 
  geom_bar(aes(fill = Outcome))

ggplot(clean, aes(y=Date)) + 
  geom_bar(aes(fill = PHU))

ggplot(clean, aes(y=PHU)) + 
  geom_bar(aes(fill = Age))

ggplot(clean, aes(y=Date)) + 
  geom_bar(aes(fill = Age,PHU))

clean$Date <- as.Date(clean$Date)
clean$PHU <- as.numeric(clean$PHU)
clean$Age <- as.numeric(clean$Age)
clean$Gender <- as.numeric(clean$Gender)
clean$Outcome <- as.numeric(clean$Outcome)

lmOutcome = lm(Outcome~Date+Age+Gender+PHU, clean)
summary(lmOutcome)
