library(scales) 
library(ggplot2) 

#Read Data
data<- read.csv("Athletes001.csv")

#variable names
names(data)

#Summary
summary(data)

#Transform into categorical variables
data$Sport<-factor(data$Sport)
data$Nation<-factor(data$Nation)
data$Gender<-factor(data$Gender)
data$Birth.Date<-factor(data$Birth.Date)
data$Place.of.Birth<-factor(data$Place.of.Birth)

######Calculate the standard deviation of total pay
sd(data$Total.Pay)

#Calculate the skewness of total pay
total_skew <- (3*(mean(data$Total.Pay) - median(data$Total.Pay)))/sd(data$Total.Pay)
total_skew

#Producing a density plot 
ggplot(data, aes(x=Total.Pay)) + geom_density() + 
  scale_x_continuous(labels=dollar)     



#######Calculate the standard deviation of salary winnings
sd(data$Salary.Winnings)

#Calculate the skewness of salary winnings
salary_winnings_skew <- (3*(mean(data$Salary.Winnings) - median(data$Salary.Winnings)))/sd(data$Salary.Winnings)
salary_winnings_skew

#Producing a density plot 
ggplot(data, aes(x=Salary.Winnings)) + geom_density() + 
  scale_x_continuous(labels=dollar) 



#######Calculate the standard deviation of Endorsements
sd(data$Endorsements)

#Calculate the skewness of endorsements
endorsements_skew <- (3*(mean(data$Endorsements) - median(data$Endorsements)))/sd(data$Endorsements)
endorsements_skew

#Producing a density plot 
ggplot(data, aes(x=Endorsements)) + geom_density() + 
  scale_x_continuous(labels=dollar) 


####Visualizations

#Exploring relationships between Sports and Gender
#Stacked bar chart
ggplot(data, aes(x=Sport, fill=Gender)) + 
  geom_bar() +
  ggtitle("Top 100 Most Paid Athletes Gender Participation in Sports")+
  labs(fill="Gender") +
  xlab("Sport") +
  ylab("Count")
  
#Exploring relationships between Sports and Salary
#Exploring distributions by category with a box plot
ggplot(data, aes(x=Salary.Winnings, y=Sport, fill=Sport)) +
  geom_boxplot() +
  ggtitle("Top Paying Sports for the 100 Most Paid Athletes")+
  scale_color_brewer(palette="Set1") +
  labs(fill="Sport") +
  xlab("Salary") +
  ylab("Sport")


#Exploring Gap in Pay between Male and Female Athletes
ggplot(data, aes(x=Salary.Winnings, color=Gender, fill=Gender)) +  
  geom_density() + 
  ggtitle("Salary Distribution by Gender")+
  scale_color_brewer(palette="Set1")
















