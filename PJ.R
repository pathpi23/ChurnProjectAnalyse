library(tidyverse)
library(ggplot2)
library(lattice)
library(rpart)
library(rpart.plot)
library(caret)
data <- read.csv("telecom_churn.csv")

summary(data)
data %>% ggplot(aes(x = State)) + geom_bar() + coord_flip()
datacut <- data %>% filter(Churn=="Churn")
datacut %>% ggplot(aes(x = State,fill=..count..))+geom_bar()+scale_fill_gradient(low='#0EC2DA',high='#04096F') + coord_flip()
#--------------------------------1
data %>% ggplot(aes(x = Customer.service.calls,fill = Churn)) + geom_bar(position = "fill")+
scale_x_continuous("Customer.service.calls",breaks=c(0,1,2,3,4,5,6,7,8,9))
#-----------------------3
data %>% filter(Churn =="Not_churn",Account.length!= "0")%>% ggplot(aes(x = Account.length, fill=..count..)) +
  geom_histogram(bins = 11)+scale_fill_gradient(low='#0EC2DA',high='#04096F')

data %>% filter(Churn =="Churn",Account.length!= "0")%>% ggplot(aes(x = Account.length, fill=..count..)) + geom_histogram(bins = 11)+scale_fill_gradient(low='#0EC2DA',high='#04096F')
#--------------4
data1<-data%>%filter(Churn=='Not_churn')
data2<-data %>% filter(Churn=='Churn')
ggplot()+geom_histogram(data=data1,aes(x=Total.day.minutes),bins=50,fill="red")+
  geom_histogram(data=data2,aes(x=Total.day.minutes),bins=50,fill="blue")
ggplot()+geom_histogram(data=data1,aes(x=Total.eve.minutes),bins=50,fill="red")+
  geom_histogram(data=data2,aes(x=Total.eve.minutes),bins=50,fill="blue")
ggplot()+geom_histogram(data=data1,aes(x=Total.night.minutes),bins=50,fill="red")+
  geom_histogram(data=data2,aes(x=Total.night.minutes),bins=50,fill="blue")

ggplot()+geom_histogram(data=data2,aes(x=Total.day.minutes),bins=50,fill="red")+
  geom_histogram(data=data2,aes(x=Total.eve.minutes),bins=50,fill="blue")+
  geom_histogram(data=data2,aes(x=Total.night.minutes),bins=50,fill="black")
#-----------------5
data <- data %>% select(-Area.code)
set.seed(100)
test_data <- sample(nrow(data), 0.3*nrow(data)) 
telecom_training <- data[-test_data,] 
telecom_testing <- data[test_data,]
png("tree.png",width = 1200,height = 1000)
tree <- rpart(Churn ~ ., data = telecom_training)
rpart.plot(tree)
tree$cptable
plotcp(tree)
dev.off()

png("tree.png",width = 1200,height = 1000)
  tree <- rpart(Churn ~ ., data = telecom_training,control = rpart.control(cp= 0.029))
rpart.plot(tree)
dev.off()
res <- predict(tree,telecom_testing,type='class')

confusionMatrix(res,
                telecom_testing$Churn,
                positive = "Churn",
                mode = 'prec_recall')
tree$variable.importance
#-----------------------------6#
data %>% ggplot(aes(x = International.plan,fill=Churn)) +geom_bar(position = "dodge")
#-----------------------------7#
datacut %>% filter(Total.day.minutes != 0 )%>%  ggplot(aes(x=Total.day.minutes))+geom_density()
datacut %>% filter(Total.day.minutes != 0 )%>%  ggplot(aes(x=Total.day.charge))+geom_density()

data %>% filter(Churn =="Not_churn") %>% ggplot(aes(x=Total.day.minutes))+geom_histogram(bins=16)
#-----------------------------8
data %>% ggplot(aes(x=Voice.mail.plan,fill=Churn)) + geom_bar(position = "dodge")
data %>% ggplot(aes(x = State,fill = Churn)) +geom_bar(position = "dodge")
sss<- ggplot(data,aes(x= Account.length,fill = Churn)) +geom_density() +facet_grid(Churn ~ .)+labs(title = "Account Length")

ggplot() +geom_density(data=data,aes(x= Account.length,fill = Churn)) +labs(title = "Account Length")

sss
#-------------------------------9#
library(usmap)
data(statepop)
data <- read.csv("telecom_churn.csv")
mapFalse <-  data %>%filter(Churn=='Not churn')%>% group_by(State) %>% summarise (countFalse = n())
names(mapFalse)[1] <-"abbr"
statepop <- inner_join(statepop,mapFalse,by = "abbr")
plot_usmap(data = statepop, values = "countFalse", lines = "white",labels=TRUE,label_color="white")+
  scale_fill_continuous(low = "#0EC2DA", high = "#04096F",name = "CountChurn",label = scales::comma) + 
  theme(legend.position = "right")
#-------------------------------10#
test <- data
data %>% mutate(age_range = cut( data$Account.length, breaks = c(0,30,60,90,120,150,180,210,240,270), 
                                 labels = c("<20","20-29","30-39", "40-49","50-59",">60"))) -> HR1
#-----------------
dataNotchurn <- data %>% filter(Churn == "Not_churn")
dataChurn <- data %>% filter(Churn =="Churn")
ggplot() +geom_histogram(data = dataNotchurn,aes(x = Total.day.minutes),bins = 25,fill = "red")+
  geom_histogram(data = dataChurn,aes(x = Total.day.minutes),bins=25,fill="blue")


data %>% filter(Churn == "Not_churn")%>% ggplot() + geom_histogram(aes(x = Total.day.minutes),bins = 25)
data %>% filter(Churn=="Churn")%>% ggplot(aes(x = Total.day.minutes)) + geom_histogram(bins = 25)
data %>% filter(Churn == "Not_churn")%>% ggplot(aes(x = Total.eve.minutes)) + geom_histogram(bins = 25)
data %>% filter(Churn == "Not_churn")%>% ggplot(aes(x = Total.night.minutes)) + geom_histogram(bins = 25)


#--------------------11#
x <- data
x$Churn <- ifelse(x$Churn=="Churn",1,0)
ggplot(x, aes(x = Total.day.minutes, y = Churn)) + # draw a 
  geom_point() + # add points
  geom_smooth(method = "auto", # plot a regression...
              method.args = list(family = "binomial")) 
#---------------------12#

data %>% ggplot(aes(x=Churn,y=Total.day.minutes,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.day.calls,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.day.charge,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.eve.minutes,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.eve.calls,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.eve.charge,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.night.minutes,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.night.calls,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.night.charge,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.intl.minutes,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.intl.calls,fill = Churn))+geom_boxplot()
data %>% ggplot(aes(x=Churn,y=Total.intl.charge,fill = Churn))+geom_boxplot()

data %>% ggplot(aes(x=Customer.service.calls,fill = Churn))+geom_bar(position = "dodge")+
  scale_x_continuous("Customer.service.calls",breaks=c(0,1,2,3,4,5,6,7,8,9))
#---------------------13#
