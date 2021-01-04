library(ggplot2)
library(ggthemes)
library(mice)
library(randomForest)
library(scales)
library(dplyr)

train<-read.csv('C:/Users/Acer/OneDrive/Desktop/datascience sreya/train.csv',stringsAsFactors=F)
test<-read.csv('C:/Users/Acer/OneDrive/Desktop/datascience sreya/test.csv',stringsAsFactors=F)
test$Survived=NA
titanic<-rbind(train,test)

glimpse(titanic)
titanic$Title<-gsub("(.*,)(\\..*)","",titanic$Name)
table(titanic$Sex,titanic$Title)
titanic$Surnames<- sapply(titanic$Name,function(x)strsplit(x,split="[.,]"))

count<-nlevels(factor(titanic$Surnames))
count
titanic$Fsize <-titanic$SibSp +titanic$Parch +1


ggplot(data=titanic[1:891,],aes(x=Fsize,fill=factor(Survived)))+
geom_bar(stat="count",position="dodge")+
  scale_x_continuous(breaks=c(1:11))+
  ylim(c(0,400))+
  labs(x="Family Size")+
  theme_bw()

  miss<-function(x){
  sum=0
  for (i in 1:ncol(x))
    {
    cat("In column",colnames(x[i]),"total NA values are:",colSums(is.na(x[i])),"\n")
  }
  }
miss(titanic)

blank <-function(x){
  sum=0
  for(i in 1:ncol(x))
  {
    cat("In column",colnames(x[i]),"total blank values are:",colSums(x[i]==""),"\n")

  }
    
}
blank(titanic)

titanic$PassengerId[titanic$Embarked==""]

titanic$Pclass[titanic$PassengerId==62]
titanic$Fare[titanic$PassengerId==62]

titanic$Pclass[titanic$PassengerId==830]
titanic$Fare[titanic$PassengerId==830]

embarkFare <- titanic %>%
  filter(PassengerId!=62 & PassengerId!=830)


ggplot(data=embarkFare,aes(x=Embarked, y = Fare, fill = factor(Pclass)))+
  geom_boxplot()+ 
  geom_hline(aes(yintercept=80),
             colour="green",linetype="dashed",lwd=2)+
  scale_y_continuous(labels=dollar_format())+
  theme_bw()
titanic$Embarked[c(62,830)]<-"C"

 titanic$PassengerId[is.na(titanic$Fare)]
 titanic[1044,]
 ggplot(data=titanic[titanic$Pclass==3 & titanic$Embarked=="S",],aes(x=Fare))+
   geom_density(fill= "#99d6ff",alpha=0.4)+
   geom_vline(aes(xintercept=median(Fare,na.rm=T)),
              colour="blue",lintype="dashed",lwd=1)+
   scale_x_continuous(labels=dollar_format())+
   theme_bw()
 titanic$Fare[1044]<-median(titanic[titanic$Pclass==3 & titanic$Embarked=='S',]$Fare,na.rm=T)

 factorVars <- c("PassengerId" , "Pclass" , "Sex" , "Embarked" , "Title" , "Surnames" )
 titanic[factorVars]<-lapply (titanic[factorVars], function(x) as.factor(x))
 
 set.seed(129)
 
 miceMod <- mice(titanic[, !names(titanic) %in% c("PassengerId","Family","Survived","Surnames")],method="rf")
 miceOutput <- complete( miceMod )

 
 
 par(mfrow=c(1,2))
 hist(titanic$Age,freq=F,main="Age:Original Data",col="yellow",ylim=c(0,0.07))
 hist(miceOutput$Age,freq=F,main="Age:MiceOUTPUT",col="pink",ylim=c(0,0.07))
  titanic$Age<-miceOutput$Age
  sum(is.na(titanic$Age))
 
 ggplot(data=titanic[1:891,],aes(x=Fsize,fill=factor(Survived)))+
   geom_histogram()+
   facet_grid(.~Sex)
 theme_bw()
 
 titanic$Child[titanic$Age<18]<-"Child"
 titanic$Child[titanic$Age>=18]<-"Adult"  
 
 table(titanic$Child,titanic$Survived)
 
 
 titanic$Mother<-'Not mother'
 titanic$Mother[titanic$Sex=='female' &titanic$Age>18 & titanic$Parch>0 & titanic$Title !='Miss']<-"Mother"
 table(titanic$Mother,titanic$Survived)
 
 titanic$Child <- factor(titanic$Child)
 titanic$Mother<- factor(titanic$Mother)
 
 train <- titanic[1:891,]
 test<-titanic[892:1309,]
 set.seed(754)
 
 model<-randomForest(factor(Survived)~Pclass + Sex + Age + SibSp + Parch +Fare +Embarked + Title + Child + Mother,data = train)
 model
 
 plot(model)
 
 
 prediction<-predict(model,test)
 solution<- data.frame(PassengerID=test$PassengerId,Survived=prediction)
 write.csv(solution,file="solution.csv",row.names = F)
 getwd()
 
 table(solution$Survived)