#Load raw data
train<-read.csv('train.csv',header=TRUE)
test<-read.csv('test.csv',header=TRUE)
#ADD Survived in the test dataset RPEATE NONE FOR NROWS IN TEST AND SEPERATE IT WITH , OR CONCATENATE IT
test.survived<-data.frame(Survived=rep("None",nrow(test)),test[,])
#combine the two data sets rows(RBIND)now 
data.combined <- rbind(train,test.survived)
str(data.combined)
data.combined$Survived=as.factor(data.combined$Survived)
data.combined$Pclass=as.factor(data.combined$Pclass)
table(data.combined$Survived)
table(data.combined$Pclass)
table(data.combined$Sex)
library(ggplot2)
train$Pclass=as.factor(train$Pclass)
ggplot(train,aes(x=Pclass,fill=factor(Survived)))+
         geom_bar(width=0.5)+
        xlab("pclass")+
        ylab("count")+
        labs(fill="Survived")
#getting the first five records of name
head(as.character(train$Name))
#get the length of unique elements on the basis of name converting into character
length(unique(as.character(data.combined$Name)))
dup.names<-as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])
data.combined[which(data.combined$Name %in% dup.names),]
#they are different ppl
library("stringr")
misses<-data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]
miss<-data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
miss[1:5,]
males<-male<-data.combined[which("male"== data.combined$Sex),]
males[1:5,]

#function
extractTitle<-function(name){
  name<-as.character(name)
  if(length(grep("Miss.",name))>0){
    return("Miss.")
  }
  if(length(grep("Mrs.",name))>0){
    return("Mrs.")
  }
  if(length(grep("Mr.",name))>0){
    return("Mr.")
  }
  if(length(grep("Master.",name))>0){
    return("Master.")
  }
  else{
    return("other")
  }
}

titles<-NULL
for(i in 1:nrow(data.combined)){
  titles=c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$Title<-as.factor(titles)

#PCLASS TITLE AND SURVIVAL 3D RELATIONSHIP
ggplot(data.combined[1:891,],aes(x=titles[1:891],fill=Survived[1:891]))+
  geom_bar(width=0.4)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("count")+
  labs("title wise survivals and deaths")

#Sex Pclass and survival 3d representation of relationship
ggplot(data.combined[1:891,],aes(x=Sex[1:891],fill=Survived[1:891]))+
  geom_bar(width=0.4)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("count")+
  labs("Gender wise survivals and deaths")

#4d relationship between age sex pclass and survival rates
summary(data.combined$Age)
ggplot(data.combined[1:891,],aes(x=Age[1:891],fill=Survived[1:891]))+
  geom_bar(width=5)+
  facet_wrap(~Sex+Pclass)+
  ggtitle("Pclass age")+
  xlab("Age")+
  ylab("count")+
  labs("Age wise survivals and deaths")

boys<-data.combined[which(data.combined$Title=="Master."),]
summary(boys$Age)


gals<-data.combined[which(data.combined$Title=="Miss."),]
summary(gals$Age)

ggplot(gals[gals$Survived!="None",],aes(x=Age,fill=Survived))+
  geom_bar(width=5)+
  facet_wrap(~Pclass)+
  ggtitle("AGE FOR GALS BY Pclass")+
  xlab("age")+
  ylab("count")

gals.alone<-gals[which(gals$SibSp==0 & gals$Parch==0),]
length(which(gals.alone$Age<=14.5))

length(unique(data.combined$SibSp))
data.combined$SibSp=as.factor(data.combined$SibSp)
#sibsp,survivals,title,pclass
ggplot(data.combined[1:891,],aes(x=SibSp[1:891],fill=Survived[1:891]))+
  geom_bar(width=5)+
  facet_wrap(~Title+Pclass)+
  ggtitle("Pclass age")+
  xlab("sibsp")+
  ylab("count")+
  labs("title wise survivals and deaths")
ggplot(data.combined[1:891,],aes(x=Parch[1:891],fill=Survived[1:891]))+
    geom_bar(width=5)+
    facet_wrap(~Title+Pclass)+
    ggtitle("Pclass age")+
    xlab("parch")+
    ylab("count")+
    labs("title wise survivals and deaths")
  
temp.Sibsp<-c(train$SibSp,test$SibSp)
temp.parch<-c(train$Parch,test$Parch)
data.combined$Familysize<-as.factor(temp.Sibsp+temp.parch+1)

ggplot(data.combined[1:891,],aes(x=Familysize,fill=Survived[1:891]))+
  geom_bar(width=5)+
  facet_wrap(~Title+Pclass)+
  ggtitle("Pclass title")+
  xlab("family size")+
  ylab("count")+
  labs("family size wise survivals and deaths")