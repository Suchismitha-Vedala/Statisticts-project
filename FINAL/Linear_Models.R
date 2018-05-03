library(openxlsx)
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(nlme)
dir=getwd()
dir
data=read.csv("Data/Normalised_Data.csv")
data$Sex[data$Sex==1]="Male"
data$Sex[data$Sex==2]="Female"


#Normalised

# # m1=abs(min(na.exclude(data$Mean_Perspiration)))
# col=data$Mean_Perspiration
# col=as.numeric(col)+m1+0.0015
# 
# list1=data$Mean_Perspiration
# #list1=na.omit(df$Mean_Perspiration)
# list1[is.na(list1)] <- 555
# minimumvalue=abs(min(list1))
# 
# for(i in c(1:length(list1))) {
#   list1[i]=list1[i]+minimumvalue+0.0015
#   #appendedlist=append(appendedlist,newlist)
#   print(list1[i])
# }
# 
# for(i in c(1:length(list1))) {
#   if (list1[i] > 500) {
#     list1[i]=NA
#     print(list1[i])
#   }
# }
# print(list1)
# data["Normalised_PP"]<-NA
# data$Normalised_PP=list1
# write.csv(data,"Data/Normalised_Data.csv")


#Performance WRT Sutures
data2=read_excel("Data/MicrosurgeryPerformance.xlsx")
data2=data2[1:15,]
Subject <- rep(unlist(data2$ID),each=5)
session=rep(c(1:5),15)#rep(c("Session1","Session2","Session3","Session4","Session5"),15)


Sutures=c()
for( i in 1:15){
  Sutures=append(Sutures,c( as.numeric(data2[i,"Sutures 1"]),as.numeric(data2[i,"Sutures 2"]) ,as.numeric(data2[i,"Sutures 3"]) ,as.numeric(data2[i,"Sutures 4"]) ,as.numeric(data2[i,"Sutures 5"])         ))
}
Age=rep(unlist(data2$Age),each=5)
Sex=rep(unlist(data2$Sex),each=5)
sdata=data.frame(Subject,Age, Sex,session,Sutures)
attach(sdata)
interaction.plot(x.factor     = session,
                 trace.factor = Sex, 
                 response     = Sutures, 
                 col=c("red","green"),  ### Colors for levels of trace var.
                 
                 ### Order by factor order in data
)
for (i in 1:length(Sex)){
  if(as.numeric(Sex[i])==1){
    Sex[i]="Male"
  }
  else if(as.numeric(Sex[i])==2) {
    Sex[i]="Female"
  }
}
p1=ggplot(sdata,aes(session, Sutures, colour=Sex)) + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Effect of Sex on Number of Sutures")
ggsave(filename="Plot/SuturingVsSex.pdf", plot=p1)

session=rep(c("Session1","Session2","Session3","Session4","Session5"),15)
summary(lm(Sutures~Sex*session))

p2=ggplot(sdata,aes(session, Sutures)) + theme(plot.title = element_text(hjust = 0.5)) + 
  #geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+
  geom_bar(stat="identity",fill="#56B4E9")+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Performance WRT number of sutures made summing  values of all subjects")
ggsave(filename="Plot/PerformanceWRTSutures_bar.pdf", plot=p2)


p3=ggplot(sdata,aes(session, Sutures)) +
  geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+ theme(plot.title = element_text(hjust = 0.5)) + 
  #geom_bar(stat="identity",fill="#56B4E9")+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Performance WRT number of sutures made")
ggsave(filename="Plot/PerformanceWRTSutures.pdf", plot=p3)

summary(lm(Sutures~session))






#Summary_Based on Scores with all other attributes
summary(lm(formula = Scores~log(Normalised_PP)+Age+Sex+Task+Scorer+Session,data=data))

#Random Effect
rand_data=na.omit(data)
anova(lme(Scores~Normalised_PP+Age+Year+Sex+Task+Session,random=~1|Subject,data=rand_data))

p4 = ggplot(data, aes(x=Scorer, y=Scores))  + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scores")
ggsave(filename="Plot/ScorerVsScore.pdf", plot=p4)


p5 = ggplot(data, aes(x=Sex, y=Scores))  + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Gender") +labs(x="Gender",y="Number of Scores")
ggsave(filename="Plot/GenderVsScore.pdf", plot=p5)


p6 = ggplot(data, aes(x=Session, y=Scores))  + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Session Number",y="Number of Scores")
ggsave(filename="Plot/SessionVsScore.pdf", plot=p6)


p7 = ggplot(data, aes(x=Task, y=Scores))  + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Task") +labs(x="Task",y="Number of Scores")
ggsave(filename="Plot/TaskVsScore.pdf", plot=p7)

#Summary_Based on Time with all other attributes
summary(lm(formula = Time~log(Normalised_PP)+Age+Sex+Task+Session,data=data))

p8 = ggplot(data, aes(x=Scorer, y=Time))  + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Scorer") +labs(x="Scorer Number",y="Time in seconds")
ggsave(filename="Plot/ScorerVsTime.pdf", plot=p8)


p9 = ggplot(data, aes(x=Sex, y=Time)) + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Gender") +labs(x="Gender",y="Time in seconds")
ggsave(filename="Plot/GenderVsTime.pdf", plot=p9)


p10 = ggplot(data, aes(x=Session, y=Time))  + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Scorer") +labs(x="Session Number",y="Time in seconds")
ggsave(filename="Plot/SessionVsTime.pdf", plot=p10)


p11 = ggplot(data, aes(x=Task,y=Time))  + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Time based on Task") +labs(x="Task",y="Time in seconds")
ggsave(filename="Plot/TaskVsTime.pdf", plot=p11)


#Analysis based on Scorer
Cutting=read.csv("Data/Cutting_Data.csv")
p12=ggplot(Cutting, aes(x=Scorer, y=Scores)) + theme(plot.title = element_text(hjust = 0.5)) +  
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scorer")
ggsave(filename="Plot/Cutting_ScorerVsScore.pdf", plot=p12)
#Wilcox Test
CScorer1=as.numeric(Cutting[1:75,7])
CScorer2=as.numeric(Cutting[76:150,7])
wilcox.test(CScorer2,CScorer1, paired=TRUE, alternative ="two.sided")

Suturing=read.csv("Data/Suturing_Data.csv")
p13=ggplot(Suturing, aes(x=Scorer, y=Scores)) + 
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scorer")
ggsave(filename="Plot/Suturing_ScorerVsScore.pdf", plot=p13)
SScorer1=as.numeric(Suturing[1:75,7])
SScorer2=as.numeric(Suturing[76:150,7])
#Wilcox Test
wilcox.test(SScorer2,SScorer1, paired=TRUE, alternative ="two.sided")


#Wilcox Test:
wilcox.test(Cutting$Scores,Suturing$Scores,paired=TRUE, alternative ="two.sided")









