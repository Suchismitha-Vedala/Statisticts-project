#Hypothesis Testing

library(openxlsx)
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(nlme)
dir=getwd()
dir
setwd(dir)
data=read.csv("Total_Data_1.csv")
View(data)
data2=read_excel("MicrosurgeryPerformance.xlsx")
data2=data2[1:15,]
Subject <- rep(unlist(data2$ID),each=5)
session=rep(c(1:5),15)
View(data2)

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
Sex
for (i in 1:length(Sex)){
  if(as.numeric(Sex[i])==1){
    Sex[i]="Male"
  }
  else if(as.numeric(Sex[i])==2) {
    Sex[i]="Female"
  }
}
png(paste("SuturingVsSex.png"))
ggplot(sdata,aes(session, Sutures, colour=Sex)) +
  geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Effect of Sex on Number of Sutures")
dev.off()

t.test(Sutures ~ Sex)
summary(lm(Sutures~Sex*session))

png(paste("PerformanceWRTSutures_bar.png"))
ggplot(sdata,aes(session, Sutures)) +
  #geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+
  geom_bar(stat="identity",fill="#56B4E9")
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Performance WRT number of sutures made summing  values of all subjects")
dev.off()
png(paste("PerformanceWRTSutures.png"))
ggplot(sdata,aes(session, Sutures)) +
  geom_jitter(alpha=0.2) +geom_smooth(method=lm)+xlim(1,5)+
  #geom_bar(stat="identity",fill="#56B4E9")+
  labs(x = 'Sessions', y = 'Number of Sutures')+ggtitle("Performance WRT number of sutures made")
dev.off()
summary(lm(Sutures~session))

t.test(Sutures,session)

m1=min(na.exclude(data$Mean_Perspiration))
m1=abs(m1)
col1=c()
col=data$Mean_Perspiration
col1=as.numeric(col)+m1+0.1
col2=log(col1)
data$Mean_Perspiration=col1
#Without Log
anova(lm(formula = data$Scores~data$Mean_Perspiration+data$Age+data$Year+data$Sex+data$Task+data$Session))
#With Log
data$Mean_Perspiration=col2
anova(lm(formula = data$Scores~data$Mean_Perspiration+data$Age+data$Year+data$Sex+data$Task+data$Session))



install.packages("lme4")
data=na.omit(data)
attach(data)
anova(lme(Scores~Mean_Perspiration+Age+Year+Sex+Task+Session,random=~1|Subject,data=data))



#shapiro.test

#Analysis of Scores_Cutting
Cutting=read.csv("Data/Cutting_Data.csv")
png(paste("Cutting_Scores.png"))
ggplot(Cutting, aes(x=Scorer, y=Scores)) + 
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scorer")
dev.off()


#Wilcox Test
wilcox.test(Cutting[76:150,7],Cutting[1:75,7], paired=TRUE, alternative ="two-sided")

#Analysis of Scores_Suturing

png(paste("Suturing_Scores.png"))
ggplot(Suturing, aes(x=Scorer, y=Suturing$Scores)) + 
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scorer")
dev.off()


#Wilcox Test
wilcox.test(Suturing[76:150,7],Suturing[1:75,7], paired=TRUE, alternative ="two-sided")
