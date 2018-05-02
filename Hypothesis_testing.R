#Hypothesis Testing

library(openxlsx)
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
dir=getwd()
dir
setwd(dir)
Subject <- rep(unlist(data2$ID),each=5)
data=read.csv("Total_Data_1.csv")
View(data)
data2=read_excel("MicrosurgeryPerformance.xlsx")
View(data2)
data2=data2[1:15,]
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
  geom_bar(stat="identity",fill="#56B4E9")+
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
