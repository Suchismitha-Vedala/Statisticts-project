df1=read.csv('/Users/suchismithavedala/Desktop/Spring 2018/Stat/Project/data/MicrosurgeryPerformance.csv')
df1[1,]
df1[,5]
for (i in c(5,6,9,10,13,14,17,18,21,22)){
  df1[,i]=as.numeric(unlist(strsplit(as.character(df1[,i]),":",""))[1])*60+as.numeric(unlist(strsplit(as.character(df1[,i]),":",""))[2])
}
subject_num=df1[,1]
for(i in c(1,2,3,4,5,6,7,8,9,10,11,12,13)){
  timing=c(df1[i,5],df1[i,6],df1[i,9],df1[i,10],df1[i,13],df1[i,14],df1[i,17],df1[1,18],df1[i,21],df1[i,22])
  accuracy=c(df1[i,8],df1[i,12],df1[i,16],df1[i,20],df1[i,24])
  session=c("Session1","Session2","Session3","Session4","Session5")
  library(ggplot2)
  library(plotly)
  title<-paste("Subject",subject_num[i],"_Performance.pdf",sep="")
  title1<-paste("Subject",subject_num[i],"_Accuracy.pdf",sep="")
  Activity<-rep(c("Cutting","Suturing"),5)
  Session1<-c(1,1,2,2,3,3,4,4,5,5)
  df=data.frame(Activity,Session1,timing)
  df2=data.frame(accuracy,session)
  p<-ggplot(df, aes(x=Session, y=timing, fill=Activity)) + geom_bar(stat='identity', position='dodge')+ggtitle(title) +labs(x="Session Number",y="Timing")
  p2 <-ggplot(df2, aes(session, accuracy))+geom_bar(stat = "identity",position='dodge')+ggtitle("accurary test") +labs(x="Session Number",y="Accuracy")
  ggsave(filename=paste("/Users/suchismithavedala/Desktop/Spring 2018/Stat/Project/Coding/",title,sep="") ,plot=p)
  ggsave(filename=paste("/Users/suchismithavedala/Desktop/Spring 2018/Stat/Project/Coding/",title1,sep="") ,plot=p2)
  
}

