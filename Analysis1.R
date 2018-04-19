library(readxl)
dir=getwd()
setwd(dir)
data=read_excel("MicrosurgeryPerformance.xlsx")
View(data)
attach(data)
Session <- rep(c(1:5), each = 2)
Session = rep(Session, 15)
data=data[1:15,]

Score1=Score2= c()
Task= rep(c("Cutting", "Suturing"),75)
for( i in 1:15){
  Score1=append(Score1,c(as.numeric(data[i,8]),as.numeric(data[i,9]),as.numeric(data[i,15]),as.numeric(data[i,16]),
                         as.numeric(data[i,22]),as.numeric(data[i,23]),as.numeric(data[i,29]),as.numeric(data[i,30]),
                         as.numeric(data[i,36]),as.numeric(data[i,37]) ) )
  Score2=append(Score2,c(as.numeric(data[i,10]),as.numeric(data[i,11]),as.numeric(data[i,17]),as.numeric(data[i,18]),
                         as.numeric(data[i,24]),as.numeric(data[i,25]),as.numeric(data[i,31]),as.numeric(data[i,32]),
                         as.numeric(data[i,38]),as.numeric(data[i,38])))}
  

Subject = rep(data$ID, each=10)                        
Age = rep(data$Age,each=10)
Sex =rep(data$Sex,each=10)
Year = rep(data$`MS-Year`,each=10)  
new_df=data.frame(Subject, Age, Year,Sex,Session,Task,Score1,Score2)

#Creating Cutting DataSet
Cutting=new_df[seq(1,149,2),]
rownames(Cutting) <- 1:nrow(Cutting)
Cutting$Task=NULL


Scorer= c(rep("Scorer1",75),rep("Scorer2",75))
Cutting_Scores= c(Cutting$Score1, Cutting$Score2)
Cutting$Score1=Cutting$Score2=NULL
Cutting = do.call("rbind",replicate(2,Cutting,simplify = FALSE))
Cutting$Scorer=Scorer
Cutting$Scores=Cutting_Scores


#Creating Suturing Data Set
Suturing=new_df[seq(2,150,2),]
rownames(Suturing) <- 1:nrow(Suturing)
Suturing$Task=NULL

Suturing_Scores= c(Suturing$Score1, Suturing$Score2)
Suturing$Score1=Suturing$Score2=NULL
Suturing = do.call("rbind",replicate(2,Suturing,simplify = FALSE))
Suturing$Scorer=Scorer
Suturing$Scores=Suturing_Scores

#Analysis of Scores_Cutting
png(paste("Cutting_Scores.png"))
ggplot(Cutting, aes(x=Scorer, y=Cutting$Scores)) + 
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scorer")
dev.off()


#Wilcox Test
wilcox.test(Cutting[76:150,7],Cutting[1:75,7], paired=TRUE, alternative ="greater")

#Analysis of Scores_Suturing

png(paste("Suturing_Scores.png"))
ggplot(Suturing, aes(x=Scorer, y=Suturing$Scores)) + 
  geom_boxplot()+ggtitle("Analysis of Scores based on Scorer") +labs(x="Scorer Number",y="Number of Scorer")
dev.off()


#Wilcox Test
wilcox.test(Suturing[76:150,7],Suturing[1:75,7], paired=TRUE, alternative ="greater")




