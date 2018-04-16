library(readxl)
dir=getwd()
setwd(dir)
data=read_excel("MicrosurgeryPerformance.xlsx")

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
                    