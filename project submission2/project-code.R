library(openxlsx)
library(ggplot2)
library(reshape2)
library(dplyr)
options(scipen=999)
olddir = getwd()
curdir = "/home/yashwanth/Downloads/spring 18/stats/Methodist microsurgery with output/"
setwd(curdir)

# Code for Multiple Plots in Single page obtained from R Tutorials online 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Code to Save Graph
savePlot <- function(plot, name) {
  png(paste(name))
  print(plot)
  dev.off()
}

# Code to plot the graphs in Question 3
plotGraphs <- function(cutNASA, n){
  p <- ggplot(cutNASA, aes(Session,as.numeric(`Mental Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Mental Demand"))
  savePlot(p,paste(n,"Mental Demand.png") )
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Physical Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Physical Demand"))
  savePlot(p,paste(n,"Physical Demand.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Temporal Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Temporal Demand"))
  savePlot(p,paste(n,"Temporal Demand.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Performance`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Performance"))
  savePlot(p,paste(n,"Performance.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Effort`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Effort"))
  savePlot(p,paste(n,"Effort.png"))
  
  p <-ggplot(cutNASA, aes(Session,as.numeric(`Mental Demand`), fill = Session)) + geom_bar(stat = "identity") + xlab("Sessions") +
    ylab("Value") + theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(n,"Frustration"))
  savePlot(p,paste(n,"Frustration.png"))
}

# Question 1
Biographic.Data <- function(){
  # Biographic Data: Draw the barplot for gender distribution; histogram for age distribution
  
  #read the data file
  age.Gender.Data <- read.xlsx("MicrosurgeryPerformance.xlsx")
  #remove last row in the data frame
  age.Gender.Data <- age.Gender.Data[1:15,]
  # transform 
  age.Gender.Data <- transform(age.Gender.Data, Sextype= ifelse(Sex==1, "Male", "Female"))
  # plot gender distribution
  
  gender <- ggplot(age.Gender.Data, aes(Sextype, fill=Sextype)) + geom_bar() + xlab("Gender") + ylab("No of Participants") + scale_y_continuous(breaks=c(0,2,4,6,8,10))+
    theme(plot.title = element_text(hjust = 0.5))+ ggtitle("Barplot for gender distribution")
  # Save the plot 
  savePlot(gender,"1_gender.png")
  
  # plot age distribution
  age <- ggplot(age.Gender.Data, aes(Age)) + geom_histogram(bins = 5) + ylab("Frequency") + 
    theme(plot.title = element_text(hjust = 0.5))+ ggtitle("Histogram for age distribution")
  # save the plot 
  savePlot(age, "1_age.png")
}
Biographic.Data()

# Question 2
# Trait Psychometric Data: Draw the histogram for TAI scores. Please consider only the total TAI score, 
# which can be found at the *tp.csv file residing at the root of each subject file system.
scores <- read.csv("tai_scores.txt", header = FALSE)

hist(scores$V2,xlim=c(20,80),col='cadetblue', xlab="Tai scores",main="Histogram of Tai scores")

# Question 3
# State Psychometric Data: For each subject draw the barplots for all the NASA-TLX subscales per task. 
# This will give two figures per subject per subscale, one for suturing and one for cutting, where the 
# evolution of the scores from the initial to the final session will be evident. There should be a downward trend, 
# reflecting increased facility with the tasks
State.Pyschometric.Data <- function(){
  for(i in c(1,2,3,4,5,6,7,8,9, 10, 11, 12, 13,19, 20, 21,22,23,24,25,26)){
    path = paste("subject", formatC(i, width=2, flag="0"), sep="")  
    print(path)
    cuttingNasa = list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Cutting)(.*)(NASA)(.*?)$")
    suturingNasa = list.files(path = path, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Suturing)(.*)(NASA)(.*?)$")
    
    Response = c("Mental Demand","Physical Demand", "Temporal Demand", "Performance" ,"Effort" ,"Frustration", "Session")
    cutNASA = as.data.frame(Response)
    for(i in cuttingNasa){
      session = substr(i,28,28)
      temp <- read.csv(i, stringsAsFactors = FALSE)
      temp <- rbind(temp, c("Session", session))
      cutNASA <- dplyr::left_join(cutNASA, temp, by="Response")
    }
    
    sutNASA = as.data.frame(Response)
    for(i in suturingNasa){
      session = substr(i,28,28)
      temp <- read.csv(i, stringsAsFactors = FALSE)
      temp <- rbind(temp, c("Session", session))
      sutNASA <- dplyr::left_join(sutNASA, temp, by="Response")
    }
    
    #added new by yash
    cutNASA <- melt(cutNASA, id.vars='Response')
    
    c <- ggplot(cutNASA, aes(Response, as.integer(value))) +   
        geom_bar(aes(fill = variable), position = "dodge", stat="identity") + scale_fill_discrete(name="Sessions") + ylab("Scores") +
      theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste(path, "State Psychometric Data of Cutting and Suturing"))
      
    sutNASA <- melt(sutNASA, id.vars='Response')
    
    s <- ggplot(sutNASA, aes(Response, as.integer(value))) +   
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") + scale_fill_discrete(name="Sessions") + ylab("Scores") +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    png(paste(path,"State Psychometric Data of Cutting and Suturing.png"))
    multiplot(c, s, cols = 1)
    dev.off()
    
  }
}
State.Pyschometric.Data()

# Question 4
# Perinasal Perspiration (Stress) Signal Data: For each session of each subject draw the stress signals, 
# using black for baseline, green for cutting, and red for suturing. Generally speaking, the baseline signal 
# should be at a lower level than the other two. In total, you will draw five figures for each subject or 
# whatever the number of his/her sessions is.
Perinasal.Perspiration <- function(){
  for(i in 1:26){
    path = paste("subject", formatC(i, width=2, flag="0"), sep="")  
    print(path)
    
    path = paste(path, path, sep="/")
    sessions = list.dirs(path)
    
    for(p in sessions[-1]){
      session = substr(p,28,28)
      baseline = list.files(path = p, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Baseline)[[:digit:]]{1}\\.csv$")
      cutting = list.files(path = p, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Cutting)[[:digit:]]{1}\\.csv$")
      suturing = list.files(path = p, full.names = TRUE, recursive = TRUE, pattern = "^(.*)(Suturing)[[:digit:]]{1}\\.csv$")
      if(length(baseline) > 0){
        b <- read.csv(baseline[1])
      } 
      if(length(cutting) > 0){
        c <- read.csv(cutting[1])
      } 
      if(length(suturing) > 0){
        s <- read.csv(suturing[1])
      } 
      
      n = paste(path,session, sep =" Session ")
      
      if (length(b) > 0){
        p1 <- geom_line(data=b,aes(Time, Perspiration))
      } else {
        p1 <- NULL
      }
      if (length(c) > 0){
        p2 <- geom_line(data=c,aes(Time, Perspiration), colour="green")
      } else {
        p2 <- NULL
      }
      if (length(s) > 0){
        p3 <- geom_line(data=s,aes(Time, Perspiration,colour="red")) 
      } else {
        p3 <- NULL
      }
      
      p5 <- ggplot() + xlab("") +
        theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste("Perinasal Perspiration", session, sep = "Session ")) + xlab("Time") +
        scale_fill_manual(values=c("black", "red", "green"), 
                          name="Experimental\nCondition",
                          labels=c("Baseline", "Cutting", "Suturing")) + theme(legend.title=element_blank()) + ylab(0,0.05)
      png(paste(n,"Perinasal Perspiration.png")) 
      print(p5 + p1 + p2 + p3)
      #multiplot(p1,p2,p3, cols=1)
      dev.off()
      
      b <- NULL
      c <- NULL
      s <- NULL
      
    }
  }
  
}
Perinasal.Perspiration()

# Question 5
# Performance Data: Draw the time barplots for each subject, using different colors for each task (cutting vs. suturing).
# You should observe a downward trend. Draw also the accuracy barplots for each subject, using different colors for 
# each task (cutting vs. suturing). You should observe an ascending trend.

### Read the data
perf.Data <- read.xlsx("MicrosurgeryPerformance.xlsx")
# Remove last row
perf.Data <- perf.Data[1:15,]


mdata <- as.data.frame(t(perf.Data))

plotTimeBarPlots <- function(x){
  
}
