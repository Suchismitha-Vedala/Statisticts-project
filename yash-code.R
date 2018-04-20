library(openxlsx)
library(ggplot2)
library(reshape)
options(scipen=999)
olddir = getwd()

# read csv file

df1 <- read.csv("total_data.csv")

df1$log_percp <- log10(df1$Mean_Perspiration ) * -8

  
df1$log_percp <-  replace(df1$log_percp , is.na(df1$log_percp ), 0)
  
ggplot(df1, aes(x=X))  + 
  geom_line(aes(y=Score1),stat="identity", col = "#5fd8d4") +
  geom_point(y=df1$log_percp) + scale_y_continuous(sec.axis = sec_axis(~., name="Users (log scaled) * 10")) +
  xlab("Days") + ylab("Avg Number of articles read ") + 
  theme(plot.title = element_text(hjust = 0.5))+ ggtitle(paste("Decile 10"))
