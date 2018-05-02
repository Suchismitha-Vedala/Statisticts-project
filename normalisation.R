library(openxlsx)
df=read.csv("C:/Users/lavan/Desktop/final_output.csv")
list1=df$Mean_Perspiration
#list1=na.omit(df$Mean_Perspiration)
list1[is.na(list1)] <- 555
minimumvalue=min(list1)

for(i in c(1:length(list1))) {
    list1[i]=list1[i]-minimumvalue
    #appendedlist=append(appendedlist,newlist)
    print(list1[i])
}

for(i in c(1:length(list1))) {
  if (list1[i] > 500) {
    list1[i]=NA
    print(list1[i])
  }
}
print(list1)
df["Normalised_Data"]<-NA
df$Normalised_Data=list1
write.csv(df, "Normalised.csv")



