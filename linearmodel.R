library(openxlsx)
df=read.csv("C:/Users/lavan/Desktop/final_output.csv")
#scores=append(df$Score1,df$Score2)

#linear modelling of scores with mean perspiration , age , year , sex , task and  session 
scorer1lm= lm(formula = df$Score1~df$Mean_Perspiration+df$Age+df$Year+df$Sex+df$Task+df$Session)
print(scorer1lm)
scorer2lm= lm(formula = df$Score2~df$Mean_Perspiration+df$Age+df$Year+df$Sex+df$Task+df$Session)
print(scorer2lm)

#linear modelling of scores with age 
score1_age=lm(formula = df$Score1~df$Age)
print(score1_age)
score2_age=lm(formula = df$Score2~df$Age)
print(score2_age)

#linear modelling of scores with year  
score1_year=lm(formula = df$Score1~df$Year)
print(score1_year)
score2_year=lm(formula = df$Score2~df$Year)
print(score2_year)

#linear modelling of scores with sex
score1_sex=lm(formula = df$Score1~df$Sex)
print(score1_sex)
score2_sex=lm(formula = df$Score2~df$Sex)
print(score2_sex)


#linear modelling of scores with mean perspiration 
score1_meanp=lm(formula = df$Score1~df$Mean_Perspiration)
print(score1_meanp)
score2_meanp=lm(formula = df$Score2~df$Mean_Perspiration)
print(score2_meanp)





