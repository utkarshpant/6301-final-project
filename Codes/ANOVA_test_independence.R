#setwd("C:/Users/soham/Documents/Soham/Assignments/6301/6301-final-project/dataset/")
data_heart <- read.csv('../dataset/heart.csv')

#Null Hypothesis: There isn't any significant difference between age & output

model <- aov(data_heart$output ~ data_heart$age, data= data_heart)

summary(model)


#Null Hypothesis: There isn't any significant difference between trtbps & output

model <- aov(data_heart$output ~ data_heart$trtbps, data= data_heart)

summary(model)


#Null Hypothesis: There isn't any significant difference between chol & output

model <- aov(data_heart$output ~ data_heart$chol, data= data_heart)

summary(model)


#Null Hypothesis: There isn't any significant difference between thalachh & output

model <- aov(data_heart$output ~ data_heart$thalachh, data= data_heart)

summary(model)

#Null Hypothesis: There isn't any significant difference between oldpeak & output

model <- aov(data_heart$output ~ data_heart$oldpeak, data= data_heart)

summary(model)

