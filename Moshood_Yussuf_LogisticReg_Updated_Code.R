#### Libraries #####
library(ggplot2)
library(plyr)
library(gam)
library(reshape)
library(readr)
library(tidyverse)
library(mice)
library(VIM)
library(dplyr)
library(lubridate)
library(hms)
library(readxl)
library(e1071)   
library(Rcpp)    
library(caret)
library(pastecs)
library(InformationValue)
library(ROCR)
library(gains)
library(MASS)
library(bestglm)


# Loading data and checking for missing data
library(readxl)
df <- read_excel("C:/Users/Flow/Downloads/MoshoodYussufBikeS.xlsx")


### converting timestamp to month,weekday,hour and year ###
month <- as.integer(format(as.POSIXlt(df$datetime), format = "%m"))
weekday <- as.integer(format(as.POSIXlt(df$datetime), format = "%u"))
hour <- as.integer(format(as.POSIXlt(df$datetime), format = "%H"))
year <- as.integer(format(as.POSIXlt(df$datetime), format = "%y"))
head(df)

mean(df$count)

df$count.new  <- ifelse(test = df$count >= 175.7105, yes = 1, no = 0)
view(df)




### New Data frame of 12 variables with timestamp already formatted and atemp remove
df <- data.frame(df$season, month, weekday, hour, year, df$workingday, df$holiday, 
                 df$weather, df$temp, df$humidity, df$windspeed, df$count.new)



names(df) <- c("season", "month", "weekday", "hour","year", "is_it_a_weekday", 
               "is_it_a_holiday", "weathertype", "temperature", "humidity", "windspeed", "count")


### removing the 0.000 from windspeed instead of replacing/filling with random number ###
df <- df[which(df$windspeed != 0.0000),]



# Weather is 1, 2, 3, 4
# Want to convert it to categorical (R will create dummy variables)
df$weathertype <- factor(df$weathertype, levels = c(1, 2, 3, 4), 
                            labels = c("Clear/Cloudy", "Mist", "Light Rain/Snow", "Heavy Rain/Snow"))

# Season is 1, 2, 3, 4
df$season <- factor(df$season, levels = c(1, 2, 3, 4), 
                         labels = c("Winter", "Spring", "Summer", "Fall"))

view(df)


# partition data
set.seed(2)   
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.6)  
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]


# Run logistic regression
logit.reg <- glm(count ~ ., data = train.df, family = "binomial")  
options(scipen=999) 
summary(logit.reg)

anova(logit.reg, test="Chisq")


pred1 <- predict(logit.reg, train.df, type = "response")
stat.desc(pred1)

pred2 <- predict(logit.reg, valid.df, type = "response")
stat.desc(pred2)

varImp(logit.reg)
plot(varImp(logit.reg))

names(logit.reg)

car::vif(logit.reg)

# Confusion matrix
c.mat <- table(ifelse(pred1 > 0.5, 1, 0), train.df[,12])
c.mat
sum(diag(c.mat))/sum(c.mat) # this gives accuracy 


c.mat <- table(ifelse(pred2 > 0.5, 1, 0), valid.df[,12])
c.mat
sum(diag(c.mat))/sum(c.mat) # this gives accuracy 

# number of 0 and 1 in valid.df    #one way table
table(valid.df[,12])
table(valid.df[,12], ifelse(pred > 0.5, 1, 0))




#ROC Curve and other capabilities of "InformationValue"


plotROC(train.df[,12], pred1)
confusionMatrix(train.df[,12], pred1, threshold = 0.5)
misClassError(train.df[,12], pred1, threshold = 0.5)

plotROC(valid.df[,12], pred2)
confusionMatrix(train.df[,12], pred2, threshold = 0.5)
misClassError(train.df[,12], pred2, threshold = 0.5)

# Compute the optimal probability cutoff score, based on a user defined objective
optimalCutoff(valid.df[,12], pred, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)




# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -12], type = "response")

data.frame(actual = valid.df$count[1:5], predicted = logit.reg.pred[1:5])

data.frame(actual = valid.df$count[1:5], predicted = pred[1:5])



gain <- gains(valid.df$count, logit.reg.pred, groups=10)
class(gain)
names(gain)

data.frame(c(0,gain$cume.pct.of.total*sum(valid.df$count)) ,
           c(0,gain$cume.obs))

data.frame( c(0,sum(valid.df$count)) , c(0, dim(valid.df)[1]))

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$count))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$count))~c(0, dim(valid.df)[1]), lty=2)





# Model Building
# Model 2
logit.reg2 <- glm(count ~  hour + year + temperature + season, data = train.df, family = "binomial")  
options(scipen=999) 
summary(logit.reg2)


anova(logit.reg2, test="Chisq")


pred3 <- predict(logit.reg2, train.df, type = "response")
stat.desc(pred3)

pred4 <- predict(logit.reg2, valid.df, type = "response")
stat.desc(pred4)

varImp(logit.reg2)
plot(varImp(logit.reg2))

names(logit.reg2)


# Confusion matrix
c.mat2 <- table(ifelse(pred3 > 0.5, 1, 0), train.df[,12])
c.mat2
sum(diag(c.mat2))/sum(c.mat2) # this gives accuracy 


# number of 0 and 1 in valid.df    #one way table
table(valid.df[,12])
table(valid.df[,12], ifelse(pred2 > 0.5, 1, 0))




#ROC Curve and other capabilities of "InformationValue"


plotROC(train.df[,12], pred3)
confusionMatrix(train.df[,12], pred3, threshold = 0.5)
misClassError(train.df[,12], pred3, threshold = 0.5)

# Compute the optimal probability cutoff score, based on a user defined objective
optimalCutoff(valid.df[,12], pred2, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)




# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred2 <- predict(logit.reg2, valid.df[, -12], type = "response")

data.frame(actual = valid.df$count[1:5], predicted = logit.reg.pred2[1:5])

data.frame(actual = valid.df$count[1:5], predicted = pred2[1:5])



gain <- gains(valid.df$count, logit.reg.pred2, groups=10)
class(gain)
names(gain)

data.frame(c(0,gain$cume.pct.of.total*sum(valid.df$count)) ,
           c(0,gain$cume.obs))

data.frame( c(0,sum(valid.df$count)) , c(0, dim(valid.df)[1]))

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$count))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$count))~c(0, dim(valid.df)[1]), lty=2)







# Weather is 1, 2, 3, 4
# Want to convert it to categorical (R will create dummy variables)
df$weathertype <- factor(df$weathertype, levels = c(1, 2, 3, 4), 
                         labels = c("Clear/Cloudy", "Mist", "Light Rain/Snow", "Heavy Rain/Snow"))

# Season is 1, 2, 3, 4
df$season <- factor(df$season, levels = c(1, 2, 3, 4), 
                    labels = c("Winter", "Spring", "Summer", "Fall"))

view(df)