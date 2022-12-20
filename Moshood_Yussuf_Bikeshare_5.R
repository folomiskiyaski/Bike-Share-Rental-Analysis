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
library(rpart)
library(rpart.plot)
library(adabag)
library(gbm) 
library(randomForest)



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

df$count.new  <- ifelse(test = df$count >= 173.2512, yes = 1, no = 0)
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


# partition data
set.seed(2)   
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.6)  
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]


# 1. Classification Tree
ct <- rpart(count ~ ., data = train.df, method = "class")
ct2 <- rpart(count ~ season + hour + humidity + temperature, data = train.df, method = "class") 


# plot tree
prp(ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
prp(ct)

prp(ct2, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
prp(ct2)

# Prediction
ct.pred.train <- predict(ct, train.df,type = "class")
table(ct.pred.train, train.df$count)

ct2.pred.train <- predict(ct2, train.df,type = "class")
table(ct2.pred.train, train.df$count)

ct.pred.valid <- predict(ct, valid.df,type = "class")
table(ct.pred.valid, valid.df$count)




## 2. random forest
rf <- randomForest(as.factor(count) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

## confusion matrix for training data
rf.pred <- predict(rf, train.df)
table(rf.pred, train.df$count)

## confusion matrix for validation data
rf.pred2 <- predict(rf, valid.df)
table(rf.pred2, valid.df$count)


# 3. Boosting and Bagging method
train.df$count <- as.factor(train.df$count)
set.seed(1)


boost <- gbm(count ~ ., data = train.df, distribution = "multinomial")

pred <- predict(boost, train.df, type="response")
head(pred)
predicted.class= apply(pred,1,which.max)-1
table(predicted.class, train.df$count)

pred2 <- predict(boost, valid.df, type="response")
head(pred2)
predicted.class2= apply(pred2,1,which.max)-1
table(predicted.class2, valid.df$count)



bag <- bagging(count ~ ., data = train.df)
pred3 <- predict(bag, train.df, type = "class")  
head(pred3)
table(pred3$class, train.df$count)

pred4 <- predict(bag, valid.df, type = "class")   
head(pred4)
table(pred4$class, valid.df$count)
