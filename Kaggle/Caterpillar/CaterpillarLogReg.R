#library used
library(lubridate)
library(AppliedPredictiveModeling)
library(caret)

# Introducing the data
train.data <- read.csv("train_set.csv", header = TRUE)
test.data <- read.csv("test_set.csv", header = TRUE)

# Cleaning/simplifying the data 

#train.data <- subset(train.data, select = -c(tube_assembly_id,supplier,bracket_pricing,quote_date))
#test.data <- subset(test.data, select = -c(tube_assembly_id,supplier,bracket_pricing,quote_date,id) )

## ANOTHER version with taking bracket_pricing in to account

train.data <- subset(train.data, select = -c(tube_assembly_id,supplier,quote_date))
test.data <- subset(test.data, select = -c(tube_assembly_id, supplier, quote_date,id) )

#train.data$supplier <- factor(train.data$supplier)
#test.data$supplier <- factor(test.data$supplier)

train.data$bracket_pricing <- factor(train.data$bracket_pricing)
test.data$bracket_pricing <- factor(test.data$bracket_pricing)

## Fit a logistic linear regression model with whole data
fitLogREg <- glm(cost ~., family = gaussian(link = 'identity'), train.data)
predictLogReg <- predict(fitLogREg,test.data)
# Form a table with predicted model

  
predicted.data <- cbind(1:NROW(test.data),predictLogReg)
#predicted.data[,2] <- exp(predicted.data[,2])


#set negative values to 0;
predicted.data[predicted.data[,2] < 0, 2] <- 0
colnames(predicted.data)<-c('id','cost')

#Forming a submission table
write.csv(predicted.data, 'SubmissionLogReg.csv', quote = FALSE, row.names = FALSE)
