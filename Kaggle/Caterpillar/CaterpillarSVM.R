#library used
library(lubridate)
#library(AppliedPredictiveModeling)
library(caret)
library(data.table)
#library(xgboost)
#library(Matrix)
library(methods)
library(e1071)
library(rpart)
library(Metrics)
library(doMC) # for multiple cores
registerDoMC(cores=3)

# Introducing the data
train <- read.csv("train_set.csv", header = TRUE)
test <- read.csv("test_set.csv", header = TRUE)

## Cleaning the data 1, removing bracket pricing

#train.cl <- subset(train, select = -c(tube_assembly_id,supplier, bracket_pricing,quote_date))
#test.cl <- subset(test, select = -c(tube_assembly_id, supplier,bracket_pricing, quote_date,id) )

## Cleaning the data 2, considering bracket pricing

#train.cl <- subset(train, select = -c(tube_assembly_id,supplier,quote_date))
#test.cl <- subset(test, select = -c(tube_assembly_id, supplier, quote_date,id) )

train.cl$bracket_pricing <- factor(train.cl$bracket_pricing)
test.cl$bracket_pricing <- factor(test.cl$bracket_pricing)

#train.cl$bracket_pricing <- ifelse(train.cl$bracket_pricing == "Yes",1,0)
#test.cl$bracket_pricing <- ifelse(test.cl$bracket_pricing == "Yes",1,0)





train$id = -(1:nrow(train))
test$cost = 0

train = rbind(train, test)

### Merging  common variables
#continueLoop = TRUE
#while(continueLoop){
#  continueLoop = FALSE
#  for(f in dir("../input/")){
#    d = read.csv(paste0("../input/", f))
#    commonVariables = intersect(names(train), names(d))
#    if(length(commonVariables) == 1){
#      train = merge(train, d, by = commonVariables, all.x = TRUE)
#      continueLoop = TRUE
#     print(dim(train))
#    }
# }
#}

## Removing NA values if any
#for(i in 1:ncol(train)){
#  if(is.numeric(train[,i])){
#    train[is.na(train[,i]),i] = -1
#  }else{
#    train[,i] = as.character(train[,i])
#    train[is.na(train[,i]),i] = "NAvalue"
#    train[,i] = as.factor(train[,i])
#  }
#}

###Clean variables with too many categories
for(i in 1:ncol(train)){
 if(!is.numeric(train[,i])){
   freq = data.frame(table(train[,i]))
    freq = freq[order(freq$Freq, decreasing = TRUE),]
    train[,i] = as.character(match(train[,i], freq$Var1[1:30]))
  train[is.na(train[,i]),i] = "rareValue"
   train[,i] = as.factor(train[,i])
 }
}

test = train[which(train$id > 0),]
train = train[which(train$id < 0),]

## removing the id from train, and the cost(all were zeros) from test

train.cl <- subset(train, select = -c(id))
test.cl <- subset(test, select = -c(cost) )

train.cl$bracket_pricing <- factor(train.cl$bracket_pricing)
test.cl$bracket_pricing <- factor(test.cl$bracket_pricing)

##Cleaning the data 2 #better result

#train.cl <- subset(train, select = -c(tube_assembly_id,supplier,quote_date))
#test.cl <- subset(test, select = -c(tube_assembly_id, supplier, quote_date,id) )

#train.cl$bracket_pricing <- factor(train.cl$bracket_pricing)
#test.cl$bracket_pricing <- factor(test.cl$bracket_pricing)

#train.cl$bracket_pricing <- ifelse(train.cl$bracket_pricing == "Yes",1,0)
#test.cl$bracket_pricing <- ifelse(test.cl$bracket_pricing == "Yes",1,0)


# Creating new data frame by adding extra column for higher order polynomial approximation

#train.cl <- data.frame(train.cl, train.cl[,2]^2,train.cl[,5]^2,train.cl[,7]^2,train.cl[,7]^3)


## Scaling the data
train.temp <- scale(train.cl[-4],center = TRUE, scale =TRUE);
test.cl <- scale(test.cl,center = TRUE, scale =TRUE);
train.cl <- data.frame(train.temp, train.cl[4]);


## Splitting cleaned training set into my.train(%80) and my.test(%20);

##Sample Indexes
indexes = sample(1:nrow(train.cl), size=0.2*nrow(train.cl))

## Split data my test data with cost column
my.test.with.cost = train.cl[indexes,]

## My train data
my.train = train.cl[-indexes,]

##removing the cost column and ready to be predicted according to my.train
my.test <- subset(my.test.with.cost, select = -c(cost))


# TODO updated the prediction via MARS
## svm for my train set and my test set. error in this setting is ~0.42
fit.svm <- svm(my.train$cost ~ ., data = my.train, cost = 10, gamma = 600, epsilon = 0.01, tolerance = 0.0001)
svm.prediction <- predict(fit.svm, my.test)
predicted.data <- cbind(1:NROW(my.test),svm.prediction)

#set negative values to 0 as we will use log function!
predicted.data[predicted.data[,2] < 0, 2] <- 0
colnames(predicted.data)<-c('id','cost')

error <- rmsle(my.test.with.cost$cost,predicted.data[,2])

# Lets apply our model to original test_set and submit to see the result error is   ~0.43

fit.svm.original.train <- svm(train.cl$cost ~ ., data = train.cl, cost = 10, gamma = 600, , epsilon = 0.01, tolerance = 0.0001)
svm.prediction.submission <- predict(fit.svm.original.train, test.cl)
predicted.data.submission <- cbind(1:NROW(test.cl),svm.prediction.submission)

#set negative values to 0 as we will ise log function!
predicted.data.submission[predicted.data.submission[,2] < 0, 2] <- 0
colnames(predicted.data.submission)<-c('id','cost')

#Forming a submission table
write.csv(predicted.data.submission, 'SubmissionSVM.csv', quote = FALSE, row.names = FALSE)


