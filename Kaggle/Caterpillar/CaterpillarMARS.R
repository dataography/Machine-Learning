#library used
#library(lubridate)
#library(AppliedPredictiveModeling)
library(caret)
library(data.table)
library(Matrix)
library(methods)
library(e1071)
library(rpart)
library(Metrics)
library(doMC) # for multiple cores
registerDoMC(cores=3)
library(e1071)
library(rpart)
library(earth)
library(faraway) #  Julian Faraway's functions from her book
library(mda)    

## Introducing the data
train <- read.csv("train_set.csv", header = TRUE)
test <- read.csv("test_set.csv", header = TRUE)
qplot(quantity, min_order_quantity, data = train)

## Cleaning the data 

#train.cl <- subset(train, select = -c(tube_assembly_id,supplier, bracket_pricing,quote_date))
#test.cl <- subset(test, select = -c(tube_assembly_id, supplier,bracket_pricing, quote_date,id) )


## for bracket_pricing predictor addition 

train.cl <- subset(train, select = -c(tube_assembly_id,supplier,quote_date))
test.cl <- subset(test, select = -c(tube_assembly_id, supplier, quote_date,id) )

train.cl$bracket_pricing <- factor(train.cl$bracket_pricing)
test.cl$bracket_pricing <- factor(test.cl$bracket_pricing)

train.cl$bracket_pricing <- ifelse(train.cl$bracket_pricing == "Yes",1,0)
test.cl$bracket_pricing <- ifelse(test.cl$bracket_pricing == "Yes",1,0)



## Splitting cleaned training set into my.train(%80) and my.test(%20);

## Sample Indexes
indexes <- sample(1:nrow(train.cl), size=0.2*nrow(train.cl))

## Split data my test data with cost column
my.test.with.cost <- train.cl[indexes,]

## My train data
my.train <- train.cl[-indexes,]

## Removing the cost column and ready to be predicted according to my.train
my.test <- subset(my.test.with.cost, select = -c(cost))

########## SPLITING FOR CROSS VALIDATION- 5 FOLD ############

set.seed(7)
cvSplits <- createFolds(train.cl, k = 5, returnTrain = TRUE )



## MARS for my train set and my test set. error in this setting is 

## fitting my training set to evalute with my test set with  MARS function
fit.mars <- mars(my.train[,-4],    # matrix containing the independent variables
                 my.train$cost,     # vector containing the response variable
                 degree = 1, # default value TODO ?
                 prune = T)  # default: # TODO add  parameters

## Another version with EARTH function 
## TODO # mars gives better result, try and add different paramaters, also add new feature
fit.mars <- earth(my.train[,-4],    # matrix containing the independent variables
                 my.train[, 4],     # vector containing the response variable
                 degree = 1, # default value TODO ?
                 nprune = T)  # default

summary(fit.mars)

################ FOR ANALYSIS OF SPESIFIC PREDICTOR #############
# summary(lm(train.cl[,3] ~ fit.mars$x-1))
# fit.mars$gcv        # 747.5245 generalized cross validation error
# sum(fit.mars$res^2) # 22574494 -- fit is not good  in terms of R2 #TODO play with parameters
## table of cut-offs of hinge function
# hinge.cuts <- fit.mars$cuts[fit.mars$selected.terms, ]
# dimnames(hinge.cuts) <- list(NULL, names(train.cl[-1])) 
#######################################################################################

mars.prediction <- predict(fit.mars, my.test)

predicted.data <- cbind(1:NROW(my.test),mars.prediction)

## Handling negative values

## Taking absolute value of the nagative values
# predicted.data[predicted.data[,2]<0,2] <- -predicted.data[predicted.data[,2]<0,2]

## Set negative values to 0 as we will use log function!
predicted.data[predicted.data[,2] < 0, 2] <- 0
colnames(predicted.data)<-c('id','cost')

error <- rmsle(my.test.with.cost$cost,predicted.data[,2])


####### FOR SUBMISSION USING GIVEN TRAIN AND TEST SETS   ###############

## Fitting to whole training set
fit.mars.original.train <- mars(train.cl[,-4],    # matrix containing the independent variables
                 train.cl[, 4],     # vector containing the response variable
                 degree = 1, # default value TODO ?
                 prune = T)  



## Lets apply our model to original test_set and submit to see the result

mars.prediction.submission <- predict(fit.mars.original.train, test.cl)
predicted.data.submission <- cbind(1:NROW(test.cl),mars.prediction.submission)

## Set negative values to 0 as we will ise log function!
predicted.data.submission[predicted.data.submission[,2] < 0, 2] <- 0
colnames(predicted.data.submission)<-c('id','cost')

## Forming a submission table in cvs format
write.csv(predicted.data.submission, 'SubmissionMARS.csv', quote = FALSE, row.names = FALSE)


