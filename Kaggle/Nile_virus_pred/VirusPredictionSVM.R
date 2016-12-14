#library(Metrics)
library(caret) # main library for various learning algorithms
library(lubridate) # date operations
#library(DMwR)
library(stringr) # for parsing weather strings
library(pROC) # ROC metric
library(sp) # for distance computation
library(doMC) # for multiple cores
registerDoMC(cores=3)
training <- read.csv("./input/train.csv")
test <- read.csv("./input/test.csv")
weatherdata <- read.csv("./input/weatherNA.csv")
spraydata <- read.csv("./input/spray.csv")
weatherdata$date <- ymd(weatherdata$Date)
training$date <- ymd(training$Date)
test$date <- ymd(test$Date)
spraydata$date <- ymd(spraydata$Date)
training$WnvPresent <- factor(training$WnvPresent,labels=c("NO","YES"))

# Impute the missing values by using Caret

weathercols <- c("Tmax","DewPoint","WetBulb",
                 "StnPressure","AvgSpeed","ResultSpeed", 
                 "ResultDir","SeaLevel","PrecipTotal","Heat","Cool")
subweather <- weatherdata[weatherdata$Station==1,weathercols]
subweather[subweather$PrecipTotal=="  T","PrecipTotal"] <- NA
subweather$PrecipTotal <- as.numeric(subweather$PrecipTotal)
processedW <- preProcess(subweather,method=c("knnImpute"),thresh=0.9,na.remove=T)
imputedweather <- predict(processedW,subweather)
imputedweather$date <- ymd(weatherdata[weatherdata$Station==1,"Date"])

# I have categorized the weathers according to their humidity

imputedweather$CodeSum <-  weatherdata[weatherdata$Station==1,"CodeSum"]
imputedweather$FG <- 0 # FOG
imputedweather[ str_detect(imputedweather$CodeSum,"(BF|FG)"),"FG" ] <- 1
imputedweather$HZ <- 0 # Haze 
imputedweather[ str_detect(imputedweather$CodeSum,"(HZ|FU)"),"HZ" ] <- 1
imputedweather$RA <- 0 # Rain
imputedweather[ str_detect(imputedweather$CodeSum,"(RA|DZ)"),"RA" ] <- 1
imputedweather$TS <- 0 # Thunderstorm
imputedweather[ str_detect(imputedweather$CodeSum,"TS"),"TS" ] <- 1



# Merging with the weather
training <- merge(training,imputedweather,by="date",all=F)
test <- merge(test,imputedweather,by="date",all=F)


cat("So few days have been sprayed, in 2011:",length(unique(spraydata[year(spraydata$date)==2011,"date"])), 
    " and in 2013: ",length(unique(spraydata[year(spraydata$date)==2013,"date"])))

# add a distance metric, which includes spatial and temporal distance to spraying:
# spray.dates <- unique(spraydata$date)
# 

# # For instance: deltaDays * deltaEuclid or log(deltaDays)*deltaDays
# # point is made up of date,longitude,latitude
# sprayed.dist <- function(point){
#   
#   deltaDays <- point$date - spray.dates
#   deltaDays <- deltaDays[deltaDays >= 0]
#   if(length(deltaDays) == 0){
#     return(list("deltaTime"=1000,"deltaDist"=5))
#   } else {
#     d <- spray.dates[ which.min(deltaDays) ]
#     t <- subset(spraydata,subset=(date==d),select=c("Longitude","Latitude"))
#     euclid <- min(spDistsN1(pts=SpatialPoints(t),pt=as.numeric(point[,c("Longitude","Latitude")] )))
#     return( list("deltaTime"=as.integer(min(deltaDays)),"deltaDist"=euclid))
#     
#   }
#   
# }
# 
# training$spray.deltaTime <- NA
# training$spray.deltaDist <- NA
# library(foreach)
# temp<- foreach(i = 1:dim(training)[1], .combine=append,.init=list(),.inorder=T ) %dopar%
#  sprayed.dist(training[i,c("date","Longitude","Latitude")])
# 
# for(i in seq(1,length(temp),2)){
#   training[round((i+1)/2),"spray.deltaTime"] <- temp[[i]]
# }
# for(i in seq(2,length(temp),2)){
#   training[i/2,"spray.deltaDist"] <- temp[[i]]
# }
# 



#TODO: use timeseries data for the weather. Fit a linear vector on the last X days of temperature,windspeed etc.
#TODO: Consider the weather type, snow etc.
predictors <- append(predictors,c("tmax.intercept","tmax.slope"))
training$tmax.intercept<- NA
training$tmax.slope <- NA
for(d in unique(training$date)){
  weatherslice <- subset(weatherdata,subset=(date <= d & date >= (d-5*24*60*60) & Station==1 ))
  weatherslice$id <- 1:dim(weatherslice)[1]
  fitted.weather <- lm(Tmax ~ id,data=weatherslice)
  training[training$date==d,"tmax.intercept"] <- coef(fitted.weather)[1]
  training[training$date==d,"tmax.slope"] <- coef(fitted.weather)[2]
}

test$tmax.intercept<- NA
test$tmax.slope <- NA
for(d in unique(test$date)){
  weatherslice <- subset(weatherdata,subset=(date <= d & date >= (d-5*24*60*60) & Station==1 ))
  weatherslice$id <- 1:dim(weatherslice)[1]
  fitted.weather <- lm(Tmax ~ id,data=weatherslice)
  test[test$date==d,"tmax.intercept"] <- coef(fitted.weather)[1]
  test[test$date==d,"tmax.slope"] <- coef(fitted.weather)[2]
}

# We can up/down sample the data, however SVM has weighted classification therefore we skip this step
# upSampleTrain <- upSample(x=training[,append(predictors,"date")],y= training$WnvPresent,yname="WnvPresent")
# downSampleTrain <- downSample(x=training[,append(predictors,"date")],y= training$WnvPresent,yname="WnvPresent")
# table(upSampleTrain$WnvPresent)
# table(downSampleTrain$WnvPresent)
training$dyear <- year(training$date)
training$dmonth <- month(training$date)
training$yday <- yday(training$date)
test$yday <- yday(test$date)


# Create a CV (cross validation)
x1<- training[year(training$date)!=2011,]
x2<-training[year(training$date)==2011,]

## check sizes of split frames
table(x1$WnvPresent)
table(x2$WnvPresent)

# CLASS WEIGHTED SVM
set.seed(7)
library(kernlab)

# predictors = c("StnPressure", "AvgSpeed","SeaLevel","PrecipTotal","Heat",
#                "Species","Block","tmax.intercept","tmax.slope","yday","WnvPresent",
#                "spray.deltaTime","spray.deltaDist")
predictors = c("FG","HZ","RA","PrecipTotal","StnPressure", "Species","Block","tmax.intercept","tmax.slope","yday","WnvPresent")
# we will train over a large cost range, so we precompute the sigma parameter
sigmas <- sigest(WnvPresent ~ ., data=training[,predictors],frac=.75)
names(sigmas) <- NULL
svmGrid <- expand.grid(sigma=sigmas[2], C = 2^(-1:1))
# The below comment is from applied predictive modeling chapter16:
## Class probabilities cannot be generated with class weights, so
## use the control object ' ctrlNoProb ' to avoid estimating the
## ROC curve.
#Everything  but ROC
fourStats <- function (data, lev = levels(data$obs), model = NULL ){
  accKapp <- postResample(data[, "pred"], data[, "obs"])
  out <- c(accKapp,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]))
  names(out)[3:4] <- c("Sens", "Spec")
  out
}
ctrlNoProb <- trainControl(method = "repeatedcv", repeats=3,
                           classProbs = F,
                           summaryFunction = fourStats,
                           verboseIter = TRUE)
set.seed(7)
fit.svm <- train(WnvPresent ~ .,
                data = x1[, predictors],
                method = "svmLinear",
                #tuneGrid = svmGrid,
               tuneGrid = expand.grid(C=2^(-3:0)),
                preProc = c("center", "scale"),
                class.weights = c(YES = 18, NO = 1), # determined from the table(training$WnvPresent) YES/NO ratio
                metric = "Sens",
                trControl = ctrlNoProb)
svnraw <- predict(fit.svm,x2,"raw")
roc(as.numeric(x2$WnvPresent),as.numeric(svnraw),plot=T)



# TEST data has UNSPECIFIED CULEX as another species that does not exist in training
# So I have divided the test set into two. Either we will determin the real type of 
# a mosquito that is marked as UNSPECIFIED CULEX, or we will remove Species feature from the training set
# and train as if all the mosquitos are from the same type
test_unspecifiedculex <- test[test$Species == "UNSPECIFIED CULEX",] 
test_withspecies <- test[test$Species != "UNSPECIFIED CULEX",] 
test_withspecies$Species <- factor(test_withspecies$Species,exclude=c("UNSPECIFIED CULEX"))

set.seed(7)
fit.svm <- train(WnvPresent ~ .,
                 data = training[, predictors],
                 method = "svmLinear",
                 #tuneGrid = svmGrid,
                 tuneGrid = expand.grid(C=2^(-2)),
                 #tuneGrid = expand.grid(C=2^(-2:0)),
                 preProc = c("center", "scale"),
                 class.weights = c(YES = 18, NO = 1), # determined from the table(training$WnvPresent) YES/NO ratio
                 metric = "Sens",
                 trControl = ctrlNoProb)
svnraw <- predict(fit.svm,test_withspecies,"raw")
test$WnvPresent <- 0
test[test$Species != "UNSPECIFIED CULEX","WnvPresent"] <- svnraw

set.seed(7)
predictors <- predictors[predictors != "Species"]
fit.svm <- train(WnvPresent ~ .,
                 data = training[, predictors],
                 method = "svmLinear",
                 #tuneGrid = svmGrid,
                 tuneGrid = expand.grid(C=2^(-2)),
                 preProc = c("center", "scale"),
                 class.weights = c(YES = 18, NO = 1), # determined from the table(training$WnvPresent) YES/NO ratio
                 metric = "Sens",
                 trControl = ctrlNoProb)
svnraw <- predict(fit.svm,test_unspecifiedculex,"raw")
test[test$Species == "UNSPECIFIED CULEX","WnvPresent"] <- svnraw

test$WnvPresent <- test$WnvPresent -1 # factors are passed NO=1, YES=2 so we need to subtract 1

# pSubmit <- predict(fit.svm,test)
# submissionFile<-cbind(test$Id,pSubmit)
# colnames(submissionFile)<-c("Id","WnvPresent")
# options("scipen"=100, "digits"=8)
# write.csv(submissionFile,"svm_submit.csv",row.names=FALSE,quote=FALSE)
table(test$WnvPresent)
write.csv(subset(test,select=c("Id","WnvPresent")), file = "submission.csv", row.names=FALSE)


#qplot(x=yday,y=ResultSpeed,data=training,geom=c("point","smooth"),facets=dyear~WnvPresent)
#qplot(x=yday,y=ResultSpeed,data=training,geom=c("point","jitter"),facets=dyear~WnvPresent)
qplot(x=yday,y=ResultSpeed,data=training,geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent,size=WnvPresent)
qplot(x=tmax.slope,y=tmax.intercept,data=training,geom=c("point","smooth"),colour=WnvPresent,shape=WnvPresent)
qplot(x=spray.deltaTime,y=spray.deltaDist,data=training[training$spray.deltaTime < 50,],geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent)
qplot(x=yday,y=StnPressure,data=training,geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent,size=WnvPresent)
qplot(x=yday,y=DewPoint,data=training,geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent,size=WnvPresent)
qplot(x=yday,y=WetBulb,data=training,geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent,size=WnvPresent)
qplot(x=yday,y=PrecipTotal,data=training,geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent,size=WnvPresent)
qplot(x=yday,y=SeaLevel,data=training,geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent,size=WnvPresent)
qplot(x=yday,y=TS,data=training,geom=c("point","jitter"),colour=WnvPresent,shape=WnvPresent,size=WnvPresent)
