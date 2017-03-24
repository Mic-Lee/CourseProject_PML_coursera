library(caret)
library(data.table)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)
library(e1071)

#loading data
path <- getwd()
training.path <- file.path(path, "pml-training.csv")
testing.path <- file.path(path,"pml-testing.csv")
training.data <- fread(training.path)
testing.data <- fread(testing.path)

#processing data
## sub.train and sub.test
inTrain <- createDataPartition(y=training.data$classe, p=0.6, list=FALSE)
myTraining <- training.data[inTrain, ]
myTesting <- training.data[-inTrain, ]
dim(myTraining); dim(myTesting)
##transfer inorder to clean
myTraining <- as.data.frame(myTraining)
myTesting <- as.data.frame(myTesting)
##clean var with more than 55% NAs
training.minusNA <- myTraining 
for(i in 1:length(myTraining)) { 
  if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { 
    for(j in 1:length(training.minusNA)) {
      if( length( grep(names(myTraining[i]), names(training.minusNA)[j]) ) ==1)  { 
        training.minusNA <- training.minusNA[ , -j] 
      }   
    } 
  }
}
myTraining <- training.minusNA
rm(training.minusNA)
##remove some personal and nonrelative vars
myTraining <- myTraining[,8:length(myTraining)]
##remove NearZeroVariance variables
nzv.tr <- nearZeroVar(myTraining, saveMetrics=TRUE)
myTraining <- myTraining[,nzv.tr$nzv==FALSE]


##synchronizing processing
myTesting <- myTesting[colnames(myTraining)]
testing.data <- as.data.frame(testing.data)
testing.data <- testing.data[colnames(myTraining[, -53])]             
dim(myTesting);dim(testing.data)

##coerce the data into the same types
for (i in 1:length(testing.data) ) {
  for(j in 1:length(myTraining)) {
    if( length( grep(names(myTraining[i]), names(testing.data)[j]) ) == 1)  {
      class(testing.data[j]) <- class(myTraining[i])
    }      
  }      
}

##for random forest
myTraining$classe = as.factor(myTraining$classe)
myTesting$classe = as.factor(myTesting$classe)


