#pre with decision tree
set.seed(12345)
modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")
fancyRpartPlot(modFitA1, tweak = 2)
# rpart.plot(modFitA1)
# prp(modFitA1, main="An Example",
#     type=1, fallen=T, branch=.3, round=0, leaf.round=9,
#     clip.right.labs=F, under.cex=1,
#     box.palette="GnYlRd",
#     prefix="ozone\n", branch.col="gray", branch.lwd=2,
#     extra=101, under=T, lt=" < ", ge=" >= ", cex.main=1.5,tweak=1.2,gap = 0)

predictionsA1 <- predict(modFitA1, myTesting, type = "class")
cmtree <- confusionMatrix(predictionsA1, myTesting$classe)
cmtree
plot(cmtree$table, col = cmtree$byClass, main = paste("Decision Tree Confusion Matrix: Accuracy =", round(cmtree$overall['Accuracy'], 4)))

#pre with random forest
set.seed(12345)

modFitB1 <- randomForest(classe ~ ., data=myTraining, method = "Class")
predictionB1 <- predict(modFitB1, myTesting, type = "class")
cmrf <- confusionMatrix(predictionB1, myTesting$classe)
cmrf
plot(modFitB1)
plot(cmrf$table, col = cmtree$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmrf$overall['Accuracy'], 4)))

#pre with SVM
# linear SVM
svmfit <- svm(classe ~ ., data = myTraining, kernel = "linear", scale = T) # linear svm, scaling turned OFF
print(svmfit)
compareTable <- table (myTraining$classe, predict(svmfit));compareTable  # tabulate
mean(myTraining$classe != predict(svmfit)) # 20.66% misclassification error

#then the random forest model is the best model
bestfitmodel <- modFitB1

#Predicting Results on the Test Data
predictionB2 <- predict(bestfitmodel, testing.data, type = "class")
predictionB2

