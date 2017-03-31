library(caret)
library(caretEnsemble)

training_dt <- read.csv('amazon_baby_train.csv')     

test_dt <- read.csv('amazon_baby_test.csv')

# Example of Boosting Algorithms
seed <- 10
control <- trainControl(method="repeatedcv", number=100, repeats=5)
metric <- "RMSE"

# rpart
set.seed(seed)
fit.rpart1 <- train(rating~review1,data=training_dt, method="rpart",metric=metric,trControl=control)
summary(fit.rpart1)

# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(rating~review1, data=training_dt, method="gbm",metric=metric, trControl=control, verbose=FALSE)
summary(fit.gbm)

# summarize results
boosting_results <- resamples(list(rpart=fit.rpart, gbm=fit.gbm))

summary(boosting_results)
dotplot(boosting_results)
modelCor(boosting_results)
splom(boosting_results)

#test data prediction and cross validation
pr <- predict(fit.rpart1, test_dt)
print(pr)
mean(pr)
mean(test_dt$rating)
table(test_dt$rating,pr)

#prunning the tree
ptree<- prune(fit.rpart,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
printcp(fit.rpart)
