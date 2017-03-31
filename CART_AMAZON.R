#CART Implementation for Amazon Dataset

install.packages("rpart")
library(rpart)                 #Recursive Partitioning and Regression Trees

install.packages("rpart.plot")
library(rpart.plot)            #To plot rpart tree

a_training_dt <- read.csv('amazon_baby_train.csv')        #Read Training Data 

a_test_dt <- read.csv('amazon_baby_test.csv')      #Read Test Data

d_tree <- rpart(rating ~ .,method="anova", data=a_training_dt)   #generate tree using rpart

rpart.plot(d_tree)       #plot tree using rpart.plot

printcp(d_tree)
summary(d_tree)
plotcp(d_tree)
rsq.rpart(d_tree)

p <- predict(d_tree, a_test_dt, type="matrix")     #predict test data using training data

table(a_test_dt[,3],p)       #output the result

d_tree1 <- rpart(rating ~ .,method="anova", data=a_test_dt)   #generate tree using rpart for test data

rpart.plot(d_tree1) 


#Prune the tree
pfit <- prune(d_tree, cp=d_tree$cptable[which.min(d_tree$cptable[,"xerror"]),"CP"])

rpart.plot(pfit)             #plot the prunning tree


