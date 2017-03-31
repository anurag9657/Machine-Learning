#CART Implementation for OCR Dataset

install.packages("rpart")
library(rpart)                 #Recursive Partitioning and Regression Trees

install.packages("rpart.plot")
library(rpart.plot)            #To plot rpart tree

training_dt <- read.csv('optdigits_raining.csv')        #Read Training Data 

test_dt <- read.csv('optdigits_test.csv')      #Read Test Data

d_tree <- rpart(a65 ~ .,method="anova", data=training_dt)   #generate tree using rpart

rpart.plot(d_tree)       #plot tree using rpart.plot

printcp(d_tree)
summary(d_tree)
plotcp(d_tree)
rsq.rpart(d_tree)

p <- predict(d_tree, test_dt, type="matrix")     #predict test data using training data

table(test_dt[,65],p)       #output the result

d_tree1 <- rpart(a65 ~ .,method="anova", data=test_dt)   #generate tree using rpart for test data

rpart.plot(d_tree1) 


#Prune the tree
pfit <- prune(d_tree, cp=d_tree$cptable[which.min(d_tree$cptable[,"xerror"]),"CP"])

rpart.plot(pfit)             #plot the prunning tree


