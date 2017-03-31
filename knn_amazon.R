training <- read.csv('amazon_baby_train.csv')
test <- read.csv('amazon_baby_test.csv')

training1 <- training[,4]
test1 <- test[,4]

cl<- training[,3]

clt <- test[,3]

library(class)

m1 <- knn(data.frame(training1), data.frame(test1), cl, k=1)

sum(clt == m1)

table(clt, m1)

summary(m1)