training <- read.csv('optdigits_raining.csv')
test <- read.csv('optdigits_test.csv')

training1 <- training[,1:64]
test1 = test[,1:64]


cl<- training[,65]

clt <- test[,65]

library(class)

m1 <- knn(training1, test1, cl, k=10, prob = FALSE, use.all = FALSE)

m1

sum(clt == m1)

table(clt, m1)

summary(m1)


