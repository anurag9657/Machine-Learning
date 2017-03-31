library('ggplot2')
library('kernlab')
library('ROCR')
library('manipulate')
############################################################################################
training_dt <- read.csv('amazon_baby_train.csv')     
test_dt <- read.csv('amazon_baby_test.csv')

training_dt$rating[training_dt$rating==1] <- -1
training_dt$rating[training_dt$rating!=-1] <- 1
test_dt$rating[test_dt$rating==1] <- -1
test_dt$rating[test_dt$rating!=-1] <- 1

linear1.svm <- ksvm( rating ~ review1, data=training_dt, type='C-svc', kernel='vanilladot',C=10)
linear1.svm
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$rating )/length(test_dt$rating), '%'))

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))


alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)

############################################################################################
training_dt <- read.csv('amazon_baby_train.csv')     
test_dt <- read.csv('amazon_baby_test.csv')

training_dt$rating[training_dt$rating==2] <- -1
training_dt$rating[training_dt$rating!=-1] <- 1
test_dt$rating[test_dt$rating==2] <- -1
test_dt$rating[test_dt$rating!=-1] <- 1

linear1.svm <- ksvm( rating ~ review1, data=training_dt, type='C-svc', kernel='vanilladot',C=10)
linear1.svm
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$rating )/length(test_dt$rating), '%'))

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))


alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)

############################################################################################
training_dt <- read.csv('amazon_baby_train.csv')     
test_dt <- read.csv('amazon_baby_test.csv')

training_dt$rating[training_dt$rating==3] <- -1
training_dt$rating[training_dt$rating!=-1] <- 1
test_dt$rating[test_dt$rating==3] <- -1
test_dt$rating[test_dt$rating!=-1] <- 1

linear1.svm <- ksvm( rating ~ review1, data=training_dt, type='C-svc', kernel='vanilladot',C=10)
linear1.svm
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$rating )/length(test_dt$rating), '%'))

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))


alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)

############################################################################################
training_dt <- read.csv('amazon_baby_train.csv')     
test_dt <- read.csv('amazon_baby_test.csv')

training_dt$rating[training_dt$rating==4] <- -1
training_dt$rating[training_dt$rating!=-1] <- 1
test_dt$rating[test_dt$rating==4] <- -1
test_dt$rating[test_dt$rating!=-1] <- 1

linear1.svm <- ksvm( rating ~ review1, data=training_dt, type='C-svc', kernel='vanilladot',C=10)
linear1.svm
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$rating )/length(test_dt$rating), '%'))

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))


alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)

############################################################################################
training_dt <- read.csv('amazon_baby_train.csv')     
test_dt <- read.csv('amazon_baby_test.csv')

training_dt$rating[training_dt$rating==5] <- -1
training_dt$rating[training_dt$rating!=-1] <- 1
test_dt$rating[test_dt$rating==5] <- -1
test_dt$rating[test_dt$rating!=-1] <- 1

linear1.svm <- ksvm( rating ~ review1, data=training_dt, type='C-svc', kernel='vanilladot',C=10)
linear1.svm
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$rating )/length(test_dt$rating), '%'))

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))


alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)