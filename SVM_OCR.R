library('ggplot2')
library('kernlab')
library('ROCR')
library('manipulate')
library("caret")
############################################################################################
training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test.csv')

training_dt$a65[training_dt$a65!=0] <- 1
test_dt$a65[test_dt$a65!=0] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=100)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=100,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))
alpha(radial.svm)
alphaindex(radial.svm)
b(radial.svm)

print('Confusion Matrix: ')
print(table(pred1,test_dt$a65, dnn= c("prediction","reality") ))

prediction.score <- predict(radial.svm, test_dt, type='decision' )
nonLinear.roc.curve <- performance( prediction(prediction.score, test_dt$a65), measure='tpr', x.measure='fpr' )
plot( nonLinear.roc.curve )

nonLinear.pr.curve <- performance( prediction(prediction.score, test_dt$a65), measure='prec', x.measure='rec' )
plot( nonLinear.pr.curve )
###########################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test.csv')

training_dt$a65[training_dt$a65!=1] <- 0
test_dt$a65[test_dt$a65!=1] <- 0

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=10)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=100,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))
alpha(radial.svm)
alphaindex(radial.svm)
b(radial.svm)

print('Confusion Matrix: ')
print(table(pred1,test_dt$a65, dnn= c("prediction","reality") ))

prediction.score <- predict(radial.svm, test_dt, type='decision' )
nonLinear.roc.curve <- performance( prediction(prediction.score, test_dt$a65), measure='tpr', x.measure='fpr' )
plot( nonLinear.roc.curve )

nonLinear.pr.curve <- performance( prediction(prediction.score, test_dt$a65), measure='prec', x.measure='rec' )
plot( nonLinear.pr.curve )
##############################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test.csv')

training_dt$a65[training_dt$a65==2] <- 0
training_dt$a65[training_dt$a65!=2] <- 1
test_dt$a65[test_dt$a65==2] <- 0
test_dt$a65[test_dt$a65!=2] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=10)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

linear2.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
pred1 = predict(linear2.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))

linear1.svm
linear2.svm
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)
###########################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test.csv')

training_dt$a65[training_dt$a65==3] <- -1
training_dt$a65[training_dt$a65!=-1] <- 1
test_dt$a65[test_dt$a65==3] <- -1
test_dt$a65[test_dt$a65!=-1] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=100)
linear1.svm
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)

radial.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=100,scale=FALSE)
radial.svm
pred1 = predict(radial.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))
alpha(radial.svm)
alphaindex(radial.svm)
b(radial.svm)

print('Confusion Matrix: ')
print(table(pred1,test_dt$a65, dnn= c("prediction","reality") ))

prediction.score <- predict(radial.svm, test_dt, type='decision' )
nonLinear.roc.curve <- performance( prediction(prediction.score, test_dt$a65), measure='tpr', x.measure='fpr' )
plot( nonLinear.roc.curve )

nonLinear.pr.curve <- performance( prediction(prediction.score, test_dt$a65), measure='prec', x.measure='rec' )
plot( nonLinear.pr.curve )
##############################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test.csv')

training_dt$a65[training_dt$a65==4] <- -1
training_dt$a65[training_dt$a65!=-1] <- 1
test_dt$a65[test_dt$a65==4] <- -1
test_dt$a65[test_dt$a65!=-1] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=100)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

linear2.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=100,scale=FALSE)
pred1 = predict(linear2.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))

linear1.svm
linear2.svm
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)
##############################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test.csv')

training_dt$a65[training_dt$a65==5] <- 0
training_dt$a65[training_dt$a65!=5] <- 1
test_dt$a65[test_dt$a65==5] <- 0
test_dt$a65[test_dt$a65!=5] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=10)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

linear2.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
pred1 = predict(linear2.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))

linear1.svm
linear2.svm
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)
##############################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test1.csv')

training_dt$a65[training_dt$a65==6] <- 0
training_dt$a65[training_dt$a65!=6] <- 1
test_dt$a65[test_dt$a65==6] <- 0
test_dt$a65[test_dt$a65!=6] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=10)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

linear2.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
pred1 = predict(linear2.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))

linear1.svm
linear2.svm
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)
##############################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test1.csv')

training_dt$a65[training_dt$a65==7] <- 0
training_dt$a65[training_dt$a65!=7] <- 1
test_dt$a65[test_dt$a65==7] <- 0
test_dt$a65[test_dt$a65!=7] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=10)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

linear2.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
pred1 = predict(linear2.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))

linear1.svm
linear2.svm
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)
##############################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test1.csv')

training_dt$a65[training_dt$a65==8] <- 0
training_dt$a65[training_dt$a65!=8] <- 1
test_dt$a65[test_dt$a65==8] <- 0
test_dt$a65[test_dt$a65!=8] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=10)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

linear2.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
pred1 = predict(linear2.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))

linear1.svm
linear2.svm
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)
##############################################################################################

training_dt <- read.csv('optdigits_raining.csv')     
test_dt <- read.csv('optdigits_test1.csv')

training_dt$a65[training_dt$a65==9] <- 0
training_dt$a65[training_dt$a65!=9] <- 1
test_dt$a65[test_dt$a65==9] <- 0
test_dt$a65[test_dt$a65!=9] <- 1

linear1.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='vanilladot',C=10)
pred = predict(linear1.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred == test_dt$a65 )/length(test_dt$a65), '%'))

linear2.svm <- ksvm( a65 ~ ., data=training_dt, type='C-svc', kernel='rbfdot',C=10,scale=FALSE)
pred1 = predict(linear2.svm, test_dt)
print(paste0('Accuracy: ', 100*sum( pred1 == test_dt$a65 )/length(test_dt$a65), '%'))

linear1.svm
linear2.svm
alpha(linear1.svm)
alphaindex(linear1.svm)
b(linear1.svm)
##############################################################################################
