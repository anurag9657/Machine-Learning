#read the training & test data
training <- read.csv('optdigits_raining.csv')
test <- read.csv('optdigits_test.csv')

#data preprocessing
maxs <- apply(training[,1:64], 2, max)
mins <- apply(training[,1:64], 2, min)
scaled.data <- as.data.frame(scale(training[,1:64],center = mins, scale = maxs - mins))

#generate formula
feats <- names(training[,1:64])
f <- paste(feats,collapse=' + ')
f <- paste('a65 ~',f)
f <- as.formula(f)

#install neuralnet package & generate model
#install.package('neuralnet')
library(neuralnet)
nn <- neuralnet(f,training,hidden=c(5,3),linear.output=FALSE,err.fct='sse', 
                learningrate=0.01,algorithm = 'backprop')

#analyze & plot neural network
weights(nn)
prediction(nn)
nn$covariate
infert$case
plot(nn,arrow.length = 0.15)

#predict test data
predicted.nn.values <- compute(nn,training[,1:64])
print(head(predicted.nn.values))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$a65,predicted.nn.values$net.result)
