#read the training & test data
training <- read.csv('amazon_baby_train.csv')
test <- read.csv('amazon_baby_test.csv')

#data preprocessing
maxs <- apply(training[4], 2, max)
mins <- apply(training[4], 2, min)
scaled.data <- as.data.frame(scale(training[4],center = mins, scale = maxs - mins))

#generate formula
feats <- names(training[4])
f <- paste(feats,collapse=' + ')
f <- paste('rating ~',f)
f <- as.formula(f)

#install neuralnet package & generate model
#install.package('neuralnet')
library(neuralnet)
nn <- neuralnet(f,training,hidden=c(3,2),linear.output=FALSE,err.fct='sse', 
                learningrate=0.01,algorithm = 'backprop')

#analyze & plot neural network
weights(nn)
prediction(nn)
nn$covariate
infert$case
plot(nn,arrow.length = 0.15)

#predict test data
predicted.nn.values <- compute(nn,test[4])
print(head(predicted.nn.values))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$rating,predicted.nn.values$net.result)
