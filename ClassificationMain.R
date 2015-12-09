# This sciprt file contains a frame for learning handwritten digitals from the MNIST dataset

#set working directory
setwd("/home/artem/git_proj/LearnMNIST")
#setwd("/home/grachevam/neuralnet")
source("loadMNISTData.R")
source("NeuralNets.R")
source("Train.R")
source("Test.R")

# load training data from files
data <- loadMNISTData("/home/artem/advanced_topics_in_ml/mnist/train-images.idx3-ubyte", 
                      "/home/artem/advanced_topics_in_ml/mnist/train-labels.idx1-ubyte")
trainLabels <- data$labels
trainData <- data$data

print(dim(trainData))
print(dim(trainLabels))
# trainingData should be 60000x786,  60000 data and 784 features (28x28), tha matrix trainData has 60000 rows and 784 columns
# trainingLabels should have 60000x1, one class label \in {0,1,...9} for each data.

#uncomment the following 3 lines to see the nth training example and its class label.
n = 130;
image( t(matrix(trainData[n, ], ncol=28, nrow=28)), Rowv=28, Colv=28, col = heat.colors(256),  margins=c(5,10))
print("Class label:"); print(trainLabels[n])
dim(trainData[n, ])

# train a model

system.time( classifier <- learnModel(data = trainData, labels = trainLabels) )
predicted <- testModel(classifier, trainData)

#calculate accuracy on training data
print("accuracy on training data: \n")
print(sum(predicted$labels == trainLabels)/length(trainLabels))

i.class = 3
evaluate(predicted$labels, predicted$probs, trainLabels, i.class)

#calculate the following error metric for each class obtained on the train data:
#Recall, precision, specificity, F-measure, FDR and ROC for each class separately. Use a package for ROC. 


# test the model
data <- loadMNISTData("/home/artem/advanced_topics_in_ml/mnist/t10k-images.idx3-ubyte", 
                      "/home/artem/advanced_topics_in_ml/mnist/t10k-labels.idx1-ubyte")

testLabels <- data$labels
testData <- data$data

print(dim(testData))
print(dim(testLabels))
#trainingData should be 10000x786,  10000 data and 784 features (28x28), tha matrix trainData has 10000 rows and 784 columns
#trainingLabels should have 10000x1, one class label \in {0,1,...9} for each data.

predicted <- testModel(classifier, testData)


#calculate accuracy
print("accuracy on test data:\t")
print(sum(predicted$labels == testLabels)/length(testLabels))
i.class = 7
evaluate(predicted$labels, predicted$probs, testLabels, i.class)

#calculate the following error metric for each class obtained on the test data:
#Recall, precision, specificity, F-measure, FDR and ROC for each class separately. Use a package for ROC. 
