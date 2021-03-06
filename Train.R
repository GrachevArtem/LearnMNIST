
learnModel <- function(data, labels) { 
  #function for learn model with gradient descent
  #alfa is rate of learning
  
  alfa = 0.5
  lambda = 0.05
  
  data = data/255;
  labels.class = labels
  
  for( i in 1 : 9) { 
    labels.class = cbind(labels.class, labels, deparse.level = 0)
  }
  
  for ( i in 1 : 10 ) { 
    labels.class[, i][labels.class[, i] == i - 1] = -1
    labels.class[, i][labels.class[, i] != -1] = 0
  }
  
  labels.class[labels.class == -1] = 1
  
  net <- createNet(c(784, 250, 10))
  #net <- createNet(c(784, 75, 50, 25, 10))
  
  for (i in 1 : 10) { 
    
    costTrain = costTotal(net, data, labels.class, lambda)
    #costTest = costTotal(net, testData, testLabels, lambda)
    
    delta <- backProp(net, data, labels.class, lambda)
    
    for (i.l in 1 : length(net) ) { 
      
      net[[i.l]] <- net[[i.l]] - alfa*delta[[i.l]]
      
    }
    
    if ((i %% 5 == 0) || (i < 10)) { 
      print(i)
      cat("Train cost: ", costTrain, "\n")
      #cat("Train cost: ", costTrain, ";  Test cost: ", costTest, "\n")
    }
    
  }
  
  return (net)
}



learnModelStoch <- function(data, labels) { 
  
  alfa = 0.2
  lambda = 0.00
  
  #shuffle data
  set.seed(5)
  data <- data[sample(nrow(data)),]
  set.seed(5)
  labels <-sample(labels)
  
  data = data/255;
  labels.class = labels
  
  for( i in 1 : 9) { 
    labels.class = cbind(labels.class, labels, deparse.level = 0)
  }
  
  
  for ( i in 1 : 10 ) { 
    labels.class[, i][labels.class[, i] == i - 1] = -1
    labels.class[, i][labels.class[, i] != -1] = 0
  }
  
  labels.class[labels.class == -1] = 1
  
  
  net <- createNet(c(784, 230, 50, 10))
  
  
  for (i in 1 : 200) { 
  
    costTrain = costTotal(net, data, labels.class, lambda)
    #costTest = costTotal(net, testData, testLabels, lambda)
    
    left = 1
    for (right in seq(from = 300, to = dim(trainData)[1], by = 300)) { 
      
      sample.batch = data[left:right, ]
      labels.class.batch = labels.class[left:right, ]
      
      left = right + 1
    
      delta <- backProp(net, sample.batch, labels.class.batch, lambda)
    
      for (i.l in 1 : length(net) ) { 
      
        net[[i.l]] <- net[[i.l]] - alfa*delta[[i.l]]
      
      }
    
    }
    
    if ((i %% 5 == 0) || (i < 10)) { 
      print(i)
      cat("Train cost: ", costTrain, "\n")
      #cat("Train cost: ", costTrain, ";  Test cost: ", costTest, "\n")
    }
    
  }
  
  return (net)
}

