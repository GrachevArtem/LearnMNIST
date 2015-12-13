
learnModel <- function(data, labels) { 
  
  alfa = 0.5
  lambda = 0.05
  
  data = data/255;
  labels.class = labels
  
  for( i in 1 : 9) { 
    labels.class = cbind(labels.class, labels, deparse.level = 0)
  }
  
  
  for ( i in 1 : 10 ) { 
    labels.class[labels.class[, i] == i - 1] = -1
    labels.class[labels.class[, i] != -1] = 0
  }
  
  labels.class[labels.class == -1] = 1
  
  
  net <- createNet(c(784, 250, 10))
  #net <- createNet(c(784, 75, 50, 25, 10))
  
  for (i in 1 : 400) { 
    
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
  lambda = 0.01
  
  data = data/255;
  labels.class = labels
  
  for( i in 1 : 9) { 
    labels.class = cbind(labels.class, labels, deparse.level = 0)
  }
  
  
  for ( i in 1 : 10 ) { 
    labels.class[labels.class[, i] == i - 1] = -1
    labels.class[labels.class[, i] != -1] = 0
  }
  
  labels.class[labels.class == -1] = 1
  
  
  net <- createNet(c(784, 75, 50, 25, 10))
  
  
  for (i in 1 : 10) { 
  
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

