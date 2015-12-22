createNet <- function(layers){
  #Creating net
  #layers is a array of number neurons for each layer of net
  
  net <- list();
  
  for (i in 2:length(layers)){ 
    new.weights <- matrix(runif((layers[i-1] + 1)*(layers[i]), min = -0.05, max = 0.05),
                          ncol = layers[i-1] + 1, nrow = layers[i]);
    
    net[[i-1]] <- new.weights;
  }
  
  return(net)
}


activationFunc <- function(x) { 
  #Function using for activation
  
  abs.tol = 1e-6
  z <- 1/(1 + exp(-x))
  z[z == 1] <- 1 - abs.tol
  z[z == 0] <- 0 + abs.tol
  
  return(z)
}


derActivationFunc <- function(x) { 
  #Derivative of activation function
  
  z <- (1/(1+exp(-x)))*(1-(1/(1 + exp(-x))));
  abs.tol = 1e-6
  z[z == 1] <- 1 - abs.tol
  z[z == 0] <- 0 + abs.tol
  
  return (z);
  
}

runAll <- function(net, sample){
  #Function compute forward step 
  
  output.i <- list()
  output.i.active <- list()
  
  n.elements = dim(sample)[1]
  
  # output[[1]] is equal to the first input values
  output.i.active[[1]] <- sample 
  
  # for each layer except the input
  for (i in 2 : ( length(net) + 1) ) { 
    
    # add bias column to the previous matrix of activation functions
    output.i.active[[i-1]] = cbind( matrix(array(1, n.elements)), output.i.active[[i-1]] ); 
    
    # for all neurons in current layer multiply corresponds neurons
    # in previous layers by the appropriate weights and sum the productions
    output.i[[i]] <- (output.i.active[[i-1]] %*% t(net[[i-1]]))
    # apply activation function for each value
    output.i.active[[i]] <- activationFunc(output.i[[i]])
    
  }
  
  return (list(output.i, output.i.active))
  
}

costError <- function(output.last.active, labels) { 
  #Cost function, logloss metric
  logH <- log(output.last.active)
  #log1H <- 1 - logH
  
  #transpose labels for matrix multiplication
  costMatrix = -1* (t(labels) %*% logH); # - (1-t(labels)) %*% log1H 
  
  return (sum(costMatrix))
  
}


costError2 <- function(output.last.active, labels) { 
  #Cost function, simple rmse-like metric
  return ( sum( (output.last.active - labels)*(output.last.active - labels) ) )
  
}

costRegul <- function(net, lambda) { 
  #Cost of regularization 
  regValue = 0;
  
  n.params = 0;
  
  for (i in 1 : length(net)) { 
    
    cur.layer = net[[i]];
    cur.layer = cur.layer*cur.layer;
    regValue = regValue + sum(cur.layer[, 2:dim(cur.layer)[2]]);
    n.params = n.params + length(cur.layer[, 2:dim(cur.layer)[2]]);
    
  }
  
  return (lambda*regValue/n.params);
  
}


costTotal <- function(net, sample, labels, lambda) { 
  #Total cost function error and regularization 
  
  out.run <- runAll(net, sample)
  
  output.last.active = out.run[[2]][[length(net) + 1]]
  
  n.samples <- dim(sample)[1];
  cost.error <- costError2(output.last.active, labels);
  regul.val <- costRegul(net, lambda);
  
  return (cost.error/n.samples + regul.val)
}







backProp <- function(net, sample, labels, lambda) { 
  #Back propagation step
  
  length.net = length(net)
  net.delta <- net
  
  for(i in 1 : length.net ) {
    net.delta[[i]] <- net.delta[[i]]*0
  }
  
  n.samples <- dim(sample)[1];
  
  forward.out <- runAll(net, sample)
  forward.list <- forward.out[[1]]
  forward.active.list <- forward.out[[2]]
  forward.last.active = forward.out[[2]][[length.net + 1]]
  
  for (i.sample in 1 : n.samples ) {
    
    #fill list with zeros
    delta <- list()  
    
    #calculate delta of error of output layer
    delta[[length.net + 1]] <- ((forward.last.active[i.sample, ]) - (labels[i.sample, ]) ) 
    
    for (i in length.net : 1) {
      
      if (i > 1) { 
        cur.output <- forward.list[[i]][i.sample, ] #may be here minus one should be
        cur.output <- cbind( 1, t(cur.output), deparse.level = 0 ); 
        
        delta[[i]] <- t(net[[i]]) %*% (delta[[i + 1]]) * t(derActivationFunc(cur.output))
    
        delta[[i]] <- delta[[i]][2:length(delta[[i]])]
        
      }
      
      net.delta[[i]] <- net.delta[[i]] + matrix(delta[[i+1]]) %*% forward.active.list[[i]][i.sample, ]
    }
    
  }
  
  for (i in 1 : length(net.delta)){
    
    net.delta[[i]] <- net.delta[[i]]/n.samples

    #regularization
    net.delta[[i]][,2:dim(net.delta[[i]])[2]] <- 
      net.delta[[i]][,2:dim(net.delta[[i]])[2]] +
      net[[i]][,2:dim(net.delta[[i]])[2]] * (lambda/2/n.samples) 
    
  }
  
  return (net.delta)
  
}

saveNetCSV <- function(net, filename) { 
  #Saving net into files
  for (i in 1 : length(net)) { 
    filename.i = paste0(filename, "_l", i, ".csv")
    write.csv(net[[i]], filename.i)
  }
  return(0)
}

readNetCSV <- function(filename.base, n.layers) {
  #Reading net from files
  net <- list();
  for (i in 1 : n.layers) { 
    filename.i = paste0(filename.base, "_l", i, ".csv")
    net[[i]] <- read.csv(filename.i, row.names=1)
  }
  return(net)
}