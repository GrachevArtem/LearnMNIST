library(pROC)
library(ROCR)

testModel <- function(net, data) {
  
  len.net <- length(net) + 1;
  out.net <- runAll(net, data);
  
  out.last.active <- out.net[[2]][[len.net]]
  
  predict.labels = max.col(out.last.active, "first") - 1;
  probs <- out.last.active / rowSums(out.last.active)
  
  return(list("labels" = predict.labels, "probs" = probs))
}

evaluate <- function(predicted.labels, probs, labels, i.class) {
  
  if (i.class == 0) { 
    labels[labels == i.class] <- -1
    labels[labels > i.class] <- 0
    labels[labels == -1] <- 1
    predicted.labels[predicted.labels == i.class] <- -1
    predicted.labels[predicted.labels > i.class] <- 0
    predicted.labels[predicted.labels == -1] <- 1
  } else { 
    labels[labels != i.class] <- 0
    labels[labels == i.class] <- 1
    predicted.labels[predicted.labels != i.class] <- 0
    predicted.labels[predicted.labels == i.class] <- 1
  }
  
  TP <- sum(labels[predicted.labels == 1])
  print(TP)
  TN <- sum(labels) - sum(labels[predicted.labels = 0])
  FN <- sum(labels[predicted.labels == 0])
  FP <- length(labels) - sum(labels) - sum(labels[predicted.labels = 1])
  
  #Acc = (TP + TN)/length(labels)
  TPR <- TP/(TP + FN)
  Rec <- TPR
  Sp <- TN/(TN + FP)
  Prec <- TP/(TP + FP)
  
  f.score <- 2*TP/(2*TP + FN + FP)
  
  FDR <- FP / (FP + TP)
  #Recall, precision, specificity, F-measure, FDR and ROC for each class separately. Use a package for RO
  
  cat("Recall: ", Rec, "\n")
  cat("Precision: ", Prec, "\n")
  cat("Specificity: ", Sp, "\n")
  cat("F-measure: ", f.score, "\n")
  cat("FDR: ", FDR, "\n")
    
  #calc Roc
  probs.i = probs[, i.class + 1]
  
  
  pred <- prediction(probs.i, labels)
  perf <- performance(pred,"tpr","fpr")
  # changing params for the ROC plot - width, etc
  par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
  # plotting the ROC curve
  plot(perf,col="black",lty=3, lwd=3)
  # calculating AUC
  auc <- performance(pred,"auc")
  # now converting S4 class to vector
  auc <- unlist(slot(auc, "y.values"))
  # adding min and max ROC AUC to the center of the plot
  minauc<-min(round(auc, digits = 2))
  maxauc<-max(round(auc, digits = 2))
  minauct <- paste(c("min(AUC)  = "),minauc,sep="")
  maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
  
  #use legend in connection with screen size
  #legend(0.8,0.4,c(minauct,"\n"),border="white",cex=0.1,box.col = "white")
  cat("AUC: ", auc, "\n")
}




