PDPlot <-
  function(X, X.model, pred.fun, J, K) {
    
    N = dim(X)[1]  #sample size
    d = dim(X)[2]  #number of predictor variables
    
    uncentered = numeric(K)
    xmin = min(X[,J])
    xmax = max(X[,J])
    x = seq(xmin, xmax, length.out=K)
    for (k in 1:K) {
      X.predict = X
      X.predict[,J] = x[k]
      y.hat = pred.fun(X.model=X.model, newdata = X.predict)
      uncentered[k] = mean(y.hat)
    }  #end of for loop
    #now vertically translate fJ, by subtracting its average (averaged across X[,J])
    a<-cut(X[,J], breaks=c(xmin-(x[2]-x[1]),x), include.lowest=TRUE)
    #print(a)
    b<- as.numeric(table(a)) #frequency count vector of X[,J] values falling into x intervals
    average = sum(uncentered*b)/sum(b)
    fJ = uncentered - average 
    
    #summary metrics
    mean = mean(uncentered)
    max <- uncentered[which.max(abs(uncentered))]
    mem = uncentered[13] # approximation of the first quartile
    std = sd(uncentered)
    sv = uncentered[25]
    
    #plot(x, fJ, type="l", xlab = paste("x_",J, " (", names(X)[J], ")", sep=""), ylab = paste("f_",J,"(x_",J,")", sep=""))
    list(x.values=x, f.values = fJ, mean=mean, max=max, mem=mem, std=std, sv=sv)    
  }
