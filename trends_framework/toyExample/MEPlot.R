MPlot <-
  function(X, X.model, pred.fun, J, K) {
    
    N = dim(X)[1]  #sample size
    d = dim(X)[2]  #number of predictor variables
    z= c(min(X[,J]), as.numeric(quantile(X[,J],seq(1/K,1,length.out=K), type=1)))  #vector of K+1 z values
    z = unique(z)  #necessary if X[,J] is discrete, in which case z could have repeated values 
    #print(z)
    K = length(z) -1 #reset K to the number of unique quantile points
    uncentered = numeric(K)
    xmin = min(X[,J])
    xmax = max(X[,J])
    x = seq(xmin, xmax, length.out=K)
    #group training rows into bins based on z
    a1=as.numeric(cut(X[,J], breaks=z, include.lowest=TRUE)) #N-length index vector indicating into which z-bin the training rows fall
    #print(a1)
    X[, J] = z[a1]
    y.hat = pred.fun(X.model=X.model, newdata = X)
    uncentered = as.numeric(tapply(y.hat, a1, mean))
    #print(fJ)
    #now vertically translate fJ, by subtracting its average (averaged across X[,J])
    a<-cut(X[,J], breaks=c(xmin-(x[2]-x[1]),x), include.lowest=TRUE)
    b<- as.numeric(table(a1)) #frequency count vector of X[,J] values falling into x intervals
    average = sum(uncentered*b)/sum(b)
    fJ = uncentered - average 
    #summary metrics
    mean = mean(uncentered)
    max <- uncentered[which.max(abs(uncentered))]
    mem = uncentered[13] # approximation of the first quartile
    std = sd(uncentered)
    sv = uncentered[25]
    #plot(x, fJ, type="l", xlab = paste("x_",J, " (", names(X)[J], ")", sep=""), ylab = paste("f_",J,"(x_",J,")", sep=""))
    list(K=K, x.values=x, f.values = fJ,  mean=mean, max=max, mem=mem, std=std, sv=sv)    
  }
  


