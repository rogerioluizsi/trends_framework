ALEPlot <-
  function(X, X.model, pred.fun, J, K = 50) {
    
    N = dim(X)[1]  #sample size
    d = dim(X)[2]  #number of predictor variables
    
    #find the vector of z values corresponding to the quantiles of X[,J]
    z= c(min(X[,J]), as.numeric(quantile(X[,J],seq(1/K,1,length.out=K), type=1)))  #vector of K+1 z values
    z = unique(z)  #necessary if X[,J] is discrete, in which case z could have repeated values 
    K = length(z)-1 #reset K to the number of unique quantile points
    uncentered = numeric(K)
    #group training rows into bins based on z
    a1=as.numeric(cut(X[,J], breaks=z, include.lowest=TRUE)) #N-length index vector indicating into which z-bin the training rows fall
    X1 = X
    X2 = X
    X1[,J] = z[a1]
    X2[,J] = z[a1+1]
    y.hat1 = pred.fun(X.model=X.model, newdata = X1)
    y.hat2 = pred.fun(X.model=X.model, newdata = X2)
    Delta=y.hat2-y.hat1  #N-length vector of individual local effect values
    Delta = as.numeric(tapply(Delta, a1, mean)) #K-length vector of averaged local effect values
    uncentered = c(0, cumsum(Delta)) #K+1 length vector
    #now vertically translate fJ, by subtracting its average (averaged across X[,J])
    b1 <- as.numeric(table(a1)) #frequency count of X[,J] values falling into z intervals
    average = sum((uncentered[1:K]+uncentered[2:(K+1)])/2*b1)/sum(b1)
    #print(average)
    fJ = uncentered - average
    x <- z
    #summary metrics
    mean = mean(uncentered)
    max <- uncentered[which.max(abs(uncentered))]
    mem = uncentered[13] # approximation of the first quartile
    std = sd(uncentered)
    sv = uncentered[25]
    #plot(x, fJ, type="l", xlab=paste("x_",J, " (", names(X)[J], ")", sep=""), ylab= paste("f_",J,"(x_",J,")", sep=""))  #end of else if (class(X[,J]) == "numeric" | class(X[,J]) == "integer") statement
    
    
    list(x.values=x, f.values = fJ, ales.values = ales, mean=mean, max=max, mem=mem, std=std, sv=sv)
  }
