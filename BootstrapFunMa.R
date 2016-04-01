BootstrapFunMa <-function(x, B, FunName, method=c("equal","unequal")) {
  if ( method=="equal" ) w = rep(1/2, 2)
  else w = c(0.4,0.6)
  
  x1=x[,1]; x2=x[,2]
  Est = FunName(cbind(x1,x2),method)
  n1 <- sum(x1); n2 <- sum(x2)
  z <- SortDataFun(x1, x2)
  z1 <- z[, 1]
  z2 <- z[, 2]
  newprob <- Two_com_correct_obspi(z1, z2)
  p1 <- newprob[,1]
  p2 <- newprob[,2]
  set.seed(123)
  X1 <- rmultinom(B, n1, p1)
  set.seed(123)
  X2 <- rmultinom(B, n2, p2)
  X <- rbind(X1, X2)
  
  se <- sd(apply(X, 2, function(x) {
    y <- matrix(x, ncol=2)
    y1 <- y[, 1]
    y2 <- y[, 2]
    #     y1 <- x[1 : length(p1)]
    #     y2 <- x[(length(p1) + 1) : (2 * length(p1))]
    FunName(cbind(y1,y2),method)
  }), na.rm=T)
  
  return(c(Est,se))
}