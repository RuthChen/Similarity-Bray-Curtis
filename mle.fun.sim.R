mle.fun.sim = function(x, method=c("equal","unequal")){
  X1=x[,1]; X2=x[,2]
  out=BootstrapFunMa(x, 100, mle.fun, method);out=round(out,4)
  return(out)
}