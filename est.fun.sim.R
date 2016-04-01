est.fun.sim = function(x, method=c("equal","unequal")){
  out=BootstrapFunMa(x, 100, est.fun, method);out=round(out,4)
  return(out)
}