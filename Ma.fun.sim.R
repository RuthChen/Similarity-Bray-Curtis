Ma.fun.sim = function(x, method=c("equal","unequal")){
  out=BootstrapFunMa(x,100,BC_Ma2,method);out=round(out,4)
  return(out)
}