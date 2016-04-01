Bray_Curtis = function(p, R, SampleSize, method=c("equal","unequal")){
  
  N = ncol(p)
  x = lapply(1:N, function(i){ rmultinom(n=R, size=SampleSize[i], prob=p[,i]) })
  if ( method=="equal" ) w = rep(1/N, N)
  else w = SampleSize/sum(SampleSize)
  
  true = true.fun(p, w)
  
  # -------- #
  # estimate #
  # -------- #
  BC = sapply( 1:R, function(i){ 
    data = sapply(1:N, function(v) x[[v]][,i] )
    c(mle.fun.sim(data, method), Ma.fun.sim(data, method), est.fun.sim(data, method) )  })
  
  
  est = round(apply(BC[c(1,3,5),],1,mean),3)
  bias = round(colMeans(apply(BC[c(1,3,5),],1,function(v) v-true)),3)
  RMSE = round(sqrt(colMeans(apply(BC[c(1,3,5),],1,function(v) (v-true)^2))),3)
  boot.sd = round(apply(BC[c(2,4,6),],1,mean),3)
  sd = round(apply(BC[c(1,3,5),],1,sd),3)
  
  output = cbind(c(SampleSize[1],"",""), c(SampleSize[2],"",""), c(round(true,3),"",""), c("MLE","PROP2","PROP3"), est, bias,RMSE,sd,boot.sd)
  colnames(output) = c(paste0("n",c(1:N)),"TRUE","Estimator","Average estimate","Average bias","Sample RMSE","sd","boot.sd")
  
  return(output)
}