mle.fun = function(x, method=c("equal","unequal")){
  N = ncol(x)
  if ( method=="equal" ) w = rep(1/N, N)
  else w = apply(x, 2, function(v) sum(v)/sum(x) )
  
  n = apply(x,2,sum )
  pp = t(apply(x,1,function(v){v/n}))
  pp_pool = apply(pp,1,function(v) sum(v*w))
  ans = sum(abs(w*pp-pp_pool/N))/2/(1-1/N)
  return (1-ans)
}