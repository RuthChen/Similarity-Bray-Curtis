true.fun = function(p, w){
  N = ncol(p)
  p_pooled = apply(p,1,function(v) sum(w*v))
  pp = sapply(1:N, function(i) w[i]*p[,i])
  ans = sum(abs(pp-p_pooled/N))/2/(1-1/N)
  return(1-ans)
}