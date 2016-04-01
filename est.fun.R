est.fun = function(x, method=c("equal","unequal")){
  N = ncol(x)
  
  # -- weight --#
  if ( method=="equal" ) w = rep(1/N, N)
  else w = apply(x, 2, function(v) sum(v)/sum(x) )
  
  # -- aik
  a1_bar = apply(x, 2, p1bar_equ)  
  a = sapply(1:N, function(i){
    ifelse(x[,i]==1, a1_bar[i], x[,i]/sum(x[,i]))
  })
  a_squar = sapply(1:N, function(i){
    ifelse(x[,i]==1, a1_bar[i]^2, x[,i]*(x[,i]-1)/sum(x[,i])/(sum(x[,i])-1))
  })
  # -- pi(pooled data)
  p1_bar = sum(w*a1_bar)
  p = apply(a,1,function(v){sum(w*v)})
  p_squar = apply(a,1,function(v){sum(w*v)^2})
  temp = w^2*a_squar-2*w*a*p/N+p_squar/N^2
  n = apply(x,2,sum )
  pp = t(apply(x,1,function(v){v/n}))
  pp_pool = apply(pp,1,function(v) sum(v*w))
  mle = abs(w*pp-pp_pool/N)^2 
  tmp = ifelse(temp<=0, mle, temp)
  ans = sum(sqrt(tmp))/2/(1-1/N)
  if(ans>1 | ans<0 ) ans=1-mle.fun(x,method)
  return (1-ans)
}