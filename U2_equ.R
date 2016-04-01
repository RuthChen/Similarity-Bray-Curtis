U2_equ=function(X,Y){
  n=sum(X)
  m=sum(Y)
  f1.=sum(X==1 & Y>0)
  p1barhat_1=p1bar_equ(X)
  out3=f1./n*(1-p1barhat_1)
  #############################
  f11=sum(X==1 & Y==1)
  p1barhat_2=p1bar_equ(Y)
  out4=f11/n/m*(1-p1barhat_1)*(1-p1barhat_2)/p1barhat_2
  if(p1barhat_2==0){out4=0}
  output=out3+out4
  return(output) 
}