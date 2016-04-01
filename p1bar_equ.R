p1bar_equ=function(X){
  n=sum(X)
  f1=sum(X==1)
  f2=sum(X==2)
  a=1
  if(f1>0 & f2>0)
  {
    a=2*f2/( (n-1)*f1+2*f2  )
  }
  if(f1>1 & f2==0)
  {
    a=2/( (n-1)*(f1-1)+2      )
  }
  if(f1==1 &  f2==0){a=1}
  if(f1==0){a=1}
  return(a)
}