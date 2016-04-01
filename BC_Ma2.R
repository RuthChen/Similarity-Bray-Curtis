BC_Ma2 = function(x, method=c("equal","unequal")){
  N = ncol(x)
  
  # -- weight --#
  if ( method=="equal" ) w = rep(1/N, N)
  else w = apply(x, 2, function(v) sum(v)/sum(x) )
  
  X1=x[,1]; X2=x[,2]
  w2=w[2]; w1=w[1]
  p1bar_1=p1bar_equ(X1)
  p1bar_2=p1bar_equ(X2)
  I=which(X1*X2>0)
  Y1=X1[I];Y2=X2[I]
  n1=sum(X1);n2=sum(X2)
  f.1=sum(X1>0 & X2==1)
  f.2=sum(X1>0 & X2==2);f.2=ifelse(f.2>0,f.2,1)
  f1.=sum(X1==1 & X2>0)
  f2.=sum(X1==2 & X2>0);f2.=ifelse(f2.>0,f2.,1)
  
  temp=0
  for(i in 1:length(I) )
  {
    a1=w1^2*ifelse(Y1[i]>1,Y1[i]*(Y1[i]-1)/n1/(n1-1),p1bar_1^2)-
      2*w1*w2*ifelse(Y1[i]>1,Y1[i]/n1,p1bar_1)*ifelse(Y2[i]>1,Y2[i]/n2,p1bar_2)+
      w2^2*ifelse(Y2[i]>1,Y2[i]*(Y2[i]-1)/n2/(n2-1),p1bar_2^2)
    a1=max(0,a1)
    if(a1==0)
    {
      a1=(abs(w1*ifelse(Y1[i]>1,Y1[i]/n1,p1bar_1)-w2*ifelse(Y2[i]>1,Y2[i]/n2,p1bar_2)))^2
    }
    temp=temp+a1^0.5
  }
  I=which(X1>0 & X2==1)
  Y1=X1[I];Y2=X2[I]
  if(length(I)>0)
  {
    for(i in 1:length(I) )
    {
      a1=w1^2*ifelse(Y1[i]>1,Y1[i]*(Y1[i]-1)/n1/(n1-1),p1bar_1^2)-
        2*w1*w2*ifelse(Y1[i]>1,Y1[i]/n1,p1bar_1)*p1bar_2+
        w2^2*p1bar_2^2
      a1=max(0,a1)
      if(a1==0)
      {
        a1=(abs(w1*ifelse(Y1[i]>1,Y1[i]/n1,p1bar_1)-w2*ifelse(Y2[i]>1,Y2[i]/n2,p1bar_2)))^2
        
      }
      temp=temp+a1^0.5*f.1/2/f.2 #(1-p1bar_2)/(n2*p1bar_2)
    }
  }
  I=which(X1==1 & X2>0)
  Y1=X1[I];Y2=X2[I]
  if(length(I)>0)
  {
    for(i in 1:length(I) )
    {
      a1=w2^2*ifelse(Y2[i]>1,Y2[i]*(Y2[i]-1)/n2/(n2-1),p1bar_2^2)-
        2*w1*w2*ifelse(Y2[i]>1,Y2[i]/n2,p1bar_2)*p1bar_1+
        w1^2*p1bar_1^2
      a1=max(0,a1)
      if(a1==0)
      {
        a1=(abs(w1*ifelse(Y1[i]>1,Y1[i]/n1,p1bar_1)-w2*ifelse(Y2[i]>1,Y2[i]/n2,p1bar_2)))^2
        
      }
      temp=temp+a1^0.5*f1./2/f2.#(1-p1bar_1)/(n1*p1bar_1)
    }
  }
  f11=sum(X1==1 & X2==1)
  sumtemp=f11*abs(w1*p1bar_1-w2*p1bar_2)*(1-p1bar_1)/(n1*p1bar_1)*(1-p1bar_2)/(n2*p1bar_2)
  if(p1bar_1==0 | p1bar_2==0)
  {
    sumtemp=0
  }
  temp=temp+sumtemp
  
  ##########################
  da1=X1  
  da2=X2
  I=which(da1*da2>0); sda1=da1[I];sda2=da2[I]
  
  
  U1=sum(sda1)/n1; V1=sum(sda2)/n2;
  ff1=sum(sda2==1);#ff1=ifelse(ff1==0, 1,ff1);
  ff2=sum(sda2==2);ff2=ifelse(ff2==0, 1,ff2);
  
  f1=sum(sda1==1);#f1=ifelse(f1==0, 1,f1);
  f2=sum(sda1==2);f2=ifelse(f2==0, 1,f2);  
  
  U2=(ff1/(2*ff2))*sum(sda1[sda2==1])/n1;uC1=f1/sum(sda1);
  V2=(f1/(2*f2))*sum(sda2[sda1==1])/n2;uC2=ff1/sum(sda2);
  U=U2+U1;U=min(U,1)
  V=V1+V2;V=min(V,1)
  ##########################
  mle=sum(abs(w1*X1/n1-w2*X2/n2))
  out=w1*(1-U)+w2*(1-V)+temp
  if(out<0 | out>1)
  {
    out=mle
  } 
  return(1-out)
}