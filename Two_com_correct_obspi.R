Two_com_correct_obspi=function(X1,X2){
  n1=sum(X1)
  n2=sum(X2)
  f11=sum(X1==1)
  f12=sum(X1==2)
  f21=sum(X2==1)
  f22=sum(X2==2)
  C1=Chat.Ind(X1,n1)
  C2=Chat.Ind(X2,n2)
  
  PP1=correct_obspi(X1)
  PP2=correct_obspi(X2)
  D12=which(X1>0 & X2>0)
  
  f0hat_1=ceiling( (n1-1)/n1* ifelse(f12 == 0,  f11 * (f11 - 1) / 2,  f11 ^ 2/ 2 / f12)   )   
  f0hat_2=ceiling( (n2-1)/n2* ifelse(f22 == 0,  f21 * (f21 - 1) / 2,  f21 ^ 2/ 2 / f22)   )
  #-----------------------------------------------------------------------------
  
  r1=which(X1>0 & X2==0)
  f.1=length(which(X1>0 & X2==1))
  f.2=length(which(X1>0 & X2==2))
  f.0=ceiling((n2-1)/n2* ifelse(f.2>0,f.1^2/2/f.2,f.1*(f.1-1)/2))
  #------------------------------------------------------------------------------
  r2=which(X1==0 & X2>0)
  f1.=length(which(X1==1 & X2>0))
  f2.=length(which(X1==2 & X2>0))
  f0.=ceiling((n1-1)/n1*ifelse(f2.>0,f1.^2/2/f2.,f1.*(f1.-1)/2))
  #------------------------------------------------------------------------------
  t11=length(which(X1==1 & X2==1))
  t22=length(which(X1==2 & X2==2))
  f00hat=ceiling((n1-1)/n1*(n2-1)/n2* ifelse(t22 == 0,  t11 * (t11 - 1) / 4,  t11 ^ 2/ 4 / t22)   )
  #------------------------------------------------------------------------------
  temp1=max(length(r1),f.0)-length(r1)+f0.+f00hat
  temp2=max(length(r2),f0.)-length(r2)+f.0+f00hat
  p1_us_sum=min(U2_equ(X1,X2),1-C1)
  p1_us=p1_us_sum/temp1
  p2_us_sum=min(U2_equ(X2,X1),1-C2)
  p2_us=p2_us_sum/temp2
  if(f0hat_1-temp1>0){p0_1= (1-C1-p1_us_sum)/(f0hat_1-temp1) }
  if(f0hat_1-temp1<=0){p0_1=0} 
  if(f0hat_2-temp2>0){p0_2= (1-C2-p2_us_sum)/(f0hat_2-temp2) }
  if(f0hat_2-temp2<=0){p0_2=0}
  #------------------------------------------------------------------------------
  P1=PP1[D12]
  P2=PP2[D12]
  if(length(r1)> f.0)
  {
    P1=c(P1,PP1[r1])
    Y=c(rep(p2_us, f.0), rep(0,length(r1)-f.0))
    P2=c(P2,sample(Y,length(Y)) )
  }
  if(length(r1)< f.0)
  {
    P1=c(P1,PP1[r1],rep( p1_us,f.0-length(r1)))
    P2=c(P2,rep(p2_us, f.0) )
  }
  #----------------------------------------------------------------------------   
  if(length(r2)> f0.)
  {
    Y=c(rep(p1_us,f0.),rep(0,length(r2)- f0.))
    P1=c(P1,sample(Y,length(Y)))
    P2=c(P2,PP2[r2] )
  }
  if(length(r2)< f0.)
  {
    P1=c(P1,rep(p1_us,f0.))
    P2=c(P2,PP2[r2],rep( p2_us,f0.-length(r2)) )
  }
  P1=c(P1,rep( p1_us,f00hat))
  P2=c(P2,rep( p2_us,f00hat))
  P1=c(P1, rep(   p0_1,max( f0hat_1-temp1,0)) , rep(    0  ,max( f0hat_2-temp2,0))         )
  P2=c(P2, rep(     0 ,max( f0hat_1-temp1,0)) , rep(   p0_2,max( f0hat_2-temp2,0))         ) 
  #------------------------------------------------------------------------------------
  a=cbind(P1,P2)
  return(a)
}