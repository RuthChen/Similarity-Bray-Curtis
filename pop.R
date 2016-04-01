pop=function(p1,p2,S12){
  S1=sum(p1>0);S2=sum(p2>0)
  p=matrix(0,nrow = S1+S2-S12,ncol = 2)
  p[1:S1,1]=sort(p1[p1>0],decreasing=T)
  p[c(1:S12,(S1+1):(S1+S2-S12)),2]=sort(p2[p2>0],decreasing=T)
  p 
}