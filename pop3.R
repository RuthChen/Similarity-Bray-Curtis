pop3=function(p1,p2,p3){
  p=matrix(0,nrow = 450,ncol = 3)
  p[c((1:60),(91:230)),1]=sort(p1[p1>0],decreasing=T)
  p[c((1:90),(231:340)),2]=sort(p2[p2>0],decreasing=T)
  p[c((1:30),(61:120),(341:450)),3]=sort(p3[p3>0],decreasing=T)
  p
}