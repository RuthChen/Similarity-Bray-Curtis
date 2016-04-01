correct_obspi<- function(X){
  Sobs <- sum(X > 0) 	
  n <- sum(X)		  	
  f1 <- sum(X == 1) 	
  f2 <- sum(X == 2)
  if(f1>0 & f2>0)
  {
    a=(n - 1) * f1 / ((n - 1) * f1 + 2 * f2) * f1 / n
  }
  if(f1>1 & f2==0)
  {
    a=(n-1)*(f1-1) / ( (n-1)*(f1-1) + 2 )*f1/n
  } 
  if(f1==1 & f2==0) {a=0}
  if(f1==0 ) {a=0} 	
  b <- sum(X / n * (1 - X / n) ^ n)
  w <- a / b  			
  Prob.hat <- X / n * (1 - w * (1 - X / n) ^ n)	
  Prob.hat
}