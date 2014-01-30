
rm(list = ls())
library(MASS)
library(CADFtest) 
set.seed(10)
y <- matrix(rnorm(100),100,1)


adfgts <- function(y, pmax , c , sig ) {
  n1  <- nrow(y)
  y_1 <- matrix(y[1:n1-1])
  dy  <- matrix((y[2:n1] - y_1),99,1)
  n2  <- nrow(dy)  
  tvb <- 0
  j = pmax
  #while(j >= 0){
      if(j < 0 || abs(tvb) > qnorm(1 - sig/200) ) {
    if(c == 1) {
      x <- cbind(as.matrix(y_1[(j+1):n2]), matrix(1,nrow = (n2-j)))
    } else if (c == 2) {
      x <- cbind(as.matrix(y_1[(j+1):n2]), matrix(1,nrow = (n2-j)),matrix(1:(n2-j),nrow = (n2-j)))
    } else  
      x <- as.matrix(y_1[(j+1):n2])
    
   i <- 1
  
        if(i > j) {
         cond <- (i <= j)
         xx <- dy[( (j + 1) - i ):(n2-i)]
          x <- cbind(x ,xx)
          i <- i +1
        }
    b   <- ginv(t(x)%*%x)%*%(t(x)%*%dy[(j+1):n2])
    rsd <- dy[(j+1):n2] - x%*%b
    ssq <- as.numeric(t(rsd)%*%rsd / (nrow(x) - ncol(x)))
    vcb <- ssq*ginv(t(x)%*%x)
    seb <- sqrt(diag(vcb))
    tv  <-c(b)/seb
    tvb <- tv[length(tv)]
    j <- j-1
      }
  out <- list("Beta" =b,"Standard Error" =seb,"T-value"=tv,"SSQ"=ssq,"VCB"=vcb)
  return(out)
}
adfgts(y, pmax = 2, c=1, sig = 5)
cad1 <-(CADFtest(y~1,type = "none", max.lag.y =2))


