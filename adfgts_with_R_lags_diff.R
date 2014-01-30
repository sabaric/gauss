y <- ts(1:10)
c <- 1
j <-  5
# The while function executes only if both conditions are TRUE! 
while(j >= 0   ) { 
  
  if(c == 1) {
    x <- ts.intersect(ts(rep(1,length(y))),(lag(y,j)));x
  } else if (c == 2) {
    x <- ts.intersect(ts(rep(1,(length(y)-j))),ts(1:(length(y)-j)),lag(y,j));x
                    
        } else  
    x <- as.matrix(y_1[(j+1):n2])
  cat( "x1= ", x)
  i <- 1
  
  while(i <= j) {
    x <- cbind(x ,dy[( (j + 1) - i ):(n2-i)])
    i <- i +1
    cat( "x2= ", x)
  }
  j <- j-1
}
