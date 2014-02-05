
cpi = read.table("cpi.txt") # CPI data
nex = read.table("for_us.txt") # Nominal exchante rate data

# Naming the columns in our data sets

c_names = c("Aus", "Aut", "Bel", "Can", "Den", "Fin", "Fra", "Ger", "Gre", "Ire", "Ita", "Jap", "Net", "Nez", "Nor", "Por", "Spa", "Swe", "Swi", "UK") # Creating a vector of country names
colnames(nex) = c_names # Using the vector of names as the column names in the dataset of nominal exchange rates
colnames(cpi) = c(c_names, "US") # Using the vector of names as the column names in the dataset of cpi and adding "US" for the 21st column

# Computing the real exchange of the US

l_cpi = as.matrix(log(cpi)) # Log of CPI and converts into matrix
l_nex = as.matrix(log(1/nex)) # Log of the nominal exchange rate and converts into matrix
rex = l_nex + l_cpi[ , 1:20] - l_cpi[ , 21] # Computes the real exchange rate
y <- matrix(rex[1:20  ,2], nrow = 20, ncol =1)

adfabic <- function(y, pmax , c , crt ) {
  n1  <- nrow(y)
  y_1 <- matrix(y[1:n1-1])
  dy  <- matrix((y[2:n1] - y_1),nrow(y_1),1)
  n2  <- nrow(dy)  
  bic <- 100000
  j = pmax
  while(j >= 0){
    
    if(c == 1) {
      x <- cbind(matrix(1,nrow = (n2-j)),as.matrix(y_1[(j+1):n2]))
    } else if (c == 2) {
      x <- cbind( matrix(1,nrow = (n2-j)),matrix(1:(n2-j),nrow = (n2-j)),as.matrix(y_1[(j+1):n2]))
    } else  
      x <- as.matrix(y_1[(j+1):n2])
    
     x <- cbind(x ,embed(dy,j)[1:nrow(x), ]) ; print(x)
#     i <- 1
#     
#     while(i <= j) {
#       x <- cbind(x ,dy[( (j + 1) - i ):(n2-i)])
#       i <- i +1
#       print(x)
#     }
    b   <- solve(t(x)%*%x)%*%(t(x)%*%dy[(j+1):n2])
    rsd <- dy[(j+1):n2] - x%*%b
    non <- nrow(x)
    if( crt == 1 ) cn <- log(non)
    if( crt == 2 ) cn <- 2 
    ssq <- as.numeric(t(rsd)%*%rsd )
    sic <- log(ssq/non) + cn*(j + 1)/non
    vcb <- (ssq/non)*solve(t(x)%*%x)
    seb <- sqrt(diag(vcb))
    tv  <- b/seb
    if(sic <= bic) {
      bic <- sic; adf <- tv[1]; rho <- b; std <- seb; lag <- j 
    }
    j <- j-1
  }
  out <- list("lag" = lag, "adf" = adf, "rho" = rho, "std" = std)
  return(out)
}
adfabic(y,2,1, 2)