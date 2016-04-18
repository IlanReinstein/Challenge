#Package Loading
require(stats)
#A function stream is created to return the difference M - L. It takes two inputs N and T. 
#The function loops T times through T different streams of numbers. 
#It registers T values for 'max' and T values for 'last' and calculates the product of the resulting vectors.
#From there on it returns M - L for the calculation of the standard deviation and the mean.
#Note: It could have been calculated directly without looping but the mean would be M-L and SD = 0.
stream <- function(t,N){
  M <- rep(0,t) #Initiate vector M
  L <- rep(0,t) #Initiate vector L
  for(i in 1:t){
    x <- c(runif(t,min = 1,max = 10))            #Stream of T random numbers
    #Extraction of N biggest numbers
    ndx <- order(x)[1:N]                         
    ndx <- order(x, decreasing = T)[1:N]
    m <- x[ndx]
    #Extraction of N last numbers
    l <- tail(x,N)
    #Updating of M and L vectors
    M[i] <- prod(m)
    L[i] <- prod(l)
    d <- M-L
  }
  return(d)
}

#Calculation of the function on the given values of T and N.
a <- stream(8,2)
sprintf("%.10f",mean(a))
sprintf("%.10f",sd(a))

b <- stream(32,4)
sprintf("%.10f",mean(b))
sprintf("%.10f",sd(b))



