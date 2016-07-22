# this is a function to compute the final size
# m initial susceptibles
# a initial infectives
# alpha infection rate
# the length of the fixed infectious period
compute.conditional.prob <- function(m, j, a, alpha, c){
  if (j==0) {
    res <- (exp(-alpha*m*c))^a
  }
  else {

    part.one <- exp(-alpha*(m-j)*c)^(j+a)
    part.two <- choose(m, j)
    
    sum <- 0;
    for (k in 0:(j-1)) {      
      sum <- sum + choose(m-k, j-k)*compute.conditional.prob(m,k,a,alpha,c)/(exp(-alpha*(m-j)*c)^(k+a))
    }
    res <- part.one*(part.two - sum)
  }
  return(res)
}


# We wish to compute P(T = k), for k = 0, ..., n
# "n" is the size of the household
# "alpha" is the person to person infection rate
# "c" is the length of the (fixed) infectious period
compute.marginal.prob <- function(k, n, c, alpha, p) {  
  prob <- 0;  
  for (y in 0:n) {
    if (k >= y) {
      cond.prob <- compute.conditional.prob(n - y, k - y, y, alpha, c)
    }
    else {
      cond.prob <- 0
    }
    prob <- prob + cond.prob*dbinom(y, n, 1 - p)
  }
  prob
}



