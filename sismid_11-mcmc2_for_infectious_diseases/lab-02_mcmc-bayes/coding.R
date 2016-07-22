#########################################################
# count the number of infectives just before time t.
#########################################################
count.no.inf <- function(infection.times, removal.times, t) {
  # individual -i- is infected just before time t, if this holds:  I_i < t < R_i
  sum(infection.times < t) - sum(removal.times < t)
}


###############################################################
# compute the integral \int_{t} S_t I_t dt using a double sum.
###############################################################
compute.total.pressure <- function(data, fs) {
  N <- nrow(data);  
  total <- 0;
  for (i in 1:fs) {
    for (j in 1:N) {
      total <- total + min(data$removal[i], data$infection[j]) - min(data$infection[i], data$infection[j])
    }
  }
  total
}
  
######################################################
# compute the log of the product \prod_{j!=1} I_{i_j}
######################################################
compute.log.prod <- function(data, infection.times, removal.times, fs) {

  # find the index of the initial infective.
  time.init.inf <- min(data$infection[1:fs])
  index.init.inf <- which(data$infection[1:fs]==time.init.inf)

  # create a vector to store the elements of the product
  log.prod.vec <- rep(NA, fs);
  
  for (i in 1:fs) {
    if (i!=index.init.inf) {
      log.prod.vec[i] <- log(count.no.inf(infection.times, removal.times, infection.times[i]))
    }
    else {
      log.prod.vec[i] <- 0
    }
  }
  sum(log.prod.vec)
}
    



      

