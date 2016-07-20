################################################################################
# R-code to simulate from a Markov and Non-Markov Stochastic epidemic models
################################################################################


# This function assumes 1 initial infective and N-1 initially susceptibles
# Per-person infection rate is beta/N

simSIR.Markov <- function(N, beta, gamma) {

  # initial number of infectives and susceptibles;
  I <- 1
  S <- N-1;

  # recording time;
  t <- 0;
  times <- c(t);

  # a vector which records the type of event (1=infection, 2=removal)
  type <- c(1);

  while (I > 0) {

    # time to next event;
    t <- t + rexp(1, (beta/N)*I*S + gamma*I);
    times <- append(times, t);
    
    if (runif(1) < beta*S/(beta*S + N*gamma)) {
      # infection
      I <- I+1;
      S <- S-1;
      type <- append(type, 1);
    }
    else {
      #removal
      I <- I-1
      type <- append(type, 2);
    }
  }
  
  # record the final size , i.e. the number of initially susceptlbles who contracted the disease sometime during the epidemic.
  # record the times of events (infections/removals) as well as the type
    
  res <- list("t"=times, "type"=type);
  res
}




simSIR.Markov.alternative <- function(N, beta, gamma) {

  # initial number of infectives and susceptibles;
  I <- 1
  S <- N-1;

  # recording time;
  t <- 0;
  times <- c(t);

  # a vector which records the type of event (1=infection, 2=removal)
  type <- c(1);

  while (I > 0) {

    ############################################
    # simulate times to the next possible events
    ############################################
    
    # time to next infection
    if (S > 0) {
      t.next.infection <- t +  rexp(1, (beta/N)*I*S)
    }
    else {
      t.next.infection <- Inf;
    }
    
    # time to next removal    
    t.next.removal <- t + rexp(1, gamma*I)


    # check which of the two events happens first
    if (t.next.infection < t.next.removal) {
      # infection occurs
      I <- I+1;
      S <- S-1;
      type <- append(type, 1);
      times <- append(times, t.next.infection);
      t <- t.next.infection
    }
    else {
      #removal occurs
      I <- I-1
      times <- append(times, t.next.removal);
      type <- append(type, 2);
      t <- t.next.removal
    }
  }
  
  # record the final size , i.e. the number of initially susceptlbles who contracted the disease sometime during the epidemic.
  # record the times of events (infections/removals) as well as the type

  res <- list("t"=times, "type"=type);
  res
}





simSIR.Non.Markov.constant <- function(N, beta, k) {

  # initial number of infectives and susceptibles;
  I <- 1
  S <- N-1;

  # recording time;
  t <- 0;
  times <- c(t);

  # create a vector containing the removal times of all the current infectives.
  r <- k 

  # a vector which records the type of event (1=infection, 2=removal)
  type <- c(1);
  
  # a counter for labelling the individuals
  lambda <- 1;

  # a vector to store the labels
  labels <- c(1);

  while (I > 0) {

    ############################################
    # simulate times to the next possible events
    ############################################
    
    # time to next infection
    if (S > 0) {
      T  <- rexp(1, (beta/N)*I*S)
    }
    else {
      T <- Inf;
    }

    # time to next removal
    R <- min(r, na.rm=TRUE);
    
    # check which of the two events happens first
    if (t + T < R) {
      # infection occurs
      I <- I+1;
      S <- S-1;
      r <- append(r, t + T + k)
      type <- append(type, 1);
      times <- append(times, t + T);

      lambda <- lambda + 1;
      labels <- append(labels, lambda)
      t <- t + T
    }
    else {
      #removal occurs
      I <- I-1
      type <- append(type, 2);
      index.min.r <- which(min(r, na.rm=TRUE)==r)
      r[index.min.r] <- NA
      labels <- append(labels, index.min.r)
      times <- append(times, R);
      t <- R

      # update the vector of 
    }
  }
  
  # record the final size , i.e. the number of initially susceptlbles who contracted the disease sometime during the epidemic.
  # record the times of events (infections/removals) as well as the type
  
  res <- list("t"=times, "type"=type, "labels"=labels);
  res
}



simSIR.Non.Markov.gamma <- function(N, beta, gamma, delta) {

  # initial number of infectives and susceptibles;
  I <- 1
  S <- N-1;

  # recording time;
  t <- 0;
  times <- c(t);

  # create a vector containing the removal times of all the current infectives.
  k <- rgamma(1, gamma, delta)
  r <- k 

  # a vector which records the type of event (1=infection, 2=removal)
  type <- c(1);

  # a counter for labelling the individuals
  lambda <- 1;
  
  # a vector to store the labels
  labels <- c(1);

  while (I > 0) {

    ############################################
    # simulate times to the next possible events
    ############################################
    
    # time to next infection
    if (S > 0) {
      T  <- rexp(1, (beta/N)*I*S)
    }
    else {
      T <- Inf;
    }

    # time to next removal
    R <- min(r, na.rm=TRUE);
    
    # check which of the two events happens first
    if (t + T < R) {
      # infection occurs
      I <- I+1;
      S <- S-1;
      k <- rgamma(1, gamma, delta)
      r <- append(r, t + T + k)

      lambda <- lambda + 1;
      labels <- append(labels, lambda)
      type <- append(type, 1);
      times <- append(times, t + T);
      t <- t + T
    }
    else {
      #removal occurs
      I <- I-1
      type <- append(type, 2);
      index.min.r <- which(min(r, na.rm=TRUE)==r)
      r[index.min.r] <- NA
      labels <- append(labels, index.min.r)
      times <- append(times, R);
      t <- R      
    }
  }
  
  # record the final size , i.e. the number of initially susceptlbles who contracted the disease sometime during the epidemic.
  # record the times of events (infections/removals) as well as the type
  
  res <- list("t"=times, "type"=type, "labels"=labels);
  res
}

