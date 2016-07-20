simSIR.Markov2 <- function(N, beta, gamma) {
  I <- 1
  S <- N - 1;
  t <- 0;
  times <- c(t);
  type <- c(1);
  while (I > 0) {
    t <- t + rexp(1, (beta/N)*I*S + gamma*I);
    times <- append(times, t);
    if (runif(1) < beta*S/(beta*S + N*gamma)) {
      # infection
      I <- I + 1;
      S <- S - 1;
      type <- append(type, 1);
    } else {
      #removal
      I <- I - 1
      type <- append(type, 2);
    }
  }
  res <- list("t" = times,
              "type" = type,
              'num_infected' = sum(type == 2),
              'num_events' = max(times));
  return(res)
}


simSIR.Non.Markov.gamma2 <- function(N, beta, gamma, delta) {

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
  res <- list("t"=times, "type"=type, "labels"=labels,
              'num_infected' = sum(type == 2),
              'num_events' = max(times));
  return(res)
}


simSIR.Non.Markov.weibull <- function(N, beta, gamma, delta, shape, scale) {

  # initial number of infectives and susceptibles;
  I <- 1
  S <- N-1;

  # recording time;
  t <- 0;
  times <- c(t);

  # create a vector containing the removal times of all the current infectives.
  k <- rweibull(1, shape, scale)
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

  res <- list("t" = times,
              "type" = type,
              "labels" = labels,
              'num_infected' = sum(type == 2),
              'num_events' = max(times));
  return(res)
}
