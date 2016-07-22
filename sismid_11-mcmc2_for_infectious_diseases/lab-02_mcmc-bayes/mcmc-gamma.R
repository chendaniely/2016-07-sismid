accept_reject_infection <- function(log.q.ratio, log.pi.can, log.pi.cur,
                                    log.prod.can,
                                    inf.time.cur,
                                    double.sum.can, cur_vals){
    u <- runif(1)

    if (log(u) < log.pi.can - log.pi.cur + log.q.ratio) {
        cur_vals$log.prod.cur <- log.prod.can
        double.sum.cur <- double.sum.can
        cur_vals$sum.R.minus.I.cur <- sum.R.minus.I.can
    } else {
        data$infection[choose.ind] <- inf.time.cur;
        infection.times[choose.ind] <- inf.time.cur;
    }
}

update_infection <- function(fs, data, N,
                             infection.times, removal.times,
                             cur_vals) {
    choose.ind <- sample(1:fs, 1)
    inf.time.cur <- infection.times[choose.ind]
    inf.time.can <- removal.times[choose.ind] - rexp(1, cur_vals$gamma.cur)

    data$infection[choose.ind] <- inf.time.can
    infection.times[choose.ind] <- inf.time.can

    log.prod.can <- compute.log.prod(data, infection.times, removal.times, fs)

    if (log.prod.can != -Inf) {
      double.sum.can <- compute.total.pressure(data, fs)
      sum.R.minus.I.can <- sum(removal.times - infection.times)

      log.q.ratio <- (log(dexp(removal.times[choose.ind] - inf.time.cur,
                              cur_vals$gamma.cur)) -
                      log(dexp(removal.times[choose.ind] - inf.time.can,
                               cur_vals$gamma.cur)))

      log.pi.can <- log.prod.can -
          (cur_vals$beta.cur / N) * double.sum.can -
          cur_vals$gamma.cur * sum.R.minus.I.can

      log.pi.cur <- cur_vals$log.prod.cur -
          (cur_vals$beta.cur / N) * cur_vals$double.sum.cur -
          cur_vals$gamma.cur * cur_vals$sum.R.minus.I.cur

      accept_reject_infection(log.q.ratio, log.pi.can, log.pi.cur,
                              log.prod,can,
                              inf.time.cur,
                              double.sum.can, cur_vals)
    } else {
        data$infection[choose.ind] <- inf.time.cur
        infection.times[choose.ind] <- inf.time.cur
    }
}

update_beta <- function() {

}

update_gamma <- function() {

}

compute_current_values <- function(data, fs, N,
                                   infection_times, removal_times,
                                   lambda.beta, nu.beta,
                                   lambda.gamma, nu.gamma) {

    double.sum.cur <- compute.total.pressure(data, fs)

    log.prod.cur <- compute.log.prod(data,
                                     infection_times,
                                     removal_times, fs)

    sum.R.minus.I.cur <- sum(removal_times - infection_times)

    beta.cur <- (rgamma(1, fs - 1 + lambda.beta, 1.0) /
                 (nu.beta + double.sum.cur/N))

    gamma.cur <- (rgamma(1, fs + lambda.gamma, 1.0) /
                  (nu.gamma + sum.R.minus.I.cur))

    return(list('double.sum.cur' = double.sum.cur,
                'log.prod.cur' = log.prod.cur,
                'sum.R.minus.I.cur' = sum.R.minus.I.cur,
                'beta.cur' = beta.cur,
                'gamma.cur' = gamma.cur))
}

setup_data <- function(data, fs, infection_times, N) {
    data$infection <- c(infection_times, rep(Inf, N - fs))
    data <- data[, c(1,3,2)] # re order
    return(data)
}

setup_res <- function(iter, beta.cur, gamma.cur, infection.times) {
    res <- matrix(NA, nrow=iter, ncol=3)
    res[1,] <- c(beta.cur, gamma.cur, sum(infection.times))
    return(res)
}

mcmcSIR.gamma <- function(data, iter,
                          lambda.beta = 1.0,
                          nu.beta = 10^(-3),
                          lambda.gamma = 1.0,
                          nu.gamma = 10^(-3)) {
    N <- nrow(data)
    fs <- sum(data$removal != Inf)
    removal.times <- data$removal[1:fs]
    infection_times <- seq(0, min(removal.times), len = fs)

    data <- setup_data(data, fs, infection_times, N)

    cur_vals <- compute_current_values(data, fs, N,
                                       infection_times, removal.times,
                                       lambda.beta, nu.beta,
                                       lambda.gamma, nu.gamma)

    res <- setup_res(iter, cur_vals$beta.cur, cur_vals$gamma.cur,
                     infection_times)

    for (i in 2:iter) {

        update_infection(fs, data, N, infection_times, removal.times, cur_vals)
        ## cur_vals$beta.cur <- update_beta()
        ## cur_vals$gamma.cur <- update_gamma()

        ## res[i, ] <- c(cur_vals$beta.cur,
        ## cur_vals$gamma.cur,
        ## sum(infection_times))
    }

    return(res)
}
