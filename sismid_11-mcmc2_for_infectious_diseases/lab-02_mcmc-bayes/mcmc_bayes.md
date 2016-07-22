# mcmc_bayes
daniel  
July 21, 2016  


```r
rm(list = ls())

if (interactive()) {
    coding <- 'sismid_11-mcmc2_for_infectious_diseases/lab-02_mcmc-bayes/coding.R'
    mcmc <- 'sismid_11-mcmc2_for_infectious_diseases/lab-02_mcmc-bayes/mcmc-Markov.R'
    data_file <- 'sismid_11-mcmc2_for_infectious_diseases/lab-02_mcmc-bayes/data.txt'
} else {
    coding <- 'coding.R'
    mcmc <- 'mcmc-Markov.R'
    data_file <- 'data.txt'
}

source(coding)
source(mcmc)
```




# Excercise 1

Have a look at the function mcmc-Markov.R and make sure you understand what is going on.

# Excercise 2

Read in the data


```r
data <-  read.table(data_file, header = TRUE)
```

# Excercise 3

fit a non-Markovian model to the observed data where the infectious period is assumed to be Gamma distributed with parameters

# Excercise 4

Write a function in R which will draw samples from the posterior distribution of interest, π(β,γ,I|R)π(β,γ,I|R), using MCMC and by making use of the function in the file coding.R.
