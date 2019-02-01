# BEGIN CODE

# Question 1
my.kernel <- function(input_vector){
  # use vectorised if-condition
  output_vector <- ifelse(test = abs(input_vector) <= 1, yes = 35/32 * (1-input_vector^2)^3, no = 0)
  return(output_vector)
}

# Question 2
my.kernel.density.estimator <- function(y, sample, h){
  # compute density estimate
  density_estimate <- 1/(length(sample) * h) * sum(my.kernel((sample-y)/h))
  return(density_estimate)
}

# Question 3
my.loglik.cv <- function(sample, h){
  # create vector to hold log density of sample size
  cv_density <- sample
  # run for loop over vector to calculate density for each element, leaving one out
  for(i in 1:length(sample)){
    cv_density[i] <- log(my.kernel.density.estimator(sample[i],sample[-i],h))
  }
  # sum log density to get log-likelihood
  cv_likelihood <- sum(cv_density)
  return(cv_likelihood)
}

# END CODE