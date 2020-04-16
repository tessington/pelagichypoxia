#' inv.logit
#'@description
#'calculate the inverse logit of a real number
#'
#' @param x a real number, or vector of numbers
#'
#' @return
#'  exp(x) / (1 + exp(x))
#'
#'
#' @export
#'
#' @examples
#'
inv.logit <- function (x) exp(x) / (1 + exp(x))

#' logit
#'@description
#'calculate the logit of a probability, p
#'
#' @param x a real number, or vector of numbers between 0 and 1
#'
#' @return
#'  log(p/(1-p))
#'
#'
#' @export
#'
#' @examples
#'
logit <- function(x) log( x / (1 - x))

#' logit.mod.se
#'@description
#' calculate standard error of a logit transform
#'
#' @param x the standard error of non transformed data
#'
#' @return
#'   1 / (x * (1 - x))
#'
#'
#' @export
#'
#' @examples
#'

logit.mod.se <- function(x) 1 / (x * (1 - x))


#' calc.ci
#'@description
#'calculate the X% credibility interval from kernel-smoothed MCMC output
#'
#' @param x the parameter values
#' @param y the probability densities
#' @param value what percentile range should the credibility interval span (usually 0.8)
#'
#' @return
#'  c(lower, upper, lower density, upper density)
#'
#' @export
#'
#' @examples
#'
get.ci <- function(x, y, value) {
  # get interval approximation
  probs <- y * (x[2] - x[1])
  # get cum probs
  cumprob <- cumsum(probs) / sum(probs)
  # find the lower 5th percentile
  lower.ci <- approx(x = cumprob, y = x,  xout = (1- value)/2)
  upper.ci <- approx(x = cumprob, y = x,  xout = 1 - (1- value)/2)
  lower.index <- 1:which.max(probs)
  upper.index <- which.max(probs):length(probs)
  lower.ci.dens <- approx(x = x[lower.index], y = y[lower.index], xout = lower.ci$y)
  upper.ci.dens <- approx(x = x[upper.index], y = y[upper.index], xout = upper.ci$y)
  return(c(lower.ci$y, upper.ci$y, lower.ci.dens$y, upper.ci.dens$y))

}



#' calc.se
#'@description
#'calculate the standard error from CTD casts
#'
#' @param x the vector of CTD data e.g. oxygen) by depth bins
#' @param t.max the maximum autocorrelation interval to use, should be sqrt(length(x))
#'
#' @return
#'  standard error of the mean
#'
#' @export
#'
#' @examples
#'
calc.se <- function(x, t.max) {
  xbar <- mean(x)
  n <- length(x)
  ct <- rep(NA, times = t.max+1)

  for (t in 0:t.max){
    sums = 0
    for (k in 1:(n-t)) sums = sums + (x[k]-xbar) * (x[k+t] - xbar)
    ct[t+1] <- sums / (n - t)
  }

  alpha <- (n - 1:t.max)/n # these are the terms that get multiplied by the ct terms
  var.hat <- 1/n * (ct[1] + 2 * sum(alpha * ct[2:(t.max+1)]))
  SE <- sqrt(var.hat)
  return(SE)
}


#' fixed.effect
#'@description
#'calculate the average (and SE) of multiple means (x1, x2,...xn) each with corresponding standard error
#'
#' @param x the vector of means
#' @param se the vector of standard errors
#'
#' @return
#'  fixed effect mean and standard error: c(mean, standard error)
#'
#' @export
#'
#' @examples
#'

fixed.effect<-function(x,se) {
  n.samples <- length(x)
  var <- se^2
  tau <- 1/ se^2
  meta.mean <- sum(x * tau) / sum(tau)
  meta.var <- (sum(tau))^(-1)
  meta.se <- sqrt(meta.var)
  return(c(meta.mean, meta.se))
}

#' get.percentile
#'@description
#' finds a given percentile from kernel-smoothed MCMC chains
#'
#' @param x the vector parameter values
#' @param y the vector probability densities
#' @param value the percentile to return (between 0 and 1)
#'
#' @return
#'  the estimated percentile from the posterior probability
#'
#' @export
#'
#' @examples
#'
get.percentile <- function(x, y, value) {
  # get interval approximation
  probs <- y * (x[2] - x[1])
  # get cum probs
  cumprob <- rep(0, length(probs))
  cumprob[1] <- probs[1]
  for (i in 2:length(cumprob)) cumprob[i] <- sum(probs[1:i])
  # find the lower valut_th percentile
  percentile <- approx(x = cumprob, y = x,  xout = value)
  return(percentile$y)

}
