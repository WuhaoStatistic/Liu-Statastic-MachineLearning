# Author: Mattias Villani, Stockholm and Linköping University, Sweden.

library(manipulate)

####################################################################
## Plotting the beta pdf
####################################################################

BetaPlot <- function(a,b){
  xGrid <- seq(0.001, 0.999, by=0.001)
  prior = dbeta(xGrid, a, b)
  maxDensity <- max(prior) # Use to make the y-axis high enough
  plot(xGrid, prior, type = 'l', lwd = 3, col = "blue", xlim <- c(0,1), ylim <- c(0, maxDensity), xlab = "theta", 
       ylab = 'Density', main = 'Beta(a,b) density')
}

manipulate(
  BetaPlot(a,b),
  a = slider(1, 10, step=1, initial = 2, label = "The hyperparameter a in Beta(a,b) density"),
  b = slider(1, 10, step=1, initial = 2, label = "The hyperparameter b in Beta(a,b) density")
)

####################################################################
## Plotting the prior-to-posterior mapping for the Bernoulli model.
####################################################################

BetaPriorPostPlot <- function(a,b,n,p){
  xGrid <- seq(0.001, 0.999, by=0.001)
  normalizedLikelihood = dbeta(xGrid, n*p+1, n*(1-p)+1)
  prior = dbeta(xGrid, a, b)
  posterior = dbeta(xGrid, a+n*p, b+n*(1-p))
  maxDensity <- max(normalizedLikelihood, prior, posterior) # Use to make the y-axis high enough
  plot(xGrid, normalizedLikelihood, type = 'l', lwd = 3, col = "blue", xlim <- c(0,1), ylim <- c(0, maxDensity), xlab = "theta", 
       ylab = 'Density', main = 'Bernoulli model - Beta(a,b) prior')
  lines(xGrid, posterior, lwd = 3, col = "red")
  lines(xGrid, prior, lwd = 3, col = "green")
  legend(x = 0.01, y = maxDensity*0.95, legend = c("Likelihood (normalized)", "Prior", "Posterior"), col = c("blue","green","red"), lwd = c(3,3,3), cex = 0.7)
}

manipulate(
  BetaPriorPostPlot(a,b,n,p),
  a = slider(1, 100, step=1, initial = 2, label = "The hyperparameter a in Beta(a,b) prior"),
  b = slider(1, 100, step=1, initial = 2, label = "The hyperparameter b in Beta(a,b) prior"),
  n = slider(1, 5000, step=1, initial = 4601, label = "The number of trials, n"),
  p = slider(0, 1, step=0.001, initial = 0.394, label = "Success proportion in the sample")
)


