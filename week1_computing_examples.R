set.seed(1431)

## define function for visualizing simulation
vis_simulation <- function(n,shape,rate) {

  ## sample from gamma
  theta = rgamma(n=m,shape=a,rate=b)

  ## plot histogram of simulated values
  view1 <- hist(theta,freq=FALSE)
  ## plot against emperical distribution
  view1 <- curve(dgamma(x,shape=a,rate=b),col='blue',add=TRUE)
  return(view1)
}

## define initial parameters
m = 100
a = 2
b = 1/3

## visualize using function above
vis_simulation(m,a,b)

## is the mean from the simulation close to the actual?
a/b
mean(rgamma(n=m,shape=a,rate=b)) ## it is not. we get a wild array of numbers. lets increase n

## looks like a pretty good approximation. what happens if we increase m?
m = 1000
vis_simulation(m,a,b)

## looks closer to the emperical. what about the mean ?
a/b
mean(rgamma(n=m,shape=a,rate=b)) ## we still get a lot of numbers far from the actual mean

## we can see we begin to get closer and closer to the emperical distribution. lets increase to 500k
m = 500000
vis_simulation(m,a,b)

a/b
mean(rgamma(n=m,shape=a,rate=b))
 ## now that we are at 500k, our mean from the simulated distribtuion is always within a few tenths of the actual!

 ## lets check 1m draws

 m = 1000000
 vis_simulation(m,a,b)

 a/b
 mean(rgamma(n=m,shape=a,rate=b))
## we now rarely ever get a value anywhere meaningfully far away from 6

## lets now compare the simulated variance from the hypothetical
## hypothetical variance is a/(b^2)
var = a/(b^2)
var(rgamma(n=m,shape=a,rate=b))
## we are always very close to the actual hypothetical value

## now, lets see how well the simulation models the probability that the true value of gamma is < 5
## emperically, we can calculate this with the following:
pgamma(q=5,shape=a,rate=b) ## roughly 50%

model <- rgamma(n=m,shape=a,rate=b)

sum(model < 5)/m

## nearly identical probabilities!

## finally, lets compare quantiles from the emperical distribution to the simulated
p90_simulation <- quantile(model,.9)
p90_emperical <- qgamma(p=0.9,shape=a,rate=b)

p90_simulation
p90_emperical

## we again find these values to be nearly identical for all practical purposes
