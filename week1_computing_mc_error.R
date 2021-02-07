set.seed(1431)

## write function for calculating se

mc_error <- function(m,a,b) {
  theta = rgamma(n=m,shape=a,rate=b)

  se = sd(theta) / sqrt(m)

  ## from this, we can calculate a confidence interval for our monte carlo estimate of the mean
  lower = mean(theta) - 2*se
  upper = mean(theta) + 2*se

  ci <- paste(round(lower,digits=3),', ',round(upper,digits=3))

  return(ci)
}


## set our distribution parameters
a = 2
b = 1/3

## lets see what error we get for m = 10000
m = 10000
mc_error(m,a,b)

## what about m = 1m?

## what happens to this range if we decrease m?
m = 1000000
mc_error(m,a,b)

## we can see our error is MUCH larger for m = 10000, but still a decent approximation
## however, there is nearly no error with m = 1000000
