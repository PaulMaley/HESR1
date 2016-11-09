# QQPlot example from the class
# Have a play

# Sample size
n <- 1000

# Make n random numbers ~N(0,1) and order them
xs <- sort(rnorm(n))

# Or a t-distribution
#xs <- sort(rt(n, df=5))

# Or a uniform distribution [-3,3]
#xs <- sort(runif(n,-3,3))

# Define quantiles q by k/n+1 for K=1,n
qs <- sapply(1:n, {function(x) x/(n+1)})

# Determine corresponding z values
zs <- qnorm(qs)

# If we use the uniform distribution
#zs <- qunif(qs,-3,3)

# Plot
plot(zs, xs)

# Compare with qqnorm function
layout(matrix(c(1,2),nrow=1))
plot(zs,xs, xlab = "Theoretical", 
     ylab = "Sample", main = "By hand")
qqnorm(xs)
lines(c(-3,3),c(-3,3))