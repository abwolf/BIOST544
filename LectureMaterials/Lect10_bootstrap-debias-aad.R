library(ggplot2)

calc.stat <- function(x){
  m <- median(x)
  return(mean(abs(x - m)))
}

do.one <- function(n){
  dat <- rnorm(n)
  return(calc.stat(dat))  
}

set.seed(1)
n <- 8
diffs <- replicate(1e4, do.one(n))

truth <- do.one(1e6)

ggplot(data.frame(diffs = diffs), aes(x = diffs, y=..density..)) + 
  geom_density() + 
  geom_vline(xintercept = truth, color = "red") +
  geom_vline(xintercept = mean(diffs), color = "black")

single.data <- rnorm(n)

do.one.boot <- function(data){
  data.re <- sample(data, replace = TRUE)
  return(calc.stat(data.re))
}

resamp.diffs <- replicate(1e4, do.one.boot(single.data))

ggplot(data.frame(r.diffs = resamp.diffs), aes(x = r.diffs, y=..density..)) + 
  geom_density() + 
  geom_vline(xintercept = calc.stat(single.data), color = "red") +
  geom_vline(xintercept = mean(resamp.diffs), color = "black")

