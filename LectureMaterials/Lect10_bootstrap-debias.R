library(ggplot2)
library(dplyr)
library(data.table)

setwd("~/Dropbox/courses/dataScience/lectures/lecture10/code")

NOAH.clinical <- read.csv("../../../data/NOAH-data/clinical_data.csv", header = TRUE)[,-1]
NOAH.expression <- fread("../../../data/NOAH-data/expression_data_probeID.csv", header = TRUE, sep = ',')[,-1]

NOAH.expression$centerid <- as.numeric(NOAH.expression$centerid)
NOAH.expression$patid <- as.numeric(NOAH.expression$patid)

NOAH.clinical.use <- NOAH.clinical %>% select(centerid, patid, her2)
NOAH <- inner_join(NOAH.expression, NOAH.clinical.use, by=c("centerid","patid"))
NOAH.genes <- NOAH %>% select(-c(centerid, patid, her2))

calc.stat <- function(x, labs){
  m1 <- mean(x[labs == "HER2+"])
  m2 <- mean(x[labs == "HER2-"])
  sd <- sqrt(mean(c(abs(x[labs == "HER2+"] - m1),
                    abs(x[labs == "HER2-"] - m2))^2))
  return((m1-m2)/sd)
}


standardized.diff <- apply(NOAH.genes,
                       2,
                       calc.stat,
                       NOAH$her2)

calc.overopt.one.m <- function(exp.data, labs, standardized.diff){
  sample.inds <- sample(1:nrow(exp.data), replace = TRUE)
  new.standardized.diff <- apply(exp.data[sample.inds,],
                                 2,
                                 calc.stat,
                                 labs[sample.inds])
  ind.m <- which.max(new.standardized.diff)
  over.opt <- new.standardized.diff[ind.m] - standardized.diff[ind.m]

  return(over.opt)
}

calc.overopt.m <- function(exp.data, labs, standardized.diff, nsim = 10){
  over.opts <- replicate(nsim, calc.overopt.one.m(exp.data, labs, standardized.diff))
  avg.over.opt <- mean(over.opts)
  return(avg.over.opt)
}



set.seed(1)
(over.opt.m <- calc.overopt.m(NOAH.genes, NOAH$her2, standardized.diff))


calc.overopt.one <- function(exp.data, labs, standardized.diff){
  sample.inds <- sample(1:nrow(exp.data), replace = TRUE)
  new.standardized.diff <- apply(exp.data[sample.inds,],
                                 2,
                                 calc.stat,
                                 labs[sample.inds])
  ord <- order(new.standardized.diff, decreasing = TRUE)
  over.opt <- new.standardized.diff[ord] - standardized.diff[ord]

  return(over.opt)
}

calc.overopt <- function(exp.data, labs, standardized.diff, nsim = 10){
  over.opts <- replicate(nsim, calc.overopt.one(exp.data, labs, standardized.diff))
  avg.over.opt <- apply(over.opts, 1, mean)
  return(avg.over.opt)
}

set.seed(1)
(over.opt <- calc.overopt(NOAH.genes, NOAH$her2, standardized.diff))

