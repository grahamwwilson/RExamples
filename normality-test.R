#!/usr/bin/env Rscript

source("normality_tests.R")

set.seed(128)
#x <- rnorm(100, mean=0.0, sd=1.0)
x <- sn::rsn(100, alpha=2.5)
tests <- normality_tests(x)
tests

