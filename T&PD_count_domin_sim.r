setwd("/home/josephluchman/github_misc/count-da-manu/count-da-manu/")

library(tidyverse)
library(magrittr)
library(domir)
library(MASS)

# direct approach ----

dat <- mvrnorm(n = 1000, rep(0, times = 4), Sigma = diag(1, 4, 4)) |> 
  as.data.frame()

dat %<>% 
  mutate(
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02 + rnorm(1000, 0, sqrt(0.9492)),
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE)
  )

glm(dv_pois ~ V1 + V2 + V3 + V4, data = dat, family = poisson()) %>% summary()

lm(dv_cont ~ V1 + V2 + V3 + V4, data = dat) %>% summary()


# aggregation approach ----

dat2 <- mvrnorm(n = 1000, rep(0, times = 4), Sigma = diag(1, 4, 4)) |> 
  as.data.frame()

dat2 %<>% 
  mutate(
    obsvd = sample(5:10, 1000, replace = TRUE), # the offset
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02 + rnorm(1000, 0, sqrt(0.9492)),
    dv_pois = qbinom(pnorm(dv_cont, log.p = TRUE), 10, .1, log.p = TRUE),
    dv_pois_red = qbinom(pnorm(dv_cont, log.p = TRUE), obsvd, .1, log.p = TRUE) # w/ offset
  )

glm(dv_pois_red ~ V1 + V2 + V3 + V4, data = dat2, family = poisson()) %>% summary()

glm(dv_pois_red ~ V1 + V2 + V3 + V4 + offset(log(obsvd)), 
    data = dat2, family = poisson()) %>% 
  summary()  
  