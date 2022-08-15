setwd("/home/josephluchman/github_misc/count-da-manu/count-da-manu/")

library(MASS)
library(pscl)
library(tidyverse)
library(magrittr)
library(domir)

# direct approach ----

dat <- mvrnorm(n = 1000, rep(0, times = 4), Sigma = diag(1, 4, 4)) |> 
  as.data.frame()

dat %<>% 
  mutate(
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02 + rnorm(1000, 0, sqrt(0.9492)),
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )


  ## model ----
glm(dv_pois ~ V1 + V2 + V3 + V4, data = dat, family = poisson()) %>% summary()

glm.nb(dv_nb ~ V1 + V2 + V3 + V4, data = dat) %>% summary()

glm(dv_nb ~ V1 + V2 + V3 + V4, data = dat, family = poisson()) %>% summary()

lm(dv_cont ~ V1 + V2 + V3 + V4, data = dat) %>% summary()

  ## domir w/metrics ----
domir(dv_pois ~ V1 + V2 + V3 + V4, 
      \(fml) {
        glm(fml, data = dat, family = poisson()) |> 
          predict(type = "response") |>
          cor(dat$dv_pois) |> (\(x) x**2)()
      }
)

# will have to make my own dev R2 function
domir(dv_pois ~ V1 + V2 + V3 + V4, 
      \(fml) {
        glm(fml, data = dat, family = poisson()) |> 
          performance::r2() |> pluck(1)
      }
)

domir(dv_nb ~ V1 + V2 + V3 + V4, 
      \(fml) {
        glm(fml, data = dat, family = poisson()) |> 
          performance::r2() |> pluck(1)
      }
)

domir(dv_nb ~ V1 + V2 + V3 + V4, 
      \(fml) {
        glm.nb(fml, data = dat) |> 
          performance::r2() |> pluck(1)
      }
)


# aggregation approach ----

dat2 <- mvrnorm(n = 1000, rep(0, times = 4), Sigma = diag(1, 4, 4)) |> 
  as.data.frame()

dat2 %<>% 
  mutate(
    obsvd = sample(5:10, 1000, replace = TRUE), # the offset - uncorr'd
    obsvd2 = qbinom(pnorm(V1*.5 + rnorm(1000, 0, sqrt(1-.5^2)), log.p = TRUE), 10, .7, log.p = TRUE), # offset - corr'd w/ V1
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02 + rnorm(1000, 0, sqrt(0.9492)),
    dv_pois = qbinom(pnorm(dv_cont, log.p = TRUE), 10, .1, log.p = TRUE),
    dv_pois_red = qbinom(pnorm(dv_cont, log.p = TRUE), obsvd, .1, log.p = TRUE), # w/ uncorr'd offset
    dv_pois_red2 = qbinom(pnorm(dv_cont, log.p = TRUE), obsvd2, .1, log.p = TRUE) # w/ corr'd offset
  )

  ## model w/ offsets ----
glm(dv_pois ~ V1 + V2 + V3 + V4, data = dat2, family = poisson()) %>% summary()

glm(dv_pois_red ~ V1 + V2 + V3 + V4, data = dat2, family = poisson()) %>% summary()

glm(dv_pois_red ~ V1 + V2 + V3 + V4 + offset(log(obsvd)), 
    data = dat2, family = poisson()) %>% 
  summary()  

glm(dv_pois_red2 ~ V1 + V2 + V3 + V4, 
    data = dat2, family = poisson()) %>% 
  summary()  

glm(dv_pois_red2 ~ V1 + V2 + V3 + V4 + offset(log(obsvd2)), 
    data = dat2, family = poisson()) %>% 
  summary()  
  



# finalize ----

  ## data gen ----

set.seed(84490856)

eq_v_uncor <- 
  mvrnorm(n = 10000, 
          mu = rep(0, times = 4), 
          Sigma = diag(1, 4, 4)) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02, 
    dv_cont = dv_cont + rnorm(10000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

eq_cov <- # v2 and v3 have too many strong and too few weak correlations
  c(1, .5^(1:3), .5, 1, .5^(1:2), .5^(2:1), 1, .5, .5^(3:1), 1) |> 
  matrix(nrow=4, byrow = TRUE) 

eq_v_pw_lw <- 
  mvrnorm(n = 10000, 
          mu = rep(0, times = 4), 
          Sigma = eq_cov) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02, 
    dv_cont = dv_cont + rnorm(10000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

pw_lw_v_uncor <- 
  mvrnorm(n = 10000, 
          mu = rep(0, times = 4), 
          Sigma = diag(1.27, 4, 4)^(1:4)) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02, 
    dv_cont = dv_cont + rnorm(10000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

pw_lw_cov <- # v2 and v3 have too many strong and too few weak correlations
  c(1.27, .5^(1:3)*sqrt(1.27^(2:4))*sqrt(1.27), 
    .5*sqrt(1.27)*sqrt(1.27^2), 1.27^2, .5^(1:2)*sqrt(1.27^2)*sqrt(1.27^(3:4)), 
    .5^(2:1)*sqrt(1.27^3)*sqrt(1.27^(1:2)), 1.27^3, .5*sqrt(1.27^3)*sqrt(1.27^4), 
    .5^(3:1)*sqrt(1.27^4)*sqrt(1.27^(1:3)), 1.27^4) |> 
  matrix(nrow=4, byrow = TRUE) 

pw_lw_v_pw_lw <- 
  mvrnorm(n = 10000, 
          mu = rep(0, times = 4), 
          Sigma = pw_lw_cov) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02, 
    dv_cont = dv_cont + rnorm(10000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

  ## model ----
lm(dv_cont ~ V1 + V2 + V3 + V4, data = eq_v_uncor) %>% summary()
lm(dv_cont ~ V1 + V2 + V3 + V4, data = eq_v_pw_lw) %>% summary()
lm(dv_cont ~ V1 + V2 + V3 + V4, data = pw_lw_v_uncor) %>% summary()
lm(dv_cont ~ V1 + V2 + V3 + V4, data = pw_lw_v_pw_lw) %>% summary()

glm(dv_pois ~ V1 + V2 + V3 + V4, data = eq_v_uncor, family = poisson()) %>% summary()
glm(dv_pois ~ V1 + V2 + V3 + V4, data = eq_v_pw_lw, family = poisson()) %>% summary()
glm(dv_pois ~ V1 + V2 + V3 + V4, data = pw_lw_v_uncor, family = poisson()) %>% summary()
glm(dv_pois ~ V1 + V2 + V3 + V4, data = pw_lw_v_pw_lw, family = poisson()) %>% summary()




# 
# 
# 
# 84490856
#Min: 1, Max: 99999999
#2022-08-14 14:50:02 UTC
#Random.org