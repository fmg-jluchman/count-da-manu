setwd("/home/josephluchman/github_misc/count-da-manu/count-da-manu/")

library(knitr)
library(kableExtra)
library(MASS)
library(pscl)
library(tidyverse)
library(magrittr)
library(domir)
library(performance)
library(parameters)
library(datawizard)

# self note - do the offsets next to be sure they aren't just weird!  Can they be brought into DA in a systematic and useful way?

# Prep ----

  ## data gen ----

set.seed(84490856)

    ### var = + uncor ----
eq_v_uncor <- 
  mvrnorm(n = 100000, 
          mu = rep(0, times = 4), 
          Sigma = diag(1, 4, 4)) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.15 + V3*.125 + V4*.1, 
    dv_cont = dv_cont + rnorm(100000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

    ### var = + cor ----
eq_cov <-
  c(1, .5^(1:3), 
    .5, 1, .5^(3:2), 
    .5^(2:3), 1, .5, 
    .5^(3:1), 1) |> 
  matrix(nrow=4, byrow = TRUE) 

eq_v_pw_lw <- 
  mvrnorm(n = 100000, 
          mu = rep(0, times = 4), 
          Sigma = eq_cov) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.15 + V3*.125 + V4*.1,  
    dv_cont = dv_cont + rnorm(100000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

    ### var \= + uncor ----
pw_lw_v_uncor <- 
  mvrnorm(n = 100000, 
          mu = rep(0, times = 4), 
          Sigma = diag(1, 4, 4)*c(1, 1.5, 2, 4)) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.15 + V3*.125 + V4*.1,  
    dv_cont = dv_cont + rnorm(100000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

      ### var \= + cor ----
pw_lw_cov <- 
  c(1, .5^(1:3)*sqrt(1*c(1.5, 2, 4))*sqrt(1), 
    .5*sqrt(1)*sqrt(1*1.5), 1.5, .5^(3:2)*sqrt(1*1.5)*sqrt(1*c(2, 4)), 
    .5^(2:3)*sqrt(1*2)*sqrt(1*c(1, 1.5)), 2, .5*sqrt(1*2)*sqrt(1*4), 
    .5^(3:1)*sqrt(1*4)*sqrt(1*c(1, 1.5, 2)), 4) |> 
  matrix(nrow=4, byrow = TRUE) 

pw_lw_v_pw_lw <- 
  mvrnorm(n = 100000, 
          mu = rep(0, times = 4), 
          Sigma = pw_lw_cov) |> 
  as.data.frame() %>% 
  mutate(
    dv_cont = V1*.2 + V2*.15 + V3*.125 + V4*.1, 
    dv_cont = dv_cont + rnorm(100000, 0, sqrt(1 - var(dv_cont))), 
    dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
    dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
  )

  ## modeling setup ----
data_list <- 
  list(
    quote(eq_v_uncor),
    quote(eq_v_pw_lw),
    quote(pw_lw_v_uncor),
    quote(pw_lw_v_pw_lw)
  )
  
  ## dev R2 ----
devR2 <- 
  function(model) {
    
    mod_sum <- model |> summary()
    
    return(1 - (mod_sum$deviance/mod_sum$null.deviance))
    
  }

# Describe ----
# 
# 

# Model ----
lms <- 
  map(data_list, 
      ~ lm(
        reformulate(str_c("V", 1:4), response = "dv_cont"),
        data = eval(.)))

compare_parameters(lms) 
sink("./includes/lm_mods.tex")
compare_parameters(lms) %>% 
  #select(-Component, -matches("CI")) %>%
  kbl("latex", booktabs = TRUE, digits = 5)
sink()

lpois <- 
  map(data_list, 
      ~ glm(
        reformulate(str_c("V", 1:4), response = "dv_pois"),
        data = eval(.), family = poisson()))

compare_parameters(pois)
sink("./includes/pois_mods.tex")
compare_parameters(pois) %>% 
  #select(-Component, -matches("CI")) %>%
  kbl("latex", booktabs = TRUE, digits = 5)
sink()

nb <- 
  map(data_list, 
      ~ glm.nb(
        reformulate(str_c("V", 1:4), response = "dv_nb"),
        data = eval(.)))

compare_parameters(nb)
sink("./includes/nb_mods.tex")
compare_parameters(nb) %>% 
  #select(-Component, -matches("CI")) %>%
  kbl("latex", booktabs = TRUE, digits = 5)
sink()
  

# lm(dv_cont ~ V1 + V2 + V3 + V4, data = eq_v_uncor) %>% summary()
# lm(dv_cont ~ V1 + V2 + V3 + V4, data = eq_v_pw_lw) %>% summary()
# lm(dv_cont ~ V1 + V2 + V3 + V4, data = pw_lw_v_uncor) %>% summary()
# lm(dv_cont ~ V1 + V2 + V3 + V4, data = pw_lw_v_pw_lw) %>% summary()
# 
# glm(dv_pois ~ V1 + V2 + V3 + V4, data = eq_v_uncor, family = poisson()) %>% summary()
# glm(dv_pois ~ V1 + V2 + V3 + V4, data = eq_v_pw_lw, family = poisson()) %>% summary()
# glm(dv_pois ~ V1 + V2 + V3 + V4, data = pw_lw_v_uncor, family = poisson()) %>% summary()
# glm(dv_pois ~ V1 + V2 + V3 + V4, data = pw_lw_v_pw_lw, family = poisson()) %>% summary()




# DA ----

  ## comparison mods ----
lms_ri <- 
  map(data_list, 
      ~ domir(
        reformulate(str_c("V", 1:4), response = "dv_cont"),
        \(fml, data) lm(fml, data = data) %>%
          r2 %>% pluck(1),
        data = eval(.)))

sapply(lms_ri, \(x) x$General_Dominance)
sapply(lms_ri, \(x) x$Value)
sapply(lms_ri, \(x) x$Standardized)

poi_ri <- 
  map(data_list, 
      ~ domir(
        reformulate(str_c("V", 1:4), response = "dv_pois"),
        \(fml, data) glm(fml, data = data, family = poisson()) %>%
          devR2(),
        data = eval(.)))

sapply(poi_ri, \(x) x$General_Dominance)
sapply(poi_ri, \(x) x$Value)
sapply(poi_ri, \(x) x$Standardized)

nb_ri <- 
  map(data_list, 
      ~ domir(
        reformulate(str_c("V", 1:4), response = "dv_nb"),
        \(fml, data) glm.nb(fml, data = data) %>%
          devR2(),
        data = eval(.)))

sapply(nb_ri, \(x) x$General_Dominance)
sapply(nb_ri, \(x) x$Value)
sapply(nb_ri, \(x) x$Standardized)

  ## wrong metric ----
poi_lmr2_ri <- 
  map(data_list, 
      ~ domir(
        reformulate(str_c("V", 1:4), response = "dv_pois"),
        \(fml, data) glm(fml, data = data, family = poisson()) %>%
          predict(type = "response") %>%
          bind_cols(data %>% select(dv_pois)) %>%
          cor %>% 
          extract(2,1) %>%
          raise_to_power(2),
        data = eval(.)))

sapply(poi_lmr2_ri, \(x) x$General_Dominance)
sapply(poi_lmr2_ri, \(x) x$Value)
sapply(poi_lmr2_ri, \(x) x$Standardized)

nb_lmr2_ri <- 
  map(data_list, 
      ~ domir(
        reformulate(str_c("V", 1:4), response = "dv_nb"),
        \(fml, data) glm(fml, data = data, family = poisson()) %>%
          predict(type = "response") %>%
          bind_cols(data %>% select(dv_nb)) %>%
          cor %>% 
          extract(2,1) %>%
          raise_to_power(2),
        data = eval(.)))

sapply(nb_lmr2_ri, \(x) x$General_Dominance)
sapply(nb_lmr2_ri, \(x) x$Value)
sapply(nb_lmr2_ri, \(x) x$Standardized)

save.image("./Data_Results/count_domin_manu.R")

# Kable results ----
fit_met_ri <-
  data.frame( 
    vars = 1:4,
    bind_cols(
      map(lms_ri, pluck, "General_Dominance")
    ) %>% set_names(str_c("Linear Model ", 1:4)), 
    bind_cols(
      map(poi_ri, pluck, "General_Dominance")
    ) %>% set_names(str_c("Poisson Model A ", 1:4)),
    bind_cols(
      map(poi_lmr2_ri, pluck, "General_Dominance")
    ) %>% set_names(str_c("Poisson Model B ", 1:4)),
    bind_cols(
      map(nb_ri, pluck, "General_Dominance")
    ) %>% set_names(str_c("Negative Binomial Model A ", 1:4)),
    bind_cols(
      map(nb_lmr2_ri, pluck, "General_Dominance")
    )%>% set_names(str_c("Negative Binomial Model B ", 1:4)),
    check.names = FALSE
  ) %>% 
  pivot_longer(matches("[1-4]$"), names_to = c(".value", "data"), names_pattern = "(.+) (.)$") %>%
  mutate(data = 
              case_when(data == 1 ~ "EU", 
                        data == 2 ~ "EC", 
                        data == 3 ~ "UU", 
                        data == 4 ~ "UC")) %>%
  arrange(data, vars)

# ~~ can I get the names to have linebreaks to squeeze the table down?
# %>% rename_with(~{ str_replace(., " Model", "\nModel")})

sink("./includes/ri_fits.tex")
fit_met_ri %>%
  kbl("latex", booktabs = TRUE, digits = 5)
sink()


# 
# 
# 
# 84490856
#Min: 1, Max: 99999999
#2022-08-14 14:50:02 UTC
#Random.org
#
#
#
## # direct approach ---
# 
# dat <- mvrnorm(n = 1000, rep(0, times = 4), Sigma = diag(1, 4, 4)) |> 
#   as.data.frame()
# 
# dat %<>% 
#   mutate(
#     dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02 + rnorm(1000, 0, sqrt(0.9492)),
#     dv_pois = qpois(pnorm(dv_cont, log.p = TRUE), 1, log.p = TRUE),
#     dv_nb = qnbinom(pnorm(dv_cont, log.p = TRUE), size = .5, mu = 1, log.p = TRUE),
#   )
# 
# 
#   ## model ---
# glm(dv_pois ~ V1 + V2 + V3 + V4, data = dat, family = poisson()) %>% summary()
# 
# glm.nb(dv_nb ~ V1 + V2 + V3 + V4, data = dat) %>% summary()
# 
# glm(dv_nb ~ V1 + V2 + V3 + V4, data = dat, family = poisson()) %>% summary()
# 
# lm(dv_cont ~ V1 + V2 + V3 + V4, data = dat) %>% summary()
# 
#   ## domir w/metrics ---
# domir(dv_pois ~ V1 + V2 + V3 + V4, 
#       \(fml) {
#         glm(fml, data = dat, family = poisson()) |> 
#           predict(type = "response") |>
#           cor(dat$dv_pois) |> (\(x) x**2)()
#       }
# )
# 
# # will have to make my own dev R2 function
# domir(dv_pois ~ V1 + V2 + V3 + V4, 
#       \(fml) {
#         glm(fml, data = dat, family = poisson()) |> 
#           performance::r2() |> pluck(1)
#       }
# )
# 
# domir(dv_nb ~ V1 + V2 + V3 + V4, 
#       \(fml) {
#         glm(fml, data = dat, family = poisson()) |> 
#           performance::r2() |> pluck(1)
#       }
# )
# 
# domir(dv_nb ~ V1 + V2 + V3 + V4, 
#       \(fml) {
#         glm.nb(fml, data = dat) |> 
#           performance::r2() |> pluck(1)
#       }
# )
# 
# 
# # aggregation approach ---
# 
# dat2 <- mvrnorm(n = 1000, rep(0, times = 4), Sigma = diag(1, 4, 4)) |> 
#   as.data.frame()
# 
# dat2 %<>% 
#   mutate(
#     obsvd = sample(5:10, 1000, replace = TRUE), # the offset - uncorr'd
#     obsvd2 = qbinom(pnorm(V1*.5 + rnorm(1000, 0, sqrt(1-.5^2)), log.p = TRUE), 10, .7, log.p = TRUE), # offset - corr'd w/ V1
#     dv_cont = V1*.2 + V2*.1 + V3*.02 + V4*-.02 + rnorm(1000, 0, sqrt(0.9492)),
#     dv_pois = qbinom(pnorm(dv_cont, log.p = TRUE), 10, .1, log.p = TRUE),
#     dv_pois_red = qbinom(pnorm(dv_cont, log.p = TRUE), obsvd, .1, log.p = TRUE), # w/ uncorr'd offset
#     dv_pois_red2 = qbinom(pnorm(dv_cont, log.p = TRUE), obsvd2, .1, log.p = TRUE) # w/ corr'd offset
#   )
# 
#   ## model w/ offsets ---
# glm(dv_pois ~ V1 + V2 + V3 + V4, data = dat2, family = poisson()) %>% summary()
# 
# glm(dv_pois_red ~ V1 + V2 + V3 + V4, data = dat2, family = poisson()) %>% summary()
# 
# glm(dv_pois_red ~ V1 + V2 + V3 + V4 + offset(log(obsvd)), 
#     data = dat2, family = poisson()) %>% 
#   summary()  
# 
# glm(dv_pois_red2 ~ V1 + V2 + V3 + V4, 
#     data = dat2, family = poisson()) %>% 
#   summary()  
# 
# glm(dv_pois_red2 ~ V1 + V2 + V3 + V4 + offset(log(obsvd2)), 
#     data = dat2, family = poisson()) %>% 
#   summary()  
#   
# 