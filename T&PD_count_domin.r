setwd("/home/josephluchman/github_misc/count-da-manu/count-da-manu/")

library(tidyverse)
library(magrittr)
library(domir)
library(MASS)

# revisit data management here - has this been looked over?

nlsORM79 <- readRDS("nls79_masterORM.rds")

# data management ----

nlsORM79 %<>%
  mutate(unemp_flag = 
           case_when(
             emp_stat == "Unemployed" ~ 1,
             TRUE ~ 0
           ),
         not_lab_fce_flag = 
           case_when(
             emp_stat == "Out of Workforce" ~ 1,
             TRUE ~ 0
           ),
         emp_valid = 
           case_when(
             emp_stat != "Missing" ~ 1,
             TRUE ~ 0
           ))

nlsORM79_reduced <- 
  nlsORM79 %>% 
  group_by(ID) %>% 
  summarise(unemp_ct = sum(unemp_flag),
            nlf_ct = sum(not_lab_fce_flag),
            valid = log(sum(emp_valid)),
            AFQT = first(AFQT),
            LOC = first(LOC) %>% as.numeric,
            GENDER = first(GENDER)) %>% 
  drop_na()

# linear model comparison ----

lm_standard <- 
  lm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER, 
     data = nlsORM79_reduced)

lm_offset <- 
  lm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
     data = nlsORM79_reduced)

loglin_nml_offset <- # needs starting values...
  glm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
     data = nlsORM79_reduced, 
     family = gaussian(link = "log"), start = coef(lm_offset))

poisson_no_offset <- 
glm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER, 
    data = nlsORM79_reduced, 
    family = poisson()) #%>% summary

poisson_offset <- 
  glm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
      data = nlsORM79_reduced, 
      family = poisson()) %>% summary

# DA ----
  ## dev R2 ----
devR2 <- 
  function(model) {
    
    mod_sum <- model |> summary()
    
    return(1 - (mod_sum$deviance/mod_sum$null.deviance))
    
  }

## Poisson with deviance R2 ----

domir(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
      \(fml) {
        (res <- glm(fml, # %>% update(. ~ . + offset(valid)), 
            data = nlsORM79_reduced, 
            family = poisson()) %T>% {coef(.) %>% print} %>% 
          devR2)
        return(res)
      },
      .adj = ~ offset(valid)) #%>% summary  <- don't think .adj factor is being included!

## Poisson with Negelkerke (no offset) ----

domir(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER, 
      \(fml) {
        (res <- glm(fml, 
                    data = nlsORM79_reduced, 
                    family = poisson()) %>% 
           performance::r2() %>% pluck("R2_Nagelkerke")) %>% 
          capture.output()
        return(res)
      }) 

## Gaussian glm not log linked ----

domir(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
      \(fml) {
        (res <- glm(fml, 
                    data = nlsORM79_reduced) %>%
           pscl::pR2() %>% pluck("McFadden")) %>% 
          capture.output()
        #AIC())
        return(res)
      },
      .adj = ~ offset(valid)) #%>% summary
#.adj = ~ 1)
  

# stata code ----

# generate une_flag = cond(emp_stat == 2, 1, .)
# 
# generate nlf_flag = cond(emp_stat == 3, 1, .)
# 
# generate emp_miss = cond(emp_stat < 5, 1, .)
# 
# egen une_sum = count(une_flag), by(ID)
# 
# egen nlf_sum = count(nlf_flag), by(ID)
# 
# egen valid_emps = count(emp_miss), by(ID)
# 
# by ID: drop if _n > 1
# 
# poisson une_sum LOC i.GENDER AFQT nlf_sum, exposure(valid_emps)
# 
# domin une_sum LOC AFQT nlf_sum, reg(poisson, exposure(valid_emps)) fitstat(e(r2_p)) sets((i.GENDER))
# 