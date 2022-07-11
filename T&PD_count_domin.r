setwd("/home/josephluchman/github_misc/count-da-manu/count-da-manu/")

devtools::load_all("/home/josephluchman/R/gitteR/domir/")

library(tidyverse)
library(magrittr)

# revisit data management here - has this been looked over?

nlsORM79 <- readRDS("NLS79_masterORM.rds")

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
            GENDER = first(GENDER))

# linear model comparison ----

lm_compare <- 
  lm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
     data = nlsORM79_reduced %>% 
       drop_na())

poisson_no_offset <- 
glm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER, 
    data = nlsORM79_reduced %>% 
      drop_na(), family = poisson()) %>% summary

poisson_offset <- 
  glm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
      data = nlsORM79_reduced %>% 
        drop_na(), family = poisson()) %>% summary

glm(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
    data = nlsORM79_reduced %>% 
      drop_na(), family = poisson()) %>% pscl::pR2()

 %>% summary

domir(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
      \(fml) {
        (res <- glm(fml, 
            data = nlsORM79_reduced %>% 
              drop_na(), family = poisson()) %>% 
          pscl::pR2() %>% pluck("McFadden")) %>% 
          capture.output()
        return(res)
      },
      .adj = ~ offset(valid)) %>% summary

domir(unemp_ct ~ nlf_ct + AFQT + LOC + GENDER + offset(valid), 
      \(fml) {
        (res <- glm(fml, 
                    data = nlsORM79_reduced %>% 
                      drop_na()) %>%
           pscl::pR2() %>% pluck("McFadden")) %>% 
          capture.output()
        return(res)
      },
      .adj = ~ offset(valid)) %>% summary
  

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