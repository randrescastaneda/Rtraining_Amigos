# Loadlibraries
library(tidyverse)

# load data
dt <- haven::read_dta(file = "data/PRY 2006 & 2016 GMD.dta")
class(dt)
typeof(dt)

sk <- skimr::skim(dt)

dt$hola <- 1
dt$hola <- NULL

# gen double welfare_ppp = welfare/cpi2011/icp2011/365

# Standard evaluation
dt$welfare_ppp <- dt$welfare/dt$cpi2011/dt$icp2011/365


# tidyverse Non-standard evaluation (NSE)

# pipe: ctrl + shift + M
dt <- dt %>%
  mutate(welfare_ppp = welfare/cpi2011/icp2011/365)


# summarise

dt %>%
  group_by(year) %>%
  summarise(mean_welfare = weighted.mean(welfare, weight_p))

# sin nombre
dt %>%
  group_by(year) %>%
  summarise(weighted.mean(x = welfare, w = weight_p))

# more than one variable
dt %>%
  group_by(year) %>%
  summarise(across(.cols = c(welfare, welfare_ppp),
                   .fns =  mean))

mw <-
dt %>%
  group_by(year) %>%
  summarise(across(.cols = c(welfare, welfare_ppp),
                   .fns = ~ weighted.mean(x = .x,
                                   w = weight_p)))




dt %>%
  group_by(year) %>%
  summarise(across(.cols = c(welfare, welfare_ppp),
                   .fns = ~ {

                     qq <- weighted.mean(x = .x,
                                   w = weight_p)
                     # e <- qq/2
                     # return(e)

                     qq/ 2
                   }))




