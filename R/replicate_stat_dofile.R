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
                   .fns = function(y) {
                     weighted.mean(x = y,
                                   w = weight_p)
                   } ))



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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## FGT measures --------


# Usando Base R
povlines <- c(1.9, 3.2, 5.5)
alphas   <- c(0, 1, 2)

wvar <- dt$welfare_ppp

for (p in povlines) {

  for (a in alphas) {
    var_name <-  paste0("fgt", a, "_", 100*p)

    dt[var_name] <-
      100*((wvar < p)*(1-(wvar/p))^a)
  }
}

# using functional programming with purrr
fgt <- function(p, a, wvar) {
    100*((wvar < p)*(1-(wvar/p))^a)

}


vecs <- expand_grid(p = povlines,
                    a = alphas)

d <-
purrr::map2(.x = vecs$p,
            .y = vecs$a,
            .f = fgt,
            wvar = wvar
            )



headcount <- function(p, a, wvar, weghts) {
    x <- 100*((wvar < p)*(1-(wvar/p))^a)
    weighted.mean(x, weights)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calculate headcount --------

dt %>%
  group_by(year) %>%
  summarise(across(.cols = matches("^fgt0.+"),
                   .fns = ~ weighted.mean(x = .x,
                                         w = weight_p)
                     ))


dt %>%
  group_by(year) %>%
  summarise(across(.cols = starts_with("fgt0"),
                   .fns = ~ weighted.mean(x = .x,
                                         w = weight_p)
                     ))


dt %>%
  group_by(year) %>%
  summarise(across(.cols = c(fgt0_190, fgt0_320, fgt0_550),
                   .fns = ~ weighted.mean(x = .x,
                                         w = weight_p)
                     ))

