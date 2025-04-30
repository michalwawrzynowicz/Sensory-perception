# Bayesian Ordinal Multilevel Regression Modelling
# @author: michal.wawrzynowicz@amu.edu.pl

# setup -------------------------------------------------------------------
library(brms)
library(dplyr)
library(tidyr)

# the data
data <- read.csv(file = "data/data.csv", sep = ",")

# sensory test data
sensory_data <- data %>%
  select(treatment, info, sex, ID, rating_sensory, sensory_property) %>%
  drop_na()
glimpse(sensory_data)

# willingness for purchase data
purchase_data <- data %>%
  select(treatment, info, sex, ID, rating_purchase) %>%
  drop_na()
glimpse(purchase_data)

rm(data)

# BOMRM - sensory test data -----------------------------------------------

# equal trait-specific variability
f01 <- formula(rating_sensory ~ 1 + treatment * info + (1|ID) + (1|sensory_property)); f01
m01_sens <- 
  brm(f01,
      data = sensory_data,
      family = cumulative("probit"),
      cores = 4, 
      control = list(adapt_delta = 0.99))

summary(m01_sens)

# some diagnostics
plot(m01_sens, ask = FALSE)
pp_check(m01_sens, type = "bars")
head(predict(m01_sens))
loo(m01_sens)

# random intercepts and random slopes for sensory traits
f02 <- formula(rating_sensory ~ 1 + treatment * info + (1 + treatment * info | sensory_property) + (1 | ID)); f02

m02_sens <- 
  brm(f02,
      data = sensory_data,
      family = cumulative("probit"),
      control = list(adapt_delta = 0.99), cores = 4)

summary(m02_sens)

# some diagnostics
plot(m02_sens, ask = FALSE)
pp_check(m02_sens, type = "bars")
head(predict(m02_sens))
loo(m02_sens)

# models comparison
loo_compare(loo(m01_sens), loo(m02_sens))

save(m01_sens, file = "data/m01_sens.RData")

# BOMRM - willingness for purchase data -----------------------------------

f03 <- formula(rating_purchase ~ treatment * info + (1|ID)); f03

m01_purchase <- 
  brm(f03,
      data = purchase_data,
      family = cumulative("probit"), 
      control = list(adapt_delta = 0.99), cores = 4)
summary(m01_purchase)

# some diagnostics
plot(m01_purchase, ask = FALSE)
pp_check(m01_purchase, type = "bars")
head(predict(m01_purchase))
loo(m01_purchase)

save(m01_purchase, file = "data/m01_purchase.RData")

# BOMRM - sensory properties data -----------------------------------------

fit_mmodels <- function(v, f, data, grouping_column = "sensory_property") {
  # v - vector with characters representing measured sensory properties (object class: "vector")
  # f - model formula (object class: "formula")
  # data - the data (object class: "data.frame")
  # grouping_column - column name to group by

  dir <- "data/sensory_properties_models"
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  
  res <- list()
  
  for (pn in prop_names) {
    t <- pn; print(t) 
    dat <- data %>% 
      filter(.data[[grouping_column]] == t)
    fit <- 
      brm(f,
          data = dat,
          family = cumulative("probit"),
          cores = 4, 
          control = list(adapt_delta = 0.99))
    save(fit, file = paste0("data/sensory_properties_models/", "sensory_property_", t, ".RData"))
    res[[t]] <- summary(fit)
  }
  return(res)
}

prop_names <- as.character(unique(sensory_data$sensory_property))
f <- formula(rating_sensory ~ treatment * info + (1|ID)); f

sp_res <- fit_mmodels(prop_names, f, sensory_data, grouping_column = "sensory_property")
warnErrList(sp_res)

save(sp_res, file = "data/sp_models.RData")
