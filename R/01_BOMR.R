
# -------------------------------------------------------------------------

# Bayesian Ordinal Multilevel Regression Modelling
# @author: michal.wawrzynowicz@amu.edu.pl

# -------------------------------------------------------------------------

# Setup -------------------------------------------------------------------
library(brms)
library(tidyverse)
library(broom.mixed)
library(viridis)

# The data ----------------------------------------------------------------


data <- read_csv(file = "data/data.csv")
data <- filter( data , sex == 1 ) # excluding male participants
data <- select( data , - sex ) # removing sex column

# sensory test data
sensory_data <- data %>%
  drop_na( rating_sensory ) %>% 
  select( sensory_property , treatment , info, ID , rating_sensory ) %>% 
  mutate( rating_sensory = as.ordered( rating_sensory ),
         sensory_property = as.factor( sensory_property ),
         treatment = as.factor( treatment ),
         info = as.factor( info ),
         ID = as.factor( ID )
         ) 

# willingness for purchase data
purchase_data <- data %>%
  drop_na( rating_purchase ) %>% 
  select( treatment , info , ID , rating_purchase ) %>% 
  mutate( rating_purchase = as.ordered( rating_purchase ),
         treatment = as.factor( treatment ),
         info = as.factor( info ),
         ID = factor( ID )
         )

rm(data); glimpse(purchase_data); glimpse(sensory_data)

# BOMR for sensory properties data ----------------------------------------

( f <- formula( rating_sensory ~ 1 + treatment * info + ( 1|sensory_property ) + ( 1|ID )) )

prior <- c(
  prior(normal(0, 1), class = "b")
)


m_sens <- brm(
  f ,
  prior = prior ,
  data = sensory_data ,
  family = cumulative( "probit" ) ,
  cores = 4 , 
  control = list( adapt_delta = 0.99 )
  )

summary( m_sens )

# some diagnostics
plot( m_sens , ask = FALSE )
pp_check( m_sens , type = "bars" )
head( predict( m_sens ))
loo( m_sens )

save( m_sens, file = "data/m_sens.RData" )


# BOMR for willingness for purchase data ----------------------------------

( f <- formula(rating_purchase ~ treatment * info + ( 1|ID )) )

m_purchase <- brm(
  f ,
  prior = prior ,
  data = purchase_data ,
  family = cumulative( "probit" ) ,
  cores = 4 , 
  control = list( adapt_delta = 0.99 )
)

summary( m_purchase )

# some diagnostics
plot( m_purchase , ask = FALSE )
pp_check( m_purchase , type = "bars" )
head( predict( m_purchase ))
loo( m_purchase )

save(m_purchase, file = "data/m_purchase.RData")


# BOMR for each individual sensory property -------------------------------

fit_mmodels <- function(property_names, formula, prior, data, grouping_column) {
  
  # Helper function for fitting BOMR for for each individual sensory property
  
  # property_names  - vector with characters representing measured sensory properties (object class: "vector")
  # formula         - model formula (object class: "formula")
  # prior           - model prior (object class: "brmsprior", "data.frame")
  # data            - the data (object class: "data.frame")
  # grouping_column - column name to group by

  models <- list()
  
  for (property in property_names) {
  message(paste("Fitting Bayesian Ordinal Multilevel Regression for:", property))

    # data for given property
    dat <- data %>% 
      filter(.data[[grouping_column]] == property)
    
    # check if not empty
    if (nrow(dat) == 0) {
      warning(paste("Brak danych dla:", property))
      next
    }
    
    # fitting 
    fit <- 
      brm(formula,
          prior = prior,
          data = dat,
          family = cumulative("probit"),
          cores = 4, 
          control = list(adapt_delta = 0.99))
    
    # saving 
    models[[property]] <- fit
    
  }
  
  return(models)
  
}

property_names <- as.character(unique(sensory_data$sensory_property))

( f <- formula(rating_sensory ~ treatment * info + ( 1|ID )) )

models <- list()

models <- fit_mmodels( property_names , f , prior , sensory_data , grouping_column = "sensory_property" )
save(models, file = "data/models.RData")

# Plotting ----------------------------------------------------------------

# Setup -------------------------------------------------------------------

# loading models
load("data/m_sens.RData")
load("data/m_purchase.RData")
load("data/models.RData")

# -------------------------------------------------------------------------

# Efects sizes

# -------------------------------------------------------------------------

forest_plot <- function(model, term_labels, breaks){
 
  # Helper function for visualizing model fixed effects
  
  # model       - fitted model object (e.g. brmsfit)
  # term_labels - named vector for renaming predictors: c("old_term_name" = "New Term Name")
  # breaks      - numeric vector defining X-axis (effect size) ticks
  
  # extracting model data
  coef_df <- tidy(model, effects = "fixed", conf.int = TRUE)
  
  fp <- coef_df %>%
    filter( !str_detect ( term , "Intercept" )) %>% 
    mutate( term = recode( term , !!!term_labels )) %>%
    mutate( term = factor( term, levels = rev(term))) %>% 
    mutate( directional = ifelse(conf.low > 0 | conf.high < 0, "credible", "overlaps 0")
            ) %>% 
    ggplot(aes(x = term, y = estimate, color = directional)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
    scale_color_manual(values = c("credible" = "red3", "overlaps 0" = "grey20")) +
    scale_y_continuous(breaks = breaks) +
    coord_flip() +
    labs(
      x = "",
      y = "Effect size",
      title = ""
    ) +
    theme_linedraw(base_size = 14) +
    theme(legend.position = "none")
  
  return(fp)
}

fixef(m_sens)

term_labs <- c(
  "treatment10" = "Effect of 10% HCF" ,
  "treatment20" = "Effect of 20% HCF" ,
  "info1" = "Effect of information on 0% HCF" ,
  "treatment10:info1" = "Extra effect of information on 10% HCF" ,
  "treatment20:info1" = "Extra effect of information on 20% HCF")


# effect sizes for sensory properties
sens_effects <- forest_plot(m_sens, term_labs, breaks = seq(-3, 3, 0.25))
# effect sizes for willinges for purchase
purchase_effects <- forest_plot(m_purchase, term_labs, breaks = seq(-3, 3, 0.5))
# effect sizes for every signle sensory property
plots <- lapply(models, forest_plot, term_labs, breaks = seq(-3, 3, 0.5))


# plotting parameters
mult <- 1
w <- 170; h <- 100

# Dimensions in inches * multiplier:
wi <- round( mult * w / 25.4, 1 )
hi <- round( mult * h / 25.4, 1 )

# plotting

# sens_effects
ggsave("Figures/sens_effects.tiff" ,
       plot = sens_effects ,
       width = wi ,
       height = hi ,
       units = "in" ,
       dpi = 300 ,
       compression = "lzw"
       )

# purchase effects
ggsave("Figures/purchase_effects.tiff" , 
       plot = purchase_effects ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw" 
       )

# taste_effects
ggsave("Figures/taste_effects.tiff" , 
       plot = plots[["taste"]] ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw" 
       )

# consistency_effects
ggsave("Figures/consistency_effects.tiff" ,
       plot = plots[["consistency"]] ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw" 
       )

# appearance_effects
ggsave("Figures/appearance_effects.tiff" , 
       plot = plots[["appearance"]] ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw" 
       )

# odour_effects
ggsave("Figures/odour_effects.tiff" , 
       plot = plots[["odor"]] ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw" 
       )

# overall_assessment_effects
ggsave("Figures/overall_assessment_effects.tiff" , 
       plot = plots[["overall_assessment"]] ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw" 
       )

# -------------------------------------------------------------------------

# Marginal effects 

# -------------------------------------------------------------------------

# Marginal effects for sensory properties data

# extracting predictions
conditions <- make_conditions(m_sens, "info")

cond_effects_sens <- conditional_effects(
  m_sens ,
  effects = "treatment" ,
  conditions = conditions, categorical = TRUE, plot = FALSE)

cond_effects_sens <- cond_effects_sens[["treatment:cats__"]] %>% 
  rename_with( ~ str_remove( . , "__") , ends_with( "__" ))
  
wid <- 0.85

# custom plot
cond_effects_sens <- cond_effects_sens %>% 
  ggplot(aes( effect1 , estimate , colour = cats )) +
  geom_vline(xintercept = c(1.5, 2.5), color = "gray85", size = 0.5) +
  geom_point( position = position_dodge( width = wid ) , size = 2.8) +
  geom_errorbar( aes( ymin = lower , ymax = upper ) , position = position_dodge( width = wid ) , width = 0.3) +
  facet_wrap(~ cond, labeller = as_labeller(c("info = 0" = "Before information", "info = 1" = "After information"))) +
  theme_bw(base_size = 14) +
  labs(
    x = "HCF enrichment [in %]",
    y = "Probability",
    title = ""
    ) +
  theme(panel.grid.major.x = element_blank()) +
  scale_color_viridis_d(option = "turbo", name = "Rating")


# Marginal effects for willingness for purchase data

# extracting predictions
conditions <- make_conditions(m_purchase, "info")

cond_effects_purchase <- conditional_effects(
  m_purchase ,
  effects = "treatment" ,
  conditions = conditions, categorical = TRUE, plot = FALSE)

cond_effects_purchase <- cond_effects_purchase[["treatment:cats__"]] %>% 
  rename_with( ~ str_remove( . , "__") , ends_with( "__" ))


wid <- 0.65

# custom plot
cond_effects_purchase <- cond_effects_purchase %>% 
  ggplot(aes( effect1 , estimate , colour = cats )) +
  geom_vline(xintercept = c(1.5, 2.5), color = "gray85", size = 0.5) +
  geom_point( position = position_dodge( width = wid ) , size = 2.8) +
  geom_errorbar( aes( ymin = lower , ymax = upper ) , position = position_dodge( width = wid ) , width = 0.3) +
  facet_wrap(~ cond, labeller = as_labeller(c("info = 0" = "Before information", "info = 1" = "After information"))) +
  theme_bw(base_size = 14) +
  labs(
    x = "HCF enrichment [in %]",
    y = "Probability",
    title = ""
  ) +
  theme(panel.grid.major.x = element_blank()) +
  scale_color_viridis_d(option = "turbo", name = "Rating")


# plotting

# plotting parameters
mult <- 1
w <- 250 ; h <- 130

# Dimensions in inches * multiplier:
wi <- round( mult * w / 25.4, 1 )
hi <- round( mult * h / 25.4, 1 )

# cond_effects_sens
ggsave("Figures/cond_effects_sens.tiff" , 
       plot = cond_effects_sens ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw"
       )

# cond_effects_purchase
ggsave("Figures/cond_effects_purchase.tiff" , 
       plot = cond_effects_purchase ,
       width = wi , 
       height = hi , 
       units = "in" , 
       dpi = 300 , 
       compression = "lzw" 
       )
