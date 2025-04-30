# Plots for publication
# @author: michal.wawrzynowicz@amu.edu.pl

# setup -------------------------------------------------------------------
library(brms)
library(ggplot2)
library(tidyr)
library(dplyr)
library(broom.mixed)


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

# models
load("data/m01_sens.RData")
load("data/m01_purchase.RData")


prop_names <- as.character(unique(sensory_data$sensory_property))
res <- list()

for (pn in prop_names) {
  print(pn)
  load(paste0("data/sensory_properties_models/", "sensory_property_", pn, ".RData"))
  res[[pn]] <- fit; rm(fit)
}



# Forest plots ------------------------------------------------------------

# Sensory test model effects sizes visualisation

# extracting model data
coef_df <- tidy(m01_sens, effects = "fixed", conf.int = TRUE)
coef_df <- coef_df %>%
  filter(term %in% c("treatment10", "treatment20", "info1", "treatment10:info1", "treatment20:info1")) %>% 
  mutate(term = recode(term,
                       "treatment10" = "Bread with 10% HCF (before information)",
                       "treatment20" = "Bread with 20% HCF (before information)",
                       "info1" = "Control bread (after information)",
                       "treatment10:info1" = "Bread with 10% HCF (after information)",
                       "treatment20:info1" = "Bread with 20% HCF (after information)"
  )) %>% 
  mutate(term = factor(term, levels = rev(unique(term)))) %>% 
  mutate(significant = ifelse(conf.low > 0 | conf.high < 0, "signif", "nsignif"))

# Forest plot
ggplot(coef_df, aes(x = term, y = estimate, color = significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("signif" = "red3", "nsignif" = "gray20")) +
  coord_flip() +
  labs(
    x = "",
    y = "Effect size",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Willingness for purchase model effects sizes visualisation

# extracting model data
coef_df <- tidy(m01_purchase, effects = "fixed", conf.int = TRUE)
coef_df <- coef_df %>%
  filter(term %in% c("treatment10", "treatment20", "info1", "treatment10:info1", "treatment20:info1")) %>% 
  mutate(term = recode(term,
                       "treatment10" = "Bread with 10% HCF (before information)",
                       "treatment20" = "Bread with 20% HCF (before information)",
                       "info1" = "Control bread (after information)",
                       "treatment10:info1" = "Bread with 10% HCF (after information)",
                       "treatment20:info1" = "Bread with 20% HCF (after information)"
  )) %>% 
  mutate(term = factor(term, levels = rev(unique(term)))) %>% 
  mutate(significant = ifelse(conf.low > 0 | conf.high < 0, "signif", "nsignif"))

# Forest plot
ggplot(coef_df, aes(x = term, y = estimate, color = significant)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("signif" = "red3", "nsignif" = "gray20")) +
  coord_flip() +
  labs(
    x = "",
    y = "Effect size",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Sensory properties models effects sizes visualisation

p_list <- list()

for (i in seq_along(v)) {
  
  coef_df <- tidy(res[[v[i]]], effects = "fixed", conf.int = TRUE)
  coef_df <- coef_df %>%
    filter(term %in% c("treatment10", "treatment20", "info1", "treatment10:info1", "treatment20:info1")) %>% 
    mutate(term = recode(term,
                         "treatment10" = "Bread with 10% HCF (before information)",
                         "treatment20" = "Bread with 20% HCF (before information)",
                         "info1" = "Control bread (after information)",
                         "treatment10:info1" = "Bread with 10% HCF (after information)",
                         "treatment20:info1" = "Bread with 20% HCF (after information)"
    )) %>% 
    mutate(term = factor(term, levels = rev(unique(term)))) %>% 
    mutate(significant = ifelse(conf.low > 0 | conf.high < 0, "signif", "nsignif"))
  
  # Forest plot
  p_list[[v[i]]] <- ggplot(coef_df, aes(x = term, y = estimate, color = significant)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("signif" = "red3", "nsignif" = "gray20")) +
    coord_flip() +
    labs(
      x = "",
      y = "Effect size",
      title = paste0(v[i])
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
}

# altogether
# library(patchwork)
# p <- p_list[["taste"]] + p_list[["consistency"]] + p_list[["appearance"]] + p_list[["odor"]] + p_list[["overall_assessment"]]; p

# separately
p_list[["taste"]]
p_list[["consistency"]]
p_list[["appearance"]]
p_list[["odor"]]
p_list[["overall_assessment"]]


# Marginal effects --------------------------------------------------------
# Sensory test model marginal effects visualisation

conditions <- make_conditions(sensory_data, vars = c("info"))
ce_t <- plot(conditional_effects(m01_sens, effects = "treatment",
                                 conditions = conditions, categorical = TRUE))

ce_t_plot <- ce_t[["treatment:cats__"]] + 
  theme_bw() +
  theme(text = element_text(family = "Arial", size = 14)) +
  scale_x_discrete(name = "HCF enrichment [in %]") +
  facet_wrap(~ info, labeller = as_labeller(c("0" = "Before information", "1" = "After information"))) +
  scale_color_discrete(name = "Rating") +
  scale_fill_discrete(name = "Rating") +
  ggtitle("")
ce_t_plot

# Willingness for purchase model marginal effects visualisation

conditions <- make_conditions(purchase_data, vars = c("info"))

ce_b <- plot(conditional_effects(m01_purchase, effects = "treatment", conditions = conditions, categorical = TRUE))

ce_b_plot <- ce_b[["treatment:cats__"]] + 
  theme_bw() +
  theme(text = element_text(family = "Arial", size = 14)) +
  scale_x_discrete(name = "HCF enrichment [in %]") +
  facet_wrap(~ info, labeller = as_labeller(c("0" = "Before information", "1" = "After information"))) +
  scale_color_discrete(name = "Rating") +
  scale_fill_discrete(name = "Rating") +
  ggtitle("")
ce_b_plot

