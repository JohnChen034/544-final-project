library(dplyr)
library(MASS)     # glm.nb
library(broom)# tidy summary
library(gtsummary)
library(gt)

# Overdispersion diagnostic: Pearson Chi-square / df
overdisp_ratio <- function(fit) {
  sum(residuals(fit, type = "pearson")^2) / df.residual(fit)
}

# IRR table (exp(beta)) with 95% CI
irr_table <- function(fit, keep_regex = NULL) {
  tt <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
  
  if (!is.null(keep_regex)) {
    tt <- tt %>% dplyr::filter(grepl(keep_regex, term))
  }
  
  tt %>% dplyr::select(term, estimate, conf.low, conf.high, p.value)
}


## ========== Aim 1: County-level main model ==========
dat_county <- county_full %>%
  filter(
    !is.na(Weather.Deaths), # shouldnt have any
    !is.na(Population), Population > 0,
    !is.na(Unemployment_Mean),
    !is.na(Median_Income), Median_Income > 0,
    !is.na(Rural_Code)
  ) %>%
  mutate(
    log_income = log(Median_Income),
    Rural_Code = factor(Rural_Code)
  )

fit_a1_nb <- glm.nb(
  Weather.Deaths ~ Unemployment_Mean + log_income + Rural_Code + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb)
irr_table(fit_a1_nb, keep_regex = "Unemployment_Mean|log_income|Rural_Code")

fit_a1_nb_heat <- glm.nb(
  Heat.Deaths ~ Unemployment_Mean + log_income + Rural_Code + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb_heat)
irr_table(fit_a1_nb_heat, keep_regex = "Unemployment_Mean|log_income|Rural_Code")

fit_a1_nb_cold <- glm.nb(
  Cold.Deaths ~ Unemployment_Mean + log_income + Rural_Code + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb_cold)
irr_table(fit_a1_nb_cold, keep_regex = "Unemployment_Mean|log_income|Rural_Code")

dat_county[["State"]] <- factor(dat_county[["State"]])
fit_a1_nb_fe <- glm.nb(
  Weather.Deaths ~ Unemployment_Mean + log_income + Rural_Code + 
    factor(State) + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb_fe)
irr_table(fit_a1_nb_fe, keep_regex = "Unemployment_Mean|log_income|Rural_Code")

fit_a1_nb_fe_heat <- glm.nb(
  Heat.Deaths ~ Unemployment_Mean + log_income + Rural_Code + 
    factor(State) + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb_fe_heat)
irr_table(fit_a1_nb_fe_heat, keep_regex = "Unemployment_Mean|log_income|Rural_Code")

fit_a1_nb_fe_cold <- glm.nb(
  Cold.Deaths ~ Unemployment_Mean + log_income + Rural_Code + 
    factor(State) + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb_fe_cold)
irr_table(fit_a1_nb_fe_cold, keep_regex = "Unemployment_Mean|log_income|Rural_Code")



## ========== Aim 2: State-level main model ==========
dat_state <- state_full %>%
  filter(
    !is.na(Weather.Deaths),
    !is.na(Population), Population > 0,
    !is.na(Unemployment_Mean),
    !is.na(Median_Income), Median_Income > 0
  ) %>%
  mutate(log_income = log(Median_Income))

fit_a2_pois <- glm(
  Weather.Deaths ~ Unemployment_Mean + log_income,
  family = poisson(link = "log"),
  offset = log(Population),
  data   = dat_state
)

fit_a2_nb <- glm.nb(
  Weather.Deaths ~ Unemployment_Mean + log_income + offset(log(Population)),
  data = dat_state
)
summary(fit_a2_nb)
irr_table(fit_a2_nb, keep_regex = "Unemployment_Mean|log_income")

dat_county[["Census.Region"]] <- factor(dat_county[["Census.Region"]])
fit_a2_nb_fe <- glm.nb(
  Weather.Deaths ~ Unemployment_Mean + log_income + factor(Census.Region) +
    offset(log(Population)),
  data = dat_state
)
summary(fit_a2_nb_fe)
irr_table(fit_a2_nb_fe, keep_regex = "Unemployment_Mean|log_income|Rural_Code")




# Table with main result
library(dplyr)
library(MASS)
library(broom)
library(purrr)
library(tidyr)

models <- list(
  a1_nb         = fit_a1_nb,
  a1_nb_heat    = fit_a1_nb_heat,
  a1_nb_cold    = fit_a1_nb_cold,
  a1_nb_fe      = fit_a1_nb_fe,
  a1_nb_fe_heat = fit_a1_nb_fe_heat,
  a1_nb_fe_cold = fit_a1_nb_fe_cold,
  a2_nb         = fit_a2_nb,
  a2_nb_fe      = fit_a2_nb_fe
)

# Extract estimates into a wide table: rows=models, cols=variables
tab_irr <- imap_dfr(models, function(m, nm) {
  broom::tidy(m, conf.int = TRUE, exponentiate = TRUE) %>%
    transmute(
      model = nm,
      term,
      value = sprintf("%.3f (%.3f, %.3f); p=%.3g", estimate, conf.low, conf.high, p.value)
    )
}) %>%
  pivot_wider(names_from = term, values_from = value)

tab_irr


# With certain variables (e.g., key terms)
keep_terms <- c("Unemployment_Mean", "log_income",
                paste0("Rural_Code", 2:9))

tab_irr_key <- tab_irr %>%
  dplyr::select(model, tidyselect::any_of(keep_terms))

tab_irr_key





### Forest Plot 1
library(broom)
library(dplyr)
library(ggplot2)

coefs <- tidy(fit_a1_nb, conf.int = TRUE, exponentiate = TRUE)

plot_coefs <- coefs %>%
  filter(grepl("Unemployment_Mean|log_income|Rural_Code", term))

ggplot(plot_coefs, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(
    x = "Rate ratio (exp(coef))",
    y = NULL,
    title = "County Predictors of Weather-Related Mortality"
  ) +
  theme_minimal(base_size = 14)

### Forest plot 2
library(broom)
library(dplyr)
library(ggplot2)

coefs <- tidy(fit_a1_nb_fe, conf.int = TRUE, exponentiate = TRUE)

plot_coefs <- coefs %>%
  filter(grepl("Unemployment_Mean|log_income|Rural_Code", term))

ggplot(plot_coefs, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(
    x = "Rate ratio (exp(coef))",
    y = NULL,
    title = "Adjusted associations (within-state FE model)"
  ) +
  theme_minimal(base_size = 14)
