library(dplyr)
library(MASS)    
library(broom)    

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


fit_a1_nb_nl <- glm.nb(
  Weather.Deaths ~ Unemployment_Mean + I(Unemployment_Mean^2) + log_income + Rural_Code + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb_nl)
irr_table(fit_a1_nb_nl, keep_regex = "Unemployment_Mean|log_income|Rural_Code")



fit_a1_nb_fe <- glm.nb(
  Weather.Deaths ~ Unemployment_Mean + log_income + Rural_Code + 
    factor(State) + offset(log(Population)),
  data = dat_county
)
summary(fit_a1_nb_fe)
irr_table(fit_a1_nb_fe, keep_regex = "Unemployment_Mean|log_income|Rural_Code")
  
  


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
