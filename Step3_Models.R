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
  
  
