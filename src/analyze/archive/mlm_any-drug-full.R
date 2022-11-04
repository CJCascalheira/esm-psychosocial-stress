# Load dependencies
library(rio)
library(lme4)
library(multilevel)
library(tidyverse)
library(misty)
library(scales)

# Import data
baseline <- import("data/cleaned_scored/baseline_survey.sav") %>%
  as_tibble()

baseline_demographics <- read_csv("data/raw_baseline/baseline_survey.csv")

daily_diaries <- read_csv("data/cleaned_scored/daily_diaries.csv")

# Combine the data
full_data <- left_join(daily_diaries, baseline, by = c("login_id", "link_id")) %>%
  arrange(login_id)

# PREPARING THE DATA ------------------------------------------------------

# Select the demographic variables to merge with other data sets
baseline_demographics1 <- baseline_demographics %>%
  select(login_id, Sex, Gender, Sex_or, Race)

# Grand mean center age
full_data$grand_age <- center(full_data$Age, type = "CGM")

# Adding and recoding variables
full_data1 <- full_data %>%
  # Remove some demographic variables
  select(-Sex, -starts_with("Sex_or"), -starts_with("Gender"), -starts_with("Race")) %>%
  left_join(baseline_demographics1) %>%
  # Create dichotomous demo variables
  mutate(
    is_transgender = if_else(Gender != "Cisgender woman (non-transgender)", 1, 0),
    is_poc = if_else(Race != "White", 1, 0),
    above_25k = if_else(Income %in% c(1, 2), 1, 0)
  ) %>%
  # Create a weekend variable
  mutate(
    week_day = weekdays(StartDate),
    weekend = if_else(week_day %in% c("Friday", "Saturday", "Sunday"), 1, 0)) %>%
  # Rescale values
  mutate(
    grand_age = rescale(grand_age, to = c(0, 1)),
    EDS_total = rescale(EDS_total, to = c(0, 1)),
    ACES_total = rescale(ACES_total, to = c(0, 1)),
    IPV_total = rescale(IPV_total, to = c(0, 1)),
    SEX_HAR_total = rescale(SEX_HAR_total, to = c(0, 1)),
    day = rescale(day, to = c(0, 1))
  )

# NULL MODEL --------------------------------------------------------------

# Specify the null model
null_model <- glmer(
  DD_any_drug_use ~ 1 + 
    (1|login_id), # Random effect of person because time varies within person
  family = "binomial", 
  data = full_data1,
  nAGQ = 5, # number of quadratures
  glmerControl(optimizer="bobyqa")
)
summary(null_model)

# ICC
null_model_aov <- aov(DD_any_drug_use ~ as.factor(login_id), full_data1)
ICC1(null_model_aov)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(null_model, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(null_model), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds of using any drug on a given day 
print(ORwCI, digits=3)

# Confidence intervals for the RE - YES, significant variance in u_oj
confint(null_model, parm = "theta_", method = "profile", oldNames = F) 

# WEEKEND MODEL -----------------------------------------------------------

# Specify the weekend model
weekend_model <- glmer(
  DD_any_drug_use ~ 1 + weekend +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Lapalce approximation 
  glmerControl(optimizer="bobyqa")
)
summary(weekend_model)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(weekend_model, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(weekend_model), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# Show the odds and odd ratios
print(ORwCI, digits = 3)

# RE TIME -----------------------------------------------------------------

# Specify the model with RE for time to control for linear trends
base_model <- glmer(
  DD_any_drug_use ~ 1 + day +
    (1 + day|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Lapalce approximation 
  glmerControl(optimizer="bobyqa")
)
summary(base_model)

# ADD L1 STRESSORS ---------------------------------------------------------

# Model with Level 1 daily stressors
l1_stressors_model <- glmer(
  DD_any_drug_use ~ 1 + day + DD_gen_stress_total + DD_minority_stress_total + 
    (1 + day + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_model)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_model, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_model), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# What are the odds of daily drug use for general stressors?
ORwCI[1,1] * ORwCI[2,1]

# What are the odds of daily drug use for minority stressors?
ORwCI[1,1] * ORwCI[3,1]

# The above odds, while significant, are too small to be meaningful.

# Nonetheless, the L1 stressor model is significantly better than the null
# model according to results from a LR test
anova(null_model, l1_stressors_model)

# DEMOGRAPHIC COVARIATES --------------------------------------------------

# A model with just demographic covariates
l2_demographics <- glmer(
  DD_any_drug_use ~ 1 + above_25k + grand_age +
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_demographics)

# L1 STRESSORS + COVARIATES -----------------------------------------------

# Model with Level 1 daily stressors and demographic covariates 
l1_stress_covariates <- glmer(
  DD_any_drug_use ~ 1 + DD_gen_stress_total + DD_minority_stress_total + 
    above_25k + grand_age +
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)
summary(l1_stress_covariates)

# L1 + L2 STRESSORS -------------------------------------------------------

# Model with Level 1 AND Level 2 stressors and demographic covariates 
l1_l2_stressors <- glmer(
  DD_any_drug_use ~ 1 + DD_gen_stress_total + DD_minority_stress_total + 
    # L2 distal stressors
    EDS_total + ACES_total + PTSD_PROB +
    # L2 demographic covariates
    above_25k + grand_age +
    # Random effects
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)
summary(l1_l2_stressors)

# LIFETIME STRESSORS ------------------------------------------------------

# Model with Level 2 lifetime stressors
l2_lifetime_stressors <- glmer(
  DD_any_drug_use ~ 1 + EDS_total + ACES_total + IPV_total + SEX_HAR_total +
    # L2 demographic covariates
    above_25k + grand_age +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)
summary(l2_lifetime_stressors)

l2_stressors_two <- glmer(
  DD_any_drug_use ~ 1 + EDS_total + IPV_total +
    # L2 demographic covariates
    above_25k + grand_age +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)
summary(l2_stressors_two)

#* Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l2_lifetime_stressors, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_lifetime_stressors), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# BAD MODELS --------------------------------------------------------------

#* Add L2 race * stressors ------------------------------------------------

# Add race as a L2 interaction effect
l2_race_model <- glmer(
  DD_any_drug_use ~ 1 + DD_minority_stress_total*is_poc + DD_gen_stress_total*is_poc +
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)

# Model failed to converge

#* Add L1 All Vars --------------------------------------------------------

# Model with Level 1 daily stressors, daily affect, daily avoidance
l1_all_vars_model <- glmer(
  DD_any_drug_use ~ 1 + DD_gen_stress_total + DD_minority_stress_total + 
    DD_affect_total + DD_exp_avoid_total +
    (1 + DD_gen_stress_total + DD_minority_stress_total + DD_affect_total + 
       DD_exp_avoid_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)
summary(l1_all_vars_model)

# Model is singular, so do not use this model

#* Just L1 Aff/Avd --------------------------------------------------------

# Model with Level 1 daily affect and daily avoidance
l1_aff_avd_model <- glmer(
  DD_any_drug_use ~ 1 + DD_affect_total + DD_exp_avoid_total +
    (1 + DD_affect_total + DD_exp_avoid_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)
summary(l1_aff_avd_model)

# Model is nearly not identified, so do not use this model

#* L1 Affect * Stressors ---------------------------------------------------

# Daily stressors interacting with daily affect
affect_interaction_model <- glmer(
  DD_any_drug_use ~ 1 + DD_minority_stress_total*DD_affect_total + 
    DD_gen_stress_total*DD_affect_total +
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  glmerControl(optimizer="bobyqa")
)
summary(affect_interaction_model)
