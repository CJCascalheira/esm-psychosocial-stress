# Load dependencies
library(rio)
library(lme4)
library(multilevel)
library(Hmisc)
library(tidyverse)
library(misty)
library(scales)

# Set options for scientific notation
options(scipen = 100)

# Import data
baseline <- import("data/cleaned_scored/baseline_survey.sav") %>%
  as_tibble()

baseline_demographics <- read_csv("data/raw_baseline/baseline_survey.csv")

daily_diaries <- read_csv("data/cleaned_scored/daily_diaries.csv")

# Combine the data
full_data <- left_join(daily_diaries, baseline, by = c("login_id", "link_id")) %>%
  arrange(login_id)

# PREPARING THE DATA ------------------------------------------------------

# People who used at least one drug other than alcohol or tobacco
subset_ids <- daily_diaries %>%
  filter(DD_any_drug_use == 1) %>%
  distinct(login_id) %>%
  pull(login_id)

# Select the demographic variables to merge with other data sets
baseline_demographics1 <- baseline_demographics %>%
  select(login_id, Sex, Gender, Sex_or, Race, Income) %>%
  # If more than one identity -> "Nonbinary"
  mutate(
    Gender = if_else(str_detect(Gender, ","), "Nonbinary", Gender),
    Gender = recode(Gender, "Transgender woman/Trans woman" = "Trans woman/feminine", 
                    "Trans feminine" = "Trans woman/feminine",
                    "Transgender man/Trans man" = "Trans man/masculine",
                    "Trans masculine" = "Trans man/masculine",
                    "Non-binary" = "Nonbinary",
                    "Gender non-conforming" = "Nonbinary",
                    "Enby" = "Nonbinary",
                    "You don’t have an option that describes my gender identity (please specify):" = "Other",
                    "Two-Spirit" = "Other",
                    "Agender" = "Other",
                    "Cisgender woman (non-transgender)" = "Cisgender woman")
  ) %>%
  # If more than one identity -> "Multiracial"
  mutate(
    Race = if_else(str_detect(Race, ","), "Multiracial", Race),
    Race = recode(Race, "Biracial or Multiracial" = "Multiracial",
                  "American Indian or Alaska Native" = "Other",
                  "Middle Eastern" = "Other",
                  "Asian" = "AAPI",
                  "Native Hawaiian or other Pacific Islander" = "AAPI",
                  "Black or African American" = "Black",
                  "Latina/Latino/Latinx or Hispanic" = "Latinx")
  ) %>%
  # If more than one identity -> "Queer"
  mutate(
    Sex_or = if_else(str_detect(Sex_or, ","), "Queer", Sex_or),
    Sex_or = recode(Sex_or, "Lesbian" = "Lesbian / Gay", "Gay" = "Lesbian / Gay", 
                    "Demisexual" = "Other", "Fluid"  = "Other", "Sexually Fluid"  = "Other",
                    "Bisexual" = "Bi/pansexual", "Pansexual" = "Bi/pansexual", "Asexual" = "Other")
  ) %>%
  # Set reference group
  mutate(
    Gender = relevel(factor(Gender), ref = "Cisgender woman"),
    Sex_or = relevel(factor(Sex_or), ref = "Lesbian / Gay"),
    Race = relevel(factor(Race), ref = "White"),
    Income = relevel(factor(Income), ref = "$25,000-$49,999")
  )

# Grand mean center age
full_data$grand_age <- center(full_data$Age, type = "CGM")

# Grand mean center distal stressors
full_data$grand_EDS <- center(full_data$EDS_total, type = "CGM")
full_data$grand_ACES <- center(full_data$ACES_total, type = "CGM")
full_data$grand_IPV <- center(full_data$IPV_total, type = "CGM")

# Create compositional effects
comp_effects <- full_data %>%
  group_by(login_id) %>%
  summarize(
    mean_general_stress = mean(DD_gen_stress_total, na.rm = TRUE),
    mean_minority_stress = mean(DD_minority_stress_total, na.rm = TRUE),
    mean_ptsd = mean(DD_ptsd_total, na.rm = TRUE)
  )

# Adding and recoding variables
full_data1 <- full_data %>%
  # Remove some demographic variables
  select(-Sex, -starts_with("Sex_or"), -starts_with("Gender"), -starts_with("Race"), -Income) %>%
  left_join(baseline_demographics1) %>%
  # Create dichotomous demo variables
  mutate(
    is_transgender = if_else(Gender != "Cisgender woman", 1, 0),
    is_poc = if_else(Race != "White", 1, 0),
    above_25k = if_else(Income %in% c("Under $9,000", "$10,000-$24,999"), 1, 0)
  ) %>%
  # Add compositional effects
  left_join(comp_effects) %>%
  # Create a weekend variable
  mutate(
    week_day = weekdays(StartDate),
    weekend = if_else(week_day %in% c("Friday", "Saturday", "Sunday"), 1, 0)) %>%
  # Center day at 0
  mutate(day = day - 1) %>%
  # Rescale values
  mutate(
    grand_age = rescale(grand_age, to = c(0, 1)),
    EDS_total = rescale(EDS_total, to = c(0, 1)),
    ACES_total = rescale(ACES_total, to = c(0, 1)),
    IPV_total = rescale(IPV_total, to = c(0, 1)),
    TR_CEN_mean = rescale(TR_CEN_mean, to = c(0, 1)),
    IH_mean = rescale(IH_mean, to = c(0, 1)),
    day = rescale(day, to = c(0, 1)),
    DD_gen_stress_total = rescale(DD_gen_stress_total, to = c(0, 1)),
    DD_minority_stress_total = rescale(DD_minority_stress_total, to = c(0, 1)),
    mean_general_stress = rescale(mean_general_stress, to = c(0, 1)),
    mean_minority_stress = rescale(mean_minority_stress, to = c(0, 1))
  ) %>%
  # Lag of dependent variable
  mutate(
    lag_DD_drug_use = Lag(DD_any_drug_use, -1),
    lag_DD_drug_use = rescale(lag_DD_drug_use, to = c(0, 1))
  ) %>%
  # Select only people who used at least one drug during the dail diary
  filter(login_id %in% subset_ids)

# BIVARIATE CORRELATION MATRIX --------------------------------------------

# Aggregate daily drug use
mean_drug_df <- full_data1 %>%
  group_by(login_id) %>%
  summarize(mean_drug_use = mean(DD_any_drug_use))

# Select the main variables: between-person (baseline, compositional)
corr_df_between <- full_data1 %>%
  select(login_id, mean_general_stress, mean_minority_stress, EDS_total, ACES_total, 
         IPV_total, IH_mean, TR_CEN_mean) %>%
  distinct(login_id, .keep_all = TRUE) %>%
  left_join(mean_drug_df)

# Select the main variables: within-person level (daily)
corr_df_within <- full_data1 %>%
  select(login_id, DD_any_drug_use, DD_gen_stress_total, DD_minority_stress_total)

# Between person correlations
rcorr(as.matrix(corr_df_between[, -1]))

# Within-person correlations
rcorr(as.matrix(corr_df_within[, -1]))

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

# ADD DAY? ----------------------------------------------------------------

# No RE for time variable
day_no_re <- glmer(
  DD_any_drug_use ~ 1 + day +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)

# No significant linear trend because no significant FE for day

# No RE for time variable
day_yes_re <- glmer(
  DD_any_drug_use ~ 1 + day +
    (1 + day|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)

# Confidence interval for day
confint(day_yes_re, parm = "theta_", method = "profile", oldNames = F) 

# Results indicate that day's CI includes 0, so there is no significant variance
# in time. That is, the time varying effect of illicit drug use ("growth rate in 
# drug use") does not differ between participants.

# Should a RE for time be included in the model?
anova(day_no_re, day_yes_re)

# No evidence for including day as an L1 RE. No evidence of linear trend, but 
# including the time variable is necessary according to Finch et al. (2019)

# LAG OF DV? ---------------------------------------------------------------

# Add lag of DV to control for drug use on previous day
lag_dv_model1 <- glmer(
  DD_any_drug_use ~ 1 + lag_DD_drug_use +
    (1|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)
summary(lag_dv_model1)

# Add lag of DV to control for drug use on previous day
lag_dv_model2 <- glmer(
  DD_any_drug_use ~ 1 + lag_DD_drug_use +
    (1 + lag_DD_drug_use|login_id),
  family = "binomial", 
  data = full_data1,
  nAGQ = 1, # Laplace approximation
  glmerControl(optimizer="bobyqa")
)
summary(lag_dv_model2)

# Should we include a random effect for yesterday's daily drug use?
anova(lag_dv_model1, lag_dv_model2)

# No, do not let the lag of daily drug use vary between person

# LIFETIME STRESSORS ------------------------------------------------------

# 1) Distal Stressors -----------------------------------------------------

# Model with Level 2 lifetime distal stressors
l2_lifetime_distal <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + lag_DD_drug_use + 
    # Add distal stressors 
    EDS_total + ACES_total + IPV_total +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_lifetime_distal)

# ...1.1) Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l2_lifetime_distal, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_lifetime_distal), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 5)

# 2) Proximal Stressors ---------------------------------------------------

# Model with Level 2 lifetime proximal stressors
l2_lifetime_proximal <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add proximal stressors
    IH_mean + TR_CEN_mean +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_lifetime_proximal)

# ...2.1) Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l2_lifetime_proximal, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_lifetime_proximal), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 5)

# 3) Centered Distal Stressors ----------------------------------------------

# Model with Level 2 lifetime distal stressors
l2_mean_distal <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    grand_EDS + grand_ACES + grand_IPV +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_mean_distal)

# ...3.1) Compute CIs AND ORs -----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l2_mean_distal, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l2_mean_distal), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 5)

# 4) Distal * Proximal ----------------------------------------------------

# Model with Level 2 interactions
l2_interactions <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add stressor interactions
    ACES_total*TR_CEN_mean +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_interactions)

# I tested each of the interactions, but the interactions were not significant:
# - ACES_total*TR_CEN_mean
# - IPV_total*TR_CEN_mean
# - EDS_total*IH_mean

# 5) Distal Interactions --------------------------------------------------

# ...5.1) EDS ----------------------------------------------------------------

# Discrimination * Race
l2_eds_race <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    EDS_total*Race +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_eds_race)

# Discrimination * gender
l2_eds_gender <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    EDS_total*Gender +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_eds_gender)

# Discrimination * sexual orientation
l2_eds_sex_or <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    EDS_total*Sex_or +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_eds_sex_or)

# Discrimination * income
l2_eds_income <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    EDS_total*above_25k +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_eds_income)

# Only sexual orientation had significant interactions

# ...5.2) ACES ---------------------------------------------------------------

# ACES * Race
# Because model with as.factor(Race) failed to converge, tried binary predictor
l2_ACES_race <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    ACES_total*is_poc +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_ACES_race)

# ACES * gender
l2_ACES_gender <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    ACES_total*Gender +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_ACES_gender)

# ACES * sexual orientation
l2_ACES_sex_or <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    ACES_total*Sex_or +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_ACES_sex_or)

# ACES * income
l2_ACES_income <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    ACES_total*above_25k +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_ACES_income)

# Once again, only sexual orientation was a significant interaction

# ...5.3) IPV ---------------------------------------------------------------

# IPV * Race
# Because model with as.factor(Race) failed to converge, tried binary predictor
l2_IPV_race <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    IPV_total*is_poc +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_IPV_race)

# IPV * gender
l2_IPV_gender <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    IPV_total*Gender +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_IPV_gender)

# IPV * sexual orientation
l2_IPV_sex_or <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    IPV_total*Sex_or +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_IPV_sex_or)

# IPV * income
l2_IPV_income <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add distal stressors 
    IPV_total*above_25k +
    # Random effects
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l2_IPV_income)

# Significant interactions for:
# Gender (no issues)
# Sexual orientation (but need to fix convergence problems)
# Income (no issues)

# ADD L1 STRESSORS ---------------------------------------------------------

# 1) Test Individual L1 Stressors --------------------------------------------

# ...1.1) General Stress CIs AND ORs ---------------------------------------------

# Individual stressor model - General Stressors
l1_stressors_gen <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_gen_stress_total + 
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_gen)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_gen, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_gen), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# ...1.2) Minority Stress CIs AND ORs --------------------------------------------

# Individual stressor model - Minority Stressors
l1_stressors_minority <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total + 
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_minority)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_minority, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_minority), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# 2) L1 Stressors: Compositional Effects  ------------------------------------

# ...2.1) General Stress CIs AND ORs ---------------------------------------------

# Individual stressor model - General Stressors
l1_stressors_gen <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_gen_stress_total + mean_general_stress +
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_gen)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_gen, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_gen), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# ...2.2) Minority Stress CIs AND ORs ------------------------------------------

# Individual stressor model - Minority Stressors
l1_stressors_minority <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total + mean_minority_stress +
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_minority)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_minority, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_minority), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits=3)

# ...2.3) Trauma Stress CIs AND ORs --------------------------------------------

# Individual stressor model - trauma Stressors
l1_stressors_trauma <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    any_interpersonal_2week + 
    # Random intercept differs across participants, but do not include PTSD
    # because model becomes singular
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_trauma)

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_trauma, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_trauma), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 3)

# 3) Demographic Moderators --------------------------------------------------

# ...3.1) General Stress ---------------------------------------------

# General stressors + race
l1_gen_race <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Use binary variable because model failed to converge with race
    DD_gen_stress_total*is_poc + mean_general_stress*is_poc +
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_gen_race)

# General stressors + gender
l1_gen_gender <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Error when running Gender, will need to investigate
    DD_gen_stress_total*is_transgender + mean_general_stress*is_transgender +
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_gen_gender)

# CONVERGENCE ISSUES WITH SEXUAL ORIENTATION, UNLESS JUST USING MEAN GENERAL 

# General stressors + sexual orientation
l1_gen_sex_or <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_gen_stress_total*Sex_or + mean_general_stress*Sex_or +
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_gen_sex_or)

# General stressors + income
l1_gen_income <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Will run with Income, but place above_25k to be consistent with above
    DD_gen_stress_total*above_25k + mean_general_stress*above_25k +
    # Random intercept and stressor differs across participants
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_gen_income)

# ...3.2) Minority Stress ------------------------------------------

# FIX CONVERGENCE ISSUES 

# Minority Stressors * race
l1_minority_race <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total*is_poc + mean_minority_stress*is_poc +
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_minority_race)

# Minority Stressors * gender
l1_minority_gender <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total*is_transgender + mean_minority_stress*is_transgender +
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_minority_gender)

# Minority Stressors * sexual orientation
l1_minority_sex_or <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total*Sex_or + mean_minority_stress*Sex_or +
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_minority_sex_or)

# Minority Stressors * income
l1_minority_income <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_minority_stress_total*above_25k + mean_minority_stress*above_25k +
    # Random intercept and stressor differs across participants
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_minority_income)

# ...3.3) Trauma Stress --------------------------------------------

# Trauma Stressors * race
l1_trauma_race <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    any_interpersonal_2week*is_poc + 
    # Random intercept differs across participants, but do not include PTSD
    # because model becomes singular
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_trauma_race)

# Trauma Stressors * gender
l1_trauma_gender <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    any_interpersonal_2week*is_transgender + 
    # Random intercept differs across participants, but do not include PTSD
    # because model becomes singular
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_trauma_gender)

# Trauma Stressors * sexual orientation
l1_trauma_sex_or <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    any_interpersonal_2week*Sex_or + 
    # Random intercept differs across participants, but do not include PTSD
    # because model becomes singular
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_trauma_sex_or)

# Trauma Stressors * income
l1_trauma_income <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    any_interpersonal_2week*above_25k + 
    # Random intercept differs across participants, but do not include PTSD
    # because model becomes singular
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_trauma_income)

# ALL L1 STRESSORS --------------------------------------------------------

# 1) Determine if REs -----------------------------------------------------

# Model with Level 1 daily stressors
l1_stressors_a <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # L1 within-person stressors
    DD_gen_stress_total + DD_minority_stress_total +
    # Only include a random intercept (participant daily drug use varies across
    # participants when all predictors are held at 0)
    (1|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_a)

l1_stressors_b <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    DD_gen_stress_total + DD_minority_stress_total +
    # Participant illicit drug use, exposure to general stressors, and exposure
    # to minority stressors varies across participants
    (1 + DD_gen_stress_total + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_b)

# Does the RE model fit better? Is there significant between-person variation
# in stressor exposure?
anova(l1_stressors_a, l1_stressors_b)

# Yes, participants differ in their exposure to daily stressors

# Model with trauma, general stressors, minority stressors
l1_stressors_all <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add all L1 stressors
    DD_gen_stress_total + DD_minority_stress_total + 
    # Compositional effects
    mean_general_stress + mean_minority_stress + any_interpersonal_2week +
    # Remove minority stress RE to stop singularity issue because RE was very 
    # close to zero
    (1 + DD_gen_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(l1_stressors_all)

# 2) Compute CIs AND ORs ----------------------------------------------------

# Compute the confidence intervals for the FE
CI <- confint(l1_stressors_all, parm = "beta_", method = "Wald")

# Combine log-odds with confidence intervals 
logodds <- cbind(OR = fixef(l1_stressors_all), CI)

# Exponentiate the log-odds
ORwCI <- exp(logodds)

# The odds and odds ratios
print(ORwCI, digits = 3)

# L1 + L2 MODEL -----------------------------------------------------------

# Model 
stressors <- glmer(
  # control for previous day's drug use
  DD_any_drug_use ~ 1 + day + lag_DD_drug_use + 
    # Add all L1 stressors
    DD_minority_stress_total*any_interpersonal_2week +
    # Add L2 stressors
    (1 + DD_minority_stress_total|login_id),
  family = "binomial",
  data = full_data1,
  nAGQ = 1, # Use Laplace approximation 
  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(stressors)

# Convert to odds ratios
CI <- confint(stressors, parm = "beta_", method = "Wald")
logodds <- cbind(OR = fixef(stressors), CI)
ORwCI <- exp(logodds)
print(ORwCI, digits = 3)

# NOTES -------------------------------------------------------------------

# - Tried the SM_stress variables, but not significant
# Model with all L1 and L2 stressors is singular
