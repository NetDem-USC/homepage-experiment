#===============================================================================
#  File:    06-additional-results.R
#  Date:    Feb 3, 2021
#  Purpose: replicate appendix analyses: Sections 5 and 6
#  Data In: 
#           ./data/survey_data.csv
#           ./data/daily_pulse_data.csv
#===============================================================================

# PACKAGES
#===============================================================================
library(tidyverse)
library(estimatr)
library(glmnet)
library(powerLATE)
library(haven)
source('code/functions.r')

# DATA
#===============================================================================
pulse <- read_csv("data/daily_pulse_data.csv")
svy <- read_csv("data/survey_data.csv")

# Dropping observations where treatment is missing
svy <- svy[!is.na(svy$W3_PATA306_treatment_w3),]

# Analysis ----------------------------------------------------------------

# https://declaredesign.org/blog/biased-fixed-effects.html

vars <- c("party7", "age", "agesq", "female", "raceeth", "educ",
          "ideo", "income", "employ", "state", "polint", "freq_tv", "freq_np", 
          "freq_rad", "freq_net", "freq_disc", "log_news_pre", "diet_mean_pre")

########################################################
### Issue H1: issue opinions, Fox News treatment
########################################################

trt <- "FoxNews"
dv <- "issue_scale_w5"
dv_pre <- "issue_scale_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Conservatism")



########################################################
### Issue H2: issue opinions, HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "issue_scale_w5"
dv_pre <- "issue_scale_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: Conservatism")

########################################################
### Issue H3: immigration attitudes, FoxNews treatment
########################################################

trt <- "FoxNews"
dv <- "imm_issue_scale_w5"
dv_pre <- "issue_scale_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "policy14_imm_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H3: Pro-immigration")

########################################################
### Issue H4: immigration attitudes, HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "imm_issue_scale_w5"
dv_pre <- "issue_scale_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "policy14_imm_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4: Pro-immigration")

########################################################
### Issue RQ1: heterogeneous treatment effects
########################################################

# H1
heterogeneous_effect(dv = "issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "HuffPost")
# H3
heterogeneous_effect(dv = "imm_issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "imm_issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "FoxNews")
# H4
heterogeneous_effect(dv = "imm_issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "imm_issue_scale_w5", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "HuffPost")


########################################################
### Agenda H1: FoxNews treatment
########################################################

trt <- "FoxNews"
dv <- "agenda_lean_w5"
dv_pre <- "agenda_lean_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Agenda setting")

########################################################
### Agenda H2: HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "agenda_lean_w5"
dv_pre <- "agenda_lean_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace,  trt, pwr,
              dv = "H2: Agenda setting")

########################################################
### Agenda RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1
heterogeneous_effect(dv = "agenda_lean_w5", dv_pre = "agenda_lean_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "agenda_lean_w5", dv_pre = "agenda_lean_pre",
                     moderator = "ideo", trt = "FoxNews")

# H2
heterogeneous_effect(dv = "agenda_lean_w5", dv_pre = "agenda_lean_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "agenda_lean_w5", dv_pre = "agenda_lean_pre",
                     moderator = "ideo", trt = "HuffPost")

########################################################
### Agenda RQ2: heterogeneous treatment effects by pre-treatment habits
########################################################

# H1
heterogeneous_effect(dv = "agenda_lean_w5", dv_pre = "log_fn_pre",
                     moderator = "ideo", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "agenda_lean_w5", dv_pre = "log_hp_pre",
                     moderator = "ideo", trt = "HuffPost")


########################################################
### Approval H1a: President
########################################################

trt <- "FoxNews"
dv <- "trump_approve_w5"
dv_pre <- "trump_approve_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1a: Pres. approval")


########################################################
### Approval H2a: President
########################################################

trt <- "HuffPost"
dv <- "trump_approve_w5"
dv_pre <- "trump_approve_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2a: Pres. approval")


########################################################
### Approval RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1a
heterogeneous_effect(dv = "trump_approve_w5", dv_pre = "trump_approve_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "trump_approve_w5", dv_pre = "trump_approve_pre",
                     moderator = "ideo", trt = "FoxNews")

# H2a
heterogeneous_effect(dv = "trump_approve_w5", dv_pre = "trump_approve_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "trump_approve_w5", dv_pre = "trump_approve_pre",
                     moderator = "ideo", trt = "HuffPost")

########################################################
### Turnout RQ1a: Fox News
########################################################

trt <- "FoxNews"
dv <- "vote_after"
dv_pre <- "vote_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "RQ1a: Turnout")

########################################################
### Turnout RQ1a: Huff Post
########################################################

trt <- "HuffPost"
dv <- "vote_after"
dv_pre <- "vote_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "RQ1b: Turnout")

########################################################
### Media Trust H1a: Trust in Fox News for FN treatment
########################################################

trt <- "FoxNews"
dv <- "trust_fox_w5"
dv_pre <- ""
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1a: Trust in Fox News")

########################################################
### Media Trust H1b: Trust in HuffPost for FN treatment
########################################################

trt <- "FoxNews"
dv <- "trust_hp_w5"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1b: Trust in HuffPost")

########################################################
### Media Trust H2a: Trust in HuffPost for HP treatment
########################################################

trt <- "HuffPost"
dv <- "trust_hp_w5"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2a: Trust in HuffPost")

########################################################
### Media Trust H2b: Trust in Fox News for HP treatment
########################################################

trt <- "HuffPost"
dv <- "trust_fox_w5"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2b: Trust in Fox News")


########################################################
### Media Trust RQ1
########################################################

# H1a
heterogeneous_effect(dv = "trust_fox_w5", 
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "trust_fox_w5", 
                     moderator = "ideo", trt = "FoxNews")
# H1b
heterogeneous_effect(dv = "trust_hp_w5", 
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "trust_hp_w5", 
                     moderator = "ideo", trt = "FoxNews")
# H2a
heterogeneous_effect(dv = "trust_hp_w5", 
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "trust_hp_w5", 
                     moderator = "ideo", trt = "HuffPost")
# H2b
heterogeneous_effect(dv = "trust_fox_w5", 
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "trust_fox_w5", 
                     moderator = "ideo", trt = "HuffPost")


########################################################
### Media Trust H4a: Media trust for FN group (w7)
########################################################

trt <- "FoxNews"
dv <- "trust_w7"
dv_pre <- ""
D <- "comp_fn"

# svy$trust_w7 <- zap_labels(svy$trust_w7)

# resetting after het fx analysis
vars <- c("party7", "age", "agesq", "female", "raceeth", "educ",
          "ideo", "income", "employ", "state", "polint", "freq_tv", "freq_np", 
          "freq_rad", "freq_net", "freq_disc", "log_news_pre", "diet_mean_pre")

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4a: Media trust (W7)")

########################################################
### Media Trust H4b: Media trust for HP group (w7)
########################################################

# svy$trust_w7 <- svy$trust_w7/sd(svy$trust_w7[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

trt <- "HuffPost"
dv <- "trust_w7"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4b: Media trust (W7)")



########################################################
### Factual knowledge RQ1a: news reception for HP group
########################################################

trt <- "HuffPost"
dv <- "event_mokken_w5"
dv_pre <- "event_pre_mokken"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, dv_pre = dv_pre)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "RQ1a: Event knowledge")

########################################################
### Factual knowledge RQ1b: news reception for FN group
########################################################

trt <- "FoxNews"
dv <- "event_mokken_w5"
dv_pre <- "event_pre_mokken"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, dv_pre = dv_pre)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "RQ1b: Event knowledge")

# RQ1a
heterogeneous_effect(dv = "event_mokken_w5", dv_pre = "event_pre_mokken",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "event_mokken_w5", dv_pre = "event_pre_mokken",
                     moderator = "ideo", trt = "HuffPost")

# RQ1b
heterogeneous_effect(dv = "event_mokken_w5", dv_pre = "event_pre_mokken",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "event_mokken_w5", dv_pre = "event_pre_mokken",
                     moderator = "ideo", trt = "FoxNews")


#### WAVE 7 ####

########################################################
### Issue H1: issue opinions, Fox News treatment (w7)
########################################################

trt <- "FoxNews"
dv <- "issue_scale_w7"
dv_pre <- "issue_scale_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Conservatism")

########################################################
### Issue H2: issue opinions, HuffPost treatment (w7)
########################################################

trt <- "HuffPost"
dv <- "issue_scale_w7"
dv_pre <- "issue_scale_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: Conservatism")

########################################################
### Issue H3: immigration attitudes, FoxNews treatment (w7)
########################################################

trt <- "FoxNews"
dv <- "imm_issue_scale_w7"
dv_pre <- "issue_scale_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "policy14_imm_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H3: Pro-immigration")

########################################################
### Issue H4: immigration attitudes, HuffPost treatment (w7)
########################################################

trt <- "HuffPost"
dv <- "imm_issue_scale_w7"
dv_pre <- "issue_scale_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "policy14_imm_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4: Pro-immigration")

########################################################
### Issue RQ1: heterogeneous treatment effects (w7)
########################################################

# H1
heterogeneous_effect(dv = "issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "HuffPost")
# H3
heterogeneous_effect(dv = "imm_issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "imm_issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "FoxNews")
# H4
heterogeneous_effect(dv = "imm_issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "imm_issue_scale_w7", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "HuffPost")



########################################################
### Agenda H1: FoxNews treatment (w8)
########################################################

trt <- "FoxNews"
dv <- "agenda_lean_w8"
dv_pre <- "agenda_lean_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Agenda setting")

########################################################
### Agenda H2: HuffPost treatment (w8)
########################################################

trt <- "HuffPost"
dv <- "agenda_lean_w8"
dv_pre <- "agenda_lean_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace,  trt, pwr,
              dv = "H2: Agenda setting")

########################################################
### Agenda RQ1: heterogeneous treatment effects by party ID / ideology (w8)
########################################################

# H1
heterogeneous_effect(dv = "agenda_lean_w8", dv_pre = "agenda_lean_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "agenda_lean_w8", dv_pre = "agenda_lean_pre",
                     moderator = "ideo", trt = "FoxNews")

# H2
heterogeneous_effect(dv = "agenda_lean_w8", dv_pre = "agenda_lean_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "agenda_lean_w8", dv_pre = "agenda_lean_pre",
                     moderator = "ideo", trt = "HuffPost")

########################################################
### Agenda RQ2: heterogeneous treatment effects by pre-treatment habits (w8)
########################################################

# H1
heterogeneous_effect(dv = "agenda_lean_w8", dv_pre = "log_fn_pre",
                     moderator = "ideo", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "agenda_lean_w8", dv_pre = "log_hp_pre",
                     moderator = "ideo", trt = "HuffPost")




########################################################
### Media Trust H4a: Media trust for FN group (w8)
########################################################

# svy$trust_w8 <- svy$trust_w8/sd(svy$trust_w8[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

trt <- "FoxNews"
dv <- "trust_w8"
D <- "comp_fn"

# svy$trust_w8 <- zap_labels(svy$trust_w8)

# resetting after het fx analysis
vars <- c("party7", "age", "agesq", "female", "raceeth", "educ",
          "ideo", "income", "employ", "state", "polint", "freq_tv", "freq_np", 
          "freq_rad", "freq_net", "freq_disc", "log_news_pre", "diet_mean_pre")

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4a: Media trust (W8)")

########################################################
### Media Trust H4b: Media trust for HP group (w8)
########################################################

trt <- "HuffPost"
dv <- "trust_w8"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4b: Media trust (W8)")