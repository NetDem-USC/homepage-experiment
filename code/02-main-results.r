#===============================================================================
#  File:    02-main-results.R
#  Date:    Feb 3, 2021
#  Purpose: replicate Figs 3+4 and paper results re: impact of encouragement on
#            key outcome variables
#  Data In: 
#           ./data/survey_data.csv
#           ./data/daily_pulse_data.csv
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(readr)
library(estimatr)
library(glmnet)
library(reshape2)
library(ggplot2)
library(cowplot)
library(stringr)
source('code/functions.r')

# DATA
#===============================================================================
pulse <- read_csv("data/daily_pulse_data.csv")
svy <- read_csv("data/survey_data.csv")

# SAMPLE SIZE
#===============================================================================
# respondents who received the encouragement and consented to wave 4
sum(!is.na(svy$W3_PATA306_treatment_w3) & !is.na(svy$W4_consent))
# Dropping observations where treatment is missing
svy <- svy[!is.na(svy$W3_PATA306_treatment_w3),]

# FIGURE 4
#===============================================================================
# creating affective polarization variable
svy <- svy %>% 
  mutate(affpol_out = case_when(
    party7 < 4 ~ as.integer(W4_PATA450a)-as.integer(W4_PATA450b),
    party7 > 4 & party7 < 8 ~ as.integer(W4_PATA450b)-as.integer(W4_PATA450a),
    TRUE ~ NA_integer_),
         affpol_pre = case_when(
           party7 < 4 ~ as.integer(W2_PATA2_3_Dem)-as.integer(W2_PATA2_3_Rep),
    party7 > 4 & party7 < 8 ~ as.integer(W2_PATA2_3_Rep)-as.integer(W2_PATA2_3_Dem),
    TRUE ~ NA_integer_))

# standardize DVs
svy$trust <- svy$trust/sd(svy$trust[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$issue_scale <- svy$issue_scale/sd(svy$issue_scale[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$imm_issue_scale <- svy$imm_issue_scale/sd(svy$imm_issue_scale[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$W4_PATA450b <- svy$W4_PATA450b/sd(svy$W4_PATA450b[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$W4_PATA450a <- svy$W4_PATA450a/sd(svy$W4_PATA450a[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$affpol_out <- svy$affpol_out/sd(svy$affpol_out[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$vote_after <- svy$vote_after/sd(svy$vote_after[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$ppol <- svy$ppol/sd(svy$ppol[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$agenda_lean <- svy$agenda_lean/sd(svy$agenda_lean[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

svy$trump_approve_num <- as.numeric(svy$trump_approve)
svy$cong_dem_approve_num <- as.numeric(svy$cong_dem_approve)
svy$cong_rep_approve_num <- as.numeric(svy$cong_rep_approve)

svy$trump_approve_num <- svy$trump_approve_num/sd(svy$trump_approve_num[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$cong_dem_approve_num <- svy$cong_dem_approve_num/sd(svy$cong_dem_approve_num[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)
svy$cong_rep_approve_num <- svy$cong_rep_approve_num/sd(svy$cong_rep_approve_num[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

# potential control variables across all models
vars <- c("party7", "age", "agesq", "female", "raceeth", "educ",
          "ideo", "income", "employ", "state", "polint", "freq_tv", "freq_np", 
          "freq_rad", "freq_net", "freq_disc", "log_news_pre", "diet_mean_pre")

ests <- NULL

########################################################
### Issue H1: issue opinions, Fox News treatment
########################################################

trt <- "FoxNews"
dv <- "issue_scale"
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

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Issue scale (conservatism)"
dat0$trt <- "Fox News"
dat$outcome <- "Issue scale (conservatism)"
dat$trt <- "Fox News"

ests <- rbind(ests, dat, dat0)

########################################################
### Issue H2: issue opinions, HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "issue_scale"
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

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Issue scale (conservatism)"
dat0$trt <- "HuffPost"
dat$outcome <- "Issue scale (conservatism)"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)


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

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Self-reported vote turnout"
dat0$trt <- "Fox News"
dat$outcome <- "Self-reported vote turnout"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

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

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Self-reported vote turnout"
dat0$trt <- "HuffPost"
dat$outcome <- "Self-reported vote turnout"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)


########################################################
### Affective H1b: HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "affpol_out"
dv_pre <- "affpol_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Affective polarization"
dat0$trt <- "HuffPost"
dat$outcome <- "Affective polarization"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)

########################################################
### Affective H2a: FoxNews treatment
########################################################

trt <- "FoxNews"
dv <- "affpol_out" 
dv_pre <- "affpol_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt)

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Affective polarization"
dat0$trt <- "Fox News"
dat$outcome <- "Affective polarization"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)


########################################################
### Media Trust H4a: Media trust for FN group
########################################################

trt <- "FoxNews"
dv <- "trust"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Media trust"
dat0$trt <- "Fox News"
dat$outcome <- "Media trust"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

########################################################
### Media Trust H4b: Media trust for HP group
########################################################

trt <- "HuffPost"
dv <- "trust"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Media trust"
dat0$trt <- "HuffPost"
dat$outcome <- "Media trust"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)


########################################################
### Issue H3: immigration attitudes, FoxNews treatment
########################################################

trt <- "FoxNews"
dv <- "imm_issue_scale"
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

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Pro-immigration attitudes"
dat0$trt <- "Fox News"
dat$outcome <- "Pro-immigration attitudes"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

########################################################
### Issue H4: immigration attitudes, HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "imm_issue_scale"
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

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Pro-immigration attitudes"
dat0$trt <- "HuffPost"
dat$outcome <- "Pro-immigration attitudes"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)

########################################################
### Perceived H1a: FoxNews treatment
########################################################

trt <- "FoxNews"
dv <- "ppol"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Perceived polarization"
dat0$trt <- "Fox News"
dat$outcome <- "Perceived polarization"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

########################################################
### Perceived H1b: HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "ppol" 
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Perceived polarization"
dat0$trt <- "HuffPost"
dat$outcome <- "Perceived polarization"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)


########################################################
### Agenda H1: FoxNews treatment
########################################################

trt <- "FoxNews"
dv <- "agenda_lean"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Agenda setting"
dat0$trt <- "Fox News"
dat$outcome <- "Agenda setting"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

########################################################
### Agenda H2: HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "agenda_lean"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Agenda setting"
dat0$trt <- "HuffPost"
dat$outcome <- "Agenda setting"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)

########################################################
### Approval H1a: President
########################################################

# creating numeric dvs to avoid labelled error
# svy$trump_approve_num <- as.numeric(svy$trump_approve)
svy$trump_approve_pre_num <- as.numeric(svy$trump_approve_pre)

trt <- "FoxNews"
dv <- "trump_approve_num"
dv_pre <- "trump_approve_pre_num"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Approval: President"
dat0$trt <- "Fox News"
dat$outcome <- "Approval: President"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

########################################################
### Approval H1b: Republicans in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
# svy$cong_rep_approve_num <- as.numeric(svy$cong_rep_approve)
svy$cong_rep_approve_pre_num <- as.numeric(svy$cong_rep_approve_pre)

trt <- "FoxNews"
dv <- "cong_rep_approve_num"
dv_pre <- "cong_rep_approve_pre_num"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Approval: Reps in Congress"
dat0$trt <- "Fox News"
dat$outcome <- "Approval: Reps in Congress"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

########################################################
### Approval H1c: Democrats in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
# svy$cong_dem_approve_num <- as.numeric(svy$cong_dem_approve)
svy$cong_dem_approve_pre_num <- as.numeric(svy$cong_dem_approve_pre)

trt <- "FoxNews"
dv <- "cong_dem_approve_num"
dv_pre <- "cong_dem_approve_pre_num"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Approval: Dems in Congress"
dat0$trt <- "Fox News"
dat$outcome <- "Approval: Dems in Congress"
dat$trt <- "Fox News"
ests <- rbind(ests, dat, dat0)

########################################################
### Approval H2a: President
########################################################

# creating numeric dvs to avoid labelled error
# svy$trump_approve_num <- as.numeric(svy$trump_approve)
svy$trump_approve_pre_num <- as.numeric(svy$trump_approve_pre)

trt <- "HuffPost"
dv <- "trump_approve_num"
dv_pre <- "trump_approve_pre_num"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Approval: President"
dat0$trt <- "HuffPost"
dat$outcome <- "Approval: President"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)

########################################################
### Approval H2b: Republicans in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
# svy$cong_rep_approve_num <- as.numeric(svy$cong_rep_approve)
svy$cong_rep_approve_pre_num <- as.numeric(svy$cong_rep_approve_pre)

trt <- "HuffPost"
dv <- "cong_rep_approve_num"
dv_pre <- "cong_rep_approve_pre_num"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat <- dat %>% mutate(estimand = "CACE")
dat0$outcome <- "Approval: Reps in Congress"
dat0$trt <- "HuffPost"
dat$outcome <- "Approval: Reps in Congress"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)

########################################################
### Approval H2c: Democrats in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
# svy$cong_dem_approve_num <- as.numeric(svy$cong_dem_approve)
svy$cong_dem_approve_pre_num <- as.numeric(svy$cong_dem_approve_pre)

trt <- "HuffPost"
dv <- "cong_dem_approve"
dv_pre <- "cong_dem_approve_pre"
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

dat0 <- itt[[1]] %>% tidy() %>% slice(2) %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "ITT")
dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value) %>%
  mutate(estimand = "CACE")
dat0$outcome <- "Approval: Dems in Congress"
dat0$trt <- "HuffPost"
dat$outcome <- "Approval: Dems in Congress"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat, dat0)

ests$outcome[grep("immig", ests$outcome)] <- "Immigration issue scale"
ests$study <- "Study 1"
ests$study[13:16] <- "Study 2"
ests$outcome_factor <- factor(ests$outcome, levels=rev(unique(ests$outcome)))


########################################################
### Creating figure in paper
########################################################

g <- ggplot(data = filter(ests, estimand == "CACE"), aes(y=estimate, x=outcome_factor, color=trt)) + 
  facet_grid(rows = vars(study), space = "free", scales = "free") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high), position=position_dodge(width=-.5))+
  scale_color_manual("Treatment", values=c("red", "blue")) +
  ylim(c(-1.33, 1.33)) + 
  xlab("") + ylab("") + 
  coord_flip() + theme_minimal() + theme(legend.position = "bottom")
g

ggsave(g, file = "graphs/main-fig4.pdf", width = 6, height = 8)
ggsave(g, file = "graphs/main-fig4.png", width = 6, height = 8)

# results highlighted in manuscript
ests %>% filter(estimand == "CACE" & outcome_factor == "Media trust")


# Long-term effects
#===============================================================================

svy$trust_w7 <- svy$trust_w7/sd(svy$trust_w7[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

########################################################
### Media Trust H4a: Media trust for FN group (w7)
########################################################

trt <- "FoxNews"
dv <- "trust_w7"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)
cace$coefficients[D]
cace$p.value[D]

########################################################
### Media Trust H4b: Media trust for HP group (w7)
########################################################

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
cace$coefficients[D]
cace$p.value[D]

########################################################
### Media Trust H4a: Media trust for FN group (w8)
########################################################

svy$trust_w8 <- svy$trust_w8/sd(svy$trust_w8[which(svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

trt <- "FoxNews"
dv <- "trust_w8"
D <- "comp_fn"

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
cace$coefficients[D]
cace$p.value[D]

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
cace$coefficients[D]
cace$p.value[D]

# substantive magnitude?
svy %>% mutate(ideo2 = case_when(
  ideo %in% c(1, 2) ~ 'Liberals',
  ideo %in% c(4, 5) ~ 'Conservatives')) %>% 
  group_by(ideo2) %>% 
  summarize(avg_trust = mean(trust, na.rm = TRUE))
# difference:
3.33 - 1.91 # = 1.42
.55 / 1.42


