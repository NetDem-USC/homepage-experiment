#===============================================================================
#  File:    08-treatment-group-comparisons.R
#  Date:    Feb 3, 2021
#  Purpose: replicate appendix analyses: Section 7
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

# For all following analysis, HuffPost: treatment, FoxNews:control

########################################################
### Issue H1 & H2: issue opinions
########################################################

trt <- "HuffPost"
dv <- "issue_scale"
dv_pre <- "issue_scale_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv= "H1 & H2: Conservatism")


########################################################
### Issue H3 & H4: immigration attitudes
########################################################

trt <- "HuffPost"
dv <- "imm_issue_scale"
dv_pre <- "issue_scale_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, more_vars = "policy14_imm_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H3 & H4: Pro-immigration")


########################################################
### Issue RQ1: heterogeneous treatment effects
########################################################

# H1 & H2
heterogeneous_effect2(dv = "issue_scale", dv_pre = "issue_scale_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "issue_scale", dv_pre = "issue_scale_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H3 & H4
heterogeneous_effect2(dv = "imm_issue_scale", dv_pre = "issue_scale_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "imm_issue_scale", dv_pre = "issue_scale_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")


########################################################
### News H1: news browsing, 1 week
########################################################

trt <- "HuffPost"
dv <- "log_cons_1w"
dv_pre <- "log_cons_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H1: Cons. news (1 week)")

########################################################
### News H2: news browsing, 1 week
########################################################

trt <- "HuffPost"
dv <- "log_lib_1w"
dv_pre <- "log_lib_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H2: Lib. news (1 week)")


########################################################
### News H1: news browsing, 4 weeks
########################################################

trt <- "HuffPost"
dv <- "log_cons_4w"
dv_pre <- "log_cons_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt,  control = control, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H1: Cons. news (4 weeks)")

########################################################
### News H2: news browsing, 4 week
########################################################

trt <- "HuffPost"
dv <- "log_lib_4w"
dv_pre <- "log_lib_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H2: Lib. news (4 weeks)")

########################################################
### News H1: news browsing, 6 weeks
########################################################

trt <- "HuffPost"
dv <- "log_cons_6w"
dv_pre <- "log_cons_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H1: Cons. news (6 weeks)")

########################################################
### News H2: news browsing, 6 week
########################################################

trt <- "HuffPost"
dv <- "log_lib_6w"
dv_pre <- "log_lib_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H2: Lib. news (6 weeks)")



########################################################
### News RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1: n/s
heterogeneous_effect2(dv = "log_cons_1w", dv_pre = "log_cons_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "log_cons_1w", dv_pre = "log_cons_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H2: n/s
heterogeneous_effect2(dv = "log_lib_1w", dv_pre = "log_lib_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "log_lib_1w", dv_pre = "log_lib_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

########################################################
### News RQ2: heterogeneous treatment effects by pre-treatment news consumption
########################################################

# H1: n/s
heterogeneous_effect2(dv = "log_cons_1w", dv_pre = "log_hp_pre",
                      moderator = "log_fn_pre", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "log_cons_1w", dv_pre = "log_fn_pre",
                      moderator = "log_fn_pre", trt = "HuffPost", control = "FoxNews")

# H2: +
heterogeneous_effect2(dv = "log_lib_1w", dv_pre = "log_hp_pre",
                      moderator = "log_hp_pre", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "log_lib_1w", dv_pre = "log_fn_pre",
                      moderator = "log_hp_pre", trt = "HuffPost", control = "FoxNews")


########################################################
### Twitter H1: conservative link shares
########################################################

# creating Twitter variables
svy$log_cons_links <- log(svy$cons_links+1)
svy$log_cons_links_pre <- log(svy$cons_links_pre+1)
svy$log_total_links_pre <- log(svy$total_links_pre+1)

trt <- "HuffPost"
dv <- "log_cons_links"
dv_pre <- "log_cons_links_pre"
control <- "FoxNews"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, blocks="chrome", more_vars = "log_total_links_pre")
compute_proportion_missing_covars(itt)

format_latex3(dim, itt, dv = "H1: Tweets w/cons. links")

########################################################
### Twitter H2: liberal link shares
########################################################

# creating Twitter variables
svy$log_lib_links <- log(svy$lib_links+1)
svy$log_lib_links_pre <- log(svy$lib_links_pre+1)
svy$log_total_links_pre <- log(svy$total_links_pre+1)

trt <- "HuffPost"
dv <- "log_lib_links"
dv_pre <- "log_lib_links_pre"
control <- "FoxNews"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)
## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control, blocks="chrome", more_vars = "log_total_links_pre")

format_latex3(dim, itt, dv = "H2: Tweets w/lib. links")

########################################################
### Twitter H3: follow conservative news sources
########################################################

# creating Twitter variables
svy$log_follows_cons_media <- log(svy$follows_cons_media+1)
svy$log_follows_elites <- log(svy$follows_elites+1)

trt <- "HuffPost"
dv <- "log_follows_cons_media"
control <- "FoxNews"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)
## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control, blocks="chrome", more_vars = "log_follows_elites")

format_latex3(dim, itt, dv = "H3: Follow cons. media")

########################################################
### Twitter H4: follow liberal news sources
########################################################

# creating Twitter variables
svy$log_follows_lib_media <- log(svy$follows_lib_media+1)
svy$log_follows_elites <- log(svy$follows_elites+1)

trt <- "HuffPost"
dv <- "log_follows_lib_media"
control <- "FoxNews"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)
## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control, blocks="chrome", more_vars = "log_follows_elites")

format_latex3(dim, itt, dv = "H4: Follow lib. media")

########################################################
### Affective H1a & H2a: Democrats, HuffPost treatment
########################################################

# creating new variables
svy$W4_PATA450a_new <- zap_labels(svy$W4_PATA450a)
svy$W2_PATA2_3_Dem_new <- zap_labels(svy$W2_PATA2_3_Dem)

trt <- "HuffPost"
dv <- "W4_PATA450a_new" # FT Democrats
dv_pre <- "W2_PATA2_3_Dem_new"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1a \\& H2a: Dem. thermometer")

########################################################
### Affective H1b & H2b: Republicans, HuffPost treatment
########################################################

# creating new variables
svy$W4_PATA450b_new <- zap_labels(svy$W4_PATA450b)
svy$W2_PATA2_3_Rep_new <- zap_labels(svy$W2_PATA2_3_Rep)

trt <- "HuffPost"
dv <- "W4_PATA450b_new" # FT Republicans
dv_pre <- "W2_PATA2_3_Rep_new"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1b \\& H2b: Rep. thermometer")

########################################################
### Affective H1c & H2c: Trump supporters, HuffPost treatment
########################################################

# creating new variables
svy$W4_PATA450e_new <- zap_labels(svy$W4_PATA450e)
svy$W2_PATA2_6_new <- zap_labels(svy$W2_PATA2_6)

trt <- "HuffPost"
dv <- "W4_PATA450e_new" # Trump social distance
dv_pre <- "W2_PATA2_6_new"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1c \\& H2c: Trump distance")

########################################################
### Polarization RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1a
heterogeneous_effect2(dv = "W4_PATA450a_new", dv_pre = "W2_PATA2_3_Dem_new",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "W4_PATA450a_new", dv_pre = "W2_PATA2_3_Dem_new",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H1b
heterogeneous_effect2(dv = "W4_PATA450b_new", dv_pre = "W2_PATA2_3_Rep_new",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "W4_PATA450b_new", dv_pre = "W2_PATA2_3_Rep_new",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

# H1c
heterogeneous_effect2(dv = "W4_PATA450e_new", dv_pre = "W2_PATA2_6_new",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "W4_PATA450e_new", dv_pre = "W2_PATA2_6_new",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

########################################################
### Perceived H1a & H1b: HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "ppol"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1a \\& H1b: Perceived polarization")

########################################################
### Perceived RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1a & H1b
heterogeneous_effect2(dv = "ppol", 
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "ppol", 
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

########################################################
### Agenda H1 & H2: HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "agenda_lean"
dv_pre <- "agenda_lean_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1 \\& H2: Agenda setting")

########################################################
### Agenda RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1 & H2
heterogeneous_effect2(dv = "agenda_lean", dv_pre = "agenda_lean_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "agenda_lean", dv_pre = "agenda_lean_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")


########################################################
### Agenda RQ2: heterogeneous treatment effects by pre-treatment habits
########################################################

# H1
heterogeneous_effect2(dv = "agenda_lean", dv_pre = "log_fn_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H2
heterogeneous_effect2(dv = "agenda_lean", dv_pre = "log_hp_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")


########################################################
### Approval H1a & H2a: President
########################################################

trt <- "HuffPost"
dv <- "trump_approve"
dv_pre <- "trump_approve_pre"
control = "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1a \\& H2a: Pres. approval")

########################################################
### Approval H1b & H2b: Republicans in Congress [Congress control preference]
########################################################

trt <- "HuffPost"
dv <- "cong_rep_approve"
dv_pre <- "cong_rep_approve_pre"
control = "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1b \\& H2b: Rep. Cong. pref.")

########################################################
### Approval H1c & H2c: Democrats in Congress [Congress control preference]
########################################################

trt <- "HuffPost"
dv <- "cong_dem_approve"
dv_pre <- "cong_dem_approve_pre"
control = "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1c \\& H2c: Dem. Cong. pref.")

########################################################
### Approval RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1a & H2a
heterogeneous_effect2(dv = "trump_approve", dv_pre = "trump_approve_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "trump_approve", dv_pre = "trump_approve_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H1b & H2b
heterogeneous_effect2(dv = "cong_rep_approve", 
                      dv_pre = "cong_rep_approve_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "cong_rep_approve", 
                      dv_pre = "cong_rep_approve_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H1c & H2c
heterogeneous_effect2(dv = "cong_dem_approve", 
                      dv_pre = "cong_dem_approve_pre",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "cong_dem_approve", 
                      dv_pre = "cong_dem_approve_pre",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

########################################################
### Turnout RQ1a: Huff Post
########################################################

trt <- "HuffPost"
dv <- "vote_after"
dv_pre <- "vote_pre"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, control = control)

format_latex3(dim, itt, dv = "RQ1a: Turnout")

########################################################
### Media Trust H1a & H2a: Trust in Fox News for HuffPost treatment
########################################################

trt <- "HuffPost"
dv <- "trust_fox"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1a \\& H2a: Trust in Fox News")

########################################################
### Media Trust H1b & H2b: Trust in HuffPost for FN treatment
########################################################

trt <- "HuffPost"
dv <- "trust_hp"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1b \\& H2b: Trust in HuffPost")


########################################################
### Media Trust H3a & H3b: Media liberal bias for HP treatment
########################################################

trt <- "HuffPost"
dv <- "bias"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H3a \\& H3b: Liberal media bias")

########################################################
### Media Trust RQ1
########################################################

# H1a & H2a
heterogeneous_effect2(dv = "trust_fox", 
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "trust_fox", 
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H1b & H2b 
heterogeneous_effect2(dv = "trust_hp", 
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "trust_hp", 
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")
# H3a & H3b
heterogeneous_effect2(dv = "bias", 
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "bias", 
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

########################################################
### Media Trust H4a: Media trust for HP group
########################################################

trt <- "HuffPost"
dv <- "trust"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H4a: Media trust")

########################################################
### Media Trust H4a: Media trust for HP group (w7)
########################################################

trt <- "HuffPost"
dv <- "trust_w7"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H4a: Media trust (w7)")

########################################################
### Factual knowledge H1: % foreign born
########################################################

trt <- "HuffPost"
dv <- "W4_PATA430a"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1: \\% foreign born")

########################################################
### Factual knowledge H2: % unemployment rate
########################################################

trt <- "HuffPost"
dv <- "W4_PATA430b"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H2: \\% unemployment")


########################################################
### Factual knowledge RQ1a & RQ1b: news reception for HP group
########################################################

trt <- "HuffPost"
dv <- "event_mokken"
dv_pre <- "event_pre_mokken"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, dv_pre = dv_pre, control = control)

format_latex3(dim, itt, dv = "RQ1a \\& RQ1b: Event knowledge")


########################################################
### Knowledge RQ2
########################################################

# H1
heterogeneous_effect2(dv = "W4_PATA430a", 
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "W4_PATA430a", 
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

# H2
heterogeneous_effect2(dv = "W4_PATA430b", 
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "W4_PATA430b", 
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")

# RQ1a & RQ1b
heterogeneous_effect2(dv = "event_mokken", dv_pre = "event_pre_mokken",
                      moderator = "party7", trt = "HuffPost", control = "FoxNews")
heterogeneous_effect2(dv = "event_mokken", dv_pre = "event_pre_mokken",
                      moderator = "ideo", trt = "HuffPost", control = "FoxNews")


########################################################
### Factual knowledge RQ1b: immigration knowledge for HP group
########################################################

svy$know_immig_w2_TRUE <- svy$W2_PATA2_10_m_3 == 1
svy$know_immig_w4_TRUE <- svy$W4_PATA409_1 == 1

trt <- "HuffPost"
dv <- "know_immig_w4_TRUE"
dv_pre <- "know_immig_w2_TRUE"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, dv_pre = dv_pre, control = control)

format_latex3(dim, itt, dv = "RQ1b: Immigration event knowledge")


########################################################
### Election prediction H1
########################################################

# Subjects assigned to the Fox News treatment...

# H1a & H2a: ... will be more likely to predict the Republican Party as winning the control of the House of Representatives
trt <- "HuffPost"
dv <- "predicted_house_winner_gop"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1a \\& H2a: \\ GOP House winner")


# H1b & H2b: ... will predict a higher House vote share for the Republican Party 

trt <- "HuffPost"
dv <- "predicted_vote_share_gop"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1b \\& H2b: \\ GOP House vote share")


# H1c & H2c: ... will be more likely to predict the Republican candidate to win the race for the House seat in their Congressional district

trt <- "HuffPost"
dv <- "predicted_district_winner_gop"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)

format_latex3(dim, itt, dv = "H1c \\& H2c: \\ GOP House district winner")




########################################################
### Election prediction H3
########################################################

# (a) Pre-treatment Republican partisans in the Fox News treatment group and (b) pre-treatment Democratic partisans in the HuffPost treatment group will report a higher average certainty in their predictions than subjects assigned to the control group.
## We revised this hypothesis to be able to compare the HuffPost and the FoxNews groups directly
## Pre-treatment Democratic partisans in the HuffPost treatment group will report a higher average certainty in their predictions than pre-treatment Republican partisans in the Fox News treatment group.

svy_sub <- filter(svy, (W3_PATA306_treatment_w3 == "FoxNews" & partylean == "Republican") | (W3_PATA306_treatment_w3 == "HuffPost" & partylean == "Democrat"))

trt <- "HuffPost"
dv <- "prediction_certainty"
control <- "FoxNews"

## DIM WITHOUT BLOCKING (data sparsity issue)
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            #blocks = W3_Browser_treatment_w3, 
                            data = svy_sub, 
                            condition1 = control, 
                            condition2 = trt))

## ITT WITHOUT Lin's covariate adjustment (data sparsity issue)
form <- as.formula("prediction_certainty ~ W3_PATA306_treatment_w3")
# itt <- lm_robust(form, data = dplyr::filter(svy_sub, W3_PATA306_treatment_w3 != "Control"))
itt <- lm_robust(form, svy_sub)

format_latex3(dim, list(itt), dv = "H3a \\& H3b: \\ Prediction certainty")

########################################################
### Election prediction H4
########################################################

# (a) # Pre-treatment Republican partisans in the HuffPost treatment group and (b) pre-treatment Democratic partisans in the Fox News treatment group will report a lower average certainty in their predictions than subjects assigned to the control group.
## We revised this hypothesis to be able to compare the HuffPost and the FoxNews groups directly
## Pre-treatment Republican partisans in the HuffPost treatment group will report a higher average certainty in their predictions than pre-treatment Democratic partisans in the Fox News treatment group.

svy_sub <- filter(svy, (W3_PATA306_treatment_w3 == "FoxNews" & partylean == "Democrat") | (W3_PATA306_treatment_w3 == "HuffPost" & partylean == "Republican"))

trt <- "HuffPost"
dv <- "prediction_certainty"
control <- "FoxNews"

## DIM WITHOUT BLOCKING (data sparsity issue)
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            #blocks = W3_Browser_treatment_w3, 
                            data = svy_sub, 
                            condition1 = control, 
                            condition2 = trt))

## ITT WITHOUT Lin's covariate adjustment (data sparsity issue)
form <- as.formula("prediction_certainty ~ W3_PATA306_treatment_w3")
# itt <- lm_robust(form, data = dplyr::filter(svy_sub, W3_PATA306_treatment_w3 != "FoxNews"))

itt <- lm_robust(form, svy_sub)

format_latex3(dim, list(itt), dv = "H4a \\& H4b: \\ Prediction certainty")



########################################################
### Bombing H1
########################################################

# Bombing H1a: HuffPost vs. Fox News treatment

trt <- "HuffPost"
dv <- "W4_PATA462_a"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)


format_latex3(dim, itt, dv = "H1a \\& H1b: Mail-bombing is a false flag")


########################################################
### Bombing H2
########################################################

# Bombing H2a: HuffPost vs. Fox News treatment

trt <- "HuffPost"
dv <- "W4_PATA462_b"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)


format_latex3(dim, itt, dv = "H2a \\& H2b: Media is accountable")


########################################################
### Bombing H3a
########################################################

# Bombing H3a: HuffPost vs. Fox News treatment

trt <- "HuffPost"
dv <- "W4_PATA462_c"
control <- "FoxNews"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = control, 
                            condition2 = trt))

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control = control)


format_latex3(dim, itt, dv = "H3a \\& H3b: Trump is accountable")

