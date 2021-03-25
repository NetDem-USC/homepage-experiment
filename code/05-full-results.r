#===============================================================================
#  File:    05-full-results.R
#  Date:    Feb 3, 2021
#  Purpose: replicate appendix analyses: Section 4 Full results
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
# install.packages("BiocManager")
# BiocManager::install("multtest")
library(multtest)
library(plot.matrix)
source('code/functions.r')

# DATA
#===============================================================================
pulse <- read_csv("data/daily_pulse_data.csv")
svy <- read_csv("data/survey_data.csv")

# Dropping observations where treatment is missing
svy <- svy[!is.na(svy$W3_PATA306_treatment_w3),]


# Preparation for multiple hypothesis testing corrections ----------

# set up containers to collect p values
dv_fn_vec <- character()
dv_hp_vec <- character()
dim_fn_pval_vec <- character()
dim_hp_pval_vec <- character()
itt_fn_pval_vec <- character()
itt_hp_pval_vec <- character()
cace_fn_pval_vec <- character()
cace_hp_pval_vec <- character()
itt_fn_cohd_vec <- character()
itt_hp_cohd_vec <- character()
hypothesis_fn_vec <- character()
hypothesis_hp_vec <- character()
hypothesis_all_vec <- character()
prereg_fn_vec <- character()
prereg_hp_vec <- character()
dv_any_vec <- character()
dim_any_pval_vec <- character()
itt_any_pval_vec <- character()
cace_any_pval_vec <- character()
itt_any_cohd_vec <- character()
#cace_any_cohd_vec <- character()
hypothesis_any_vec <- character()
prereg_any_vec <- character()


# function to collect pvals
update_pvals <- function(dv, trt, dim, itt, cace, prereg = "", hypothesis = "") {
  if(trt == "FoxNews"){
    dv_fn_vec[length(dv_fn_vec) +1] <<- dv
    dim_fn_pval_vec[length(dim_fn_pval_vec) +1] <<- dim[["p.value"]]
    itt_fn_pval_vec[length(itt_fn_pval_vec) +1] <<- itt[["p.value"]][2]
    cace_fn_pval_vec[length(cace_fn_pval_vec) +1] <<- cace[["p.value"]][2]
    itt_fn_cohd_vec[length(itt_fn_cohd_vec) +1] <<- pwr$pwr_itt[2]
    prereg_fn_vec[length(prereg_fn_vec) +1] <<- prereg
    hypothesis_fn_vec[length(hypothesis_fn_vec) +1] <<- hypothesis
  }
  if(trt == "HuffPost"){
    dv_hp_vec[length(dv_hp_vec) +1] <<- dv
    dim_hp_pval_vec[length(dim_hp_pval_vec) +1] <<- dim[["p.value"]]
    itt_hp_pval_vec[length(itt_hp_pval_vec) +1] <<- itt[["p.value"]][2]
    cace_hp_pval_vec[length(cace_hp_pval_vec) +1] <<- cace[["p.value"]][2]
    itt_hp_cohd_vec[length(itt_hp_cohd_vec) +1] <<- pwr$pwr_itt[2]
    prereg_hp_vec[length(prereg_hp_vec) +1] <<- prereg
    hypothesis_hp_vec[length(hypothesis_hp_vec) +1] <<- hypothesis
  }
  if(!(trt %in% c("FoxNews", "HuffPost"))){
    dv_any_vec[length(dv_any_vec) +1] <<- dv
    dim_any_pval_vec[length(dim_any_pval_vec) +1] <<- dim[["p.value"]]
    itt_any_pval_vec[length(itt_any_pval_vec) +1] <<- itt[["p.value"]][2]
    cace_any_pval_vec[length(cace_any_pval_vec) +1] <<- cace[["p.value"]][2]
    itt_any_cohd_vec[length(itt_any_cohd_vec) +1] <<- pwr$pwr_itt[2]
    prereg_any_vec[length(prereg_any_vec) +1] <<- prereg
    hypothesis_any_vec[length(hypothesis_any_vec) +1] <<- hypothesis
  }
  hypothesis_all_vec[length(hypothesis_all_vec) +1] <<- hypothesis
}

# Analysis ----------------------------------------------------------------

# https://declaredesign.org/blog/biased-fixed-effects.html

vars <- c("party7", "age", "agesq", "female", "raceeth", "educ",
          "ideo", "income", "employ", "state", "polint", "freq_tv", "freq_np", 
          "freq_rad", "freq_net", "freq_disc", "log_news_pre", "diet_mean_pre")

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
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Conservatism")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Issue Opinions H1")


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
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: Conservatism")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Issue Opinions H2")


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
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H3: Pro-immigration")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Issue Opinions H3")


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
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4: Pro-immigration")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Issue Opinions H4")


########################################################
### Issue RQ1: heterogeneous treatment effects
########################################################

# H1
heterogeneous_effect(dv = "issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "HuffPost")
# H3
heterogeneous_effect(dv = "imm_issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "imm_issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "FoxNews")
# H4
heterogeneous_effect(dv = "imm_issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "imm_issue_scale", dv_pre = "issue_scale_pre",
                     moderator = "ideo", trt = "HuffPost")


########################################################
### News H1: news browsing, FoxNews treatment, 1 week
########################################################

trt <- "FoxNews"
dv <- "log_cons_1w"
dv_pre <- "log_cons_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Cons. news")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "News Browsing H1")


########################################################
### News H2: news browsing, HuffPost treatment, 1 week
########################################################

trt <- "HuffPost"
dv <- "log_lib_1w"
dv_pre <- "log_lib_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: Lib. news")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "News Browsing H2")


########################################################
### News H1: news browsing, FoxNews treatment, 4 weeks
########################################################

trt <- "FoxNews"
dv <- "log_cons_4w"
dv_pre <- "log_cons_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Cons. news")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "News Browsing H1-4w")


########################################################
### News H2: news browsing, HuffPost treatment, 4 week
########################################################

trt <- "HuffPost"
dv <- "log_lib_4w"
dv_pre <- "log_lib_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: Lib. news")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "News Browsing H2-4w")


########################################################
### News H1: news browsing, FoxNews treatment, 6 weeks
########################################################

trt <- "FoxNews"
dv <- "log_cons_6w"
dv_pre <- "log_cons_pre"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Cons. news")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "News Browsing H1-6w")


########################################################
### News H2: news browsing, HuffPost treatment, 6 week
########################################################

trt <- "HuffPost"
dv <- "log_lib_6w"
dv_pre <- "log_lib_pre"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, more_vars = "log_total_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: Lib. news")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "News Browsing H2-6w")


########################################################
### News RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1
heterogeneous_effect(dv = "log_cons_1w", dv_pre = "log_cons_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "log_cons_1w", dv_pre = "log_cons_pre",
                     moderator = "ideo", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "log_lib_1w", dv_pre = "log_lib_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "log_lib_1w", dv_pre = "log_lib_pre",
                     moderator = "ideo", trt = "HuffPost")

########################################################
### News RQ2: heterogeneous treatment effects by pre-treatment news consumption
########################################################

# H1
heterogeneous_effect(dv = "log_cons_1w", dv_pre = "log_hp_pre",
                     moderator = "log_fn_pre", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "log_lib_1w", dv_pre = "log_fn_pre",
                     moderator = "log_hp_pre", trt = "HuffPost")


########################################################
### Twitter H1: conservative link shares
########################################################

# creating Twitter variables
svy$log_cons_links <- log(svy$cons_links+1)
svy$log_cons_links_pre <- log(svy$cons_links_pre+1)
svy$log_total_links_pre <- log(svy$total_links_pre+1)

trt <- "FoxNews"
dv <- "log_cons_links"
dv_pre <- "log_cons_links_pre"
D <- "comp_fn"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, blocks="chrome", more_vars = "log_total_links_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Tweets w/cons. links")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Social Media H1")



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
D <- "comp_hp"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)
## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, blocks="chrome", more_vars="log_total_links_pre")
compute_proportion_missing_covars(itt)

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt) # error with LATE power

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: tweets w/lib. links")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Social Media H2")


########################################################
### Twitter H3: follow conservative news sources
########################################################

# creating Twitter variables
svy$log_follows_cons_media <- log(svy$follows_cons_media+1)
svy$log_follows_elites <- log(svy$follows_elites+1)

trt <- "FoxNews"
dv <- "log_follows_cons_media"
D <- "comp_fn"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)
## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, blocks="chrome", more_vars = "log_follows_elites")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H3: follow cons. media")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Social Media H3")


########################################################
### Twitter H4: follow liberal news sources
########################################################

# creating Twitter variables
svy$log_follows_lib_media <- log(svy$follows_lib_media+1)
svy$log_follows_elites <- log(svy$follows_elites+1)

trt <- "HuffPost"
dv <- "log_follows_lib_media"
D <- "comp_hp"

# using chrome vs others as blocks here
svy$chrome <- ifelse(svy$W3_Browser_treatment_w3=="Chrome", 1, 0)
## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = chrome, 
                            data = svy, 
                            condition1 = "Control", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, blocks="chrome", more_vars = "log_follows_elites")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H4: follow lib. media")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Social Media H4")


########################################################
### Affective H1a: Democrats, HuffPost treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA450a_num <- as.numeric(svy$W4_PATA450a)
svy$W2_PATA2_3_Dem_num <- as.numeric(svy$W2_PATA2_3_Dem)

trt <- "HuffPost"
dv <- "W4_PATA450a_num" # FT Democrats
dv_pre <- "W2_PATA2_3_Dem_num"
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
              dv = "H1a: Dem. thermometer")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Affective Polarization H1a")


########################################################
### Affective H1b: Republicans, HuffPost treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA450b_num <- as.numeric(svy$W4_PATA450b)
svy$W2_PATA2_3_Rep_num <- as.numeric(svy$W2_PATA2_3_Rep)

trt <- "HuffPost"
dv <- "W4_PATA450b_num" # FT Republicans
dv_pre <- "W2_PATA2_3_Rep_num"
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
              dv = "H1b: Rep. thermometer")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Affective Polarization H1b")


########################################################
### Affective H1c: Trump supporters, HuffPost treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA450e_num <- as.numeric(svy$W4_PATA450e)
svy$W2_PATA2_6_num <- as.numeric(svy$W2_PATA2_6)

trt <- "HuffPost"
dv <- "W4_PATA450e_num" # Trump social distance
dv_pre <- "W2_PATA2_6_num"
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
              dv = "H1c: Trump distance")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Affective Polarization H1c")


########################################################
### Affective H2a: Democrats, FoxNews treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA450a_num <- as.numeric(svy$W4_PATA450a)
svy$W2_PATA2_3_Dem_num <- as.numeric(svy$W2_PATA2_3_Dem)

trt <- "FoxNews"
dv <- "W4_PATA450a_num" # FT Democrats
dv_pre <- "W2_PATA2_3_Dem_num"
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
              dv = "H2a: Dem. thermometer")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Affective Polarization H2a")


########################################################
### Affective H2b: Republicans, FoxNews treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA450b_num <- as.numeric(svy$W4_PATA450b)
svy$W2_PATA2_3_Rep_num <- as.numeric(svy$W2_PATA2_3_Rep)

trt <- "FoxNews"
dv <- "W4_PATA450b_num" # FT Republicans
dv_pre <- "W2_PATA2_3_Rep_num"
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
              dv = "H2b: Rep. thermometer")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Affective Polarization H2b")


########################################################
### Affective H2c: Trump supporters, FoxNews treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA450e_num <- as.numeric(svy$W4_PATA450e)
svy$W2_PATA2_6_Rep_num <- as.numeric(svy$W2_PATA2_6)

trt <- "FoxNews"
dv <- "W4_PATA450e_num" # Trump social distance
dv_pre <- "W2_PATA2_6_Rep_num"
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
              dv = "H2c: Trump distance")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Affective Polarization H2c")


########################################################
### Polarization RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1a
heterogeneous_effect(dv = "W4_PATA450a", dv_pre = "W2_PATA2_3_Dem",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "W4_PATA450a", dv_pre = "W2_PATA2_3_Dem",
                     moderator = "ideo", trt = "HuffPost")
# H1b
heterogeneous_effect(dv = "W4_PATA450b", dv_pre = "W2_PATA2_3_Rep",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "W4_PATA450b", dv_pre = "W2_PATA2_3_Rep",
                     moderator = "ideo", trt = "HuffPost")

# H1c
heterogeneous_effect(dv = "W4_PATA450e", dv_pre = "W2_PATA2_6",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "W4_PATA450e", dv_pre = "W2_PATA2_6",
                     moderator = "ideo", trt = "HuffPost")

# H2a
heterogeneous_effect(dv = "W4_PATA450a", dv_pre = "W2_PATA2_3_Dem",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "W4_PATA450a", dv_pre = "W2_PATA2_3_Dem",
                     moderator = "ideo", trt = "FoxNews")
# H2b
heterogeneous_effect(dv = "W4_PATA450b", dv_pre = "W2_PATA2_3_Rep",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "W4_PATA450b", dv_pre = "W2_PATA2_3_Rep",
                     moderator = "ideo", trt = "FoxNews")

# H2c
heterogeneous_effect(dv = "W4_PATA450e", dv_pre = "W2_PATA2_6",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "W4_PATA450e", dv_pre = "W2_PATA2_6",
                     moderator = "ideo", trt = "FoxNews")


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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt) 

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1a: Perceived polarization")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Perceived Polarization H1a")


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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1b: Perceived polarization")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Perceived Polarization H1b")


########################################################
### Perceived RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1a
heterogeneous_effect(dv = "ppol", 
                     moderator = "party7", trt = "FoxNews")
# H1b
heterogeneous_effect(dv = "ppol", 
                     moderator = "party7", trt = "HuffPost")


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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: Agenda setting")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Agenda Setting H1")


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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace,  trt, pwr,
              dv = "H2: Agenda setting")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Agenda Setting H2")


########################################################
### Agenda RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1
heterogeneous_effect(dv = "agenda_lean", dv_pre = "agenda_lean_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "agenda_lean", dv_pre = "agenda_lean_pre",
                     moderator = "ideo", trt = "FoxNews")

# H2
heterogeneous_effect(dv = "agenda_lean", dv_pre = "agenda_lean_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "agenda_lean", dv_pre = "agenda_lean_pre",
                     moderator = "ideo", trt = "HuffPost")

########################################################
### Agenda RQ2: heterogeneous treatment effects by pre-treatment habits
########################################################

# H1
heterogeneous_effect(dv = "agenda_lean", dv_pre = "log_fn_pre",
                     moderator = "ideo", trt = "FoxNews")
# H2
heterogeneous_effect(dv = "agenda_lean", dv_pre = "log_hp_pre",
                     moderator = "ideo", trt = "HuffPost")


########################################################
### Approval H1a: President
########################################################

# creating numeric dvs to avoid labelled error
svy$trump_approve_num <- as.numeric(svy$trump_approve)
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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1a: Pres. approval")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Elite Approval H1a")


########################################################
### Approval H1b: Republicans in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
svy$cong_rep_approve_num <- as.numeric(svy$cong_rep_approve)
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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1b: Rep. Cong. pref.")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Elite Approval H1b")


########################################################
### Approval H1c: Democrats in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
svy$cong_dem_approve_num <- as.numeric(svy$cong_dem_approve)
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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1c: Dem. Cong. pref.")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Elite Approval H1c")


########################################################
### Approval H2a: President
########################################################

# creating numeric dvs to avoid labelled error
svy$trump_approve_num <- as.numeric(svy$trump_approve)
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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2a: Pres. approval")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Elite Approval H2a")


########################################################
### Approval H2b: Republicans in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
svy$cong_rep_approve_num <- as.numeric(svy$cong_rep_approve)
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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2b: Rep. Cong. pref.")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Elite Approval H2b")


########################################################
### Approval H2c: Democrats in Congress [Congress control preference]
########################################################

# creating numeric dvs to avoid labelled error
svy$cong_dem_approve_num <- as.numeric(svy$cong_dem_approve)
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

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2c: Dem. Cong. pref.")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Elite Approval H2c")


########################################################
### Approval RQ1: heterogeneous treatment effects by party ID / ideology
########################################################

# H1a
heterogeneous_effect(dv = "trump_approve", dv_pre = "trump_approve_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "trump_approve", dv_pre = "trump_approve_pre",
                     moderator = "ideo", trt = "FoxNews")
# H1b
heterogeneous_effect(dv = "cong_rep_approve", 
                     dv_pre = "cong_rep_approve_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "cong_rep_approve", 
                     dv_pre = "cong_rep_approve_pre",
                     moderator = "ideo", trt = "FoxNews")
# H1c
heterogeneous_effect(dv = "cong_dem_approve", 
                     dv_pre = "cong_dem_approve_pre",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "cong_dem_approve", 
                     dv_pre = "cong_dem_approve_pre",
                     moderator = "ideo", trt = "FoxNews")

# H2a
heterogeneous_effect(dv = "trump_approve", dv_pre = "trump_approve_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "trump_approve", dv_pre = "trump_approve_pre",
                     moderator = "ideo", trt = "HuffPost")
# H2b
heterogeneous_effect(dv = "cong_rep_approve", 
                     dv_pre = "cong_rep_approve_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "cong_rep_approve", 
                     dv_pre = "cong_rep_approve_pre",
                     moderator = "ideo", trt = "HuffPost")
# H2c
heterogeneous_effect(dv = "cong_dem_approve", 
                     dv_pre = "cong_dem_approve_pre",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "cong_dem_approve", 
                     dv_pre = "cong_dem_approve_pre",
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Turnout RQ1a")


########################################################
### Turnout RQ1b: Huff Post
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 1", hypothesis = "Turnout RQ1b")


########################################################
### Media Trust H1a: Trust in Fox News for FN treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_fox_num <- as.numeric(svy$trust_fox)

trt <- "FoxNews"
dv <- "trust_fox_num"
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H1a")


########################################################
### Media Trust H1b: Trust in HuffPost for FN treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_hp_num <- as.numeric(svy$trust_hp)

trt <- "FoxNews"
dv <- "trust_hp_num"
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H1b")


########################################################
### Media Trust H2a: Trust in HuffPost for HP treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_hp_num <- as.numeric(svy$trust_hp)

trt <- "HuffPost"
dv <- "trust_hp_num"
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H2a")


########################################################
### Media Trust H2b: Trust in Fox News for HP treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_fox_num <- as.numeric(svy$trust_fox)

trt <- "HuffPost"
dv <- "trust_fox_num"
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H2b")


########################################################
### Media Trust H3a: Media liberal bias for FN treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$bias_num <- as.numeric(svy$bias)

trt <- "FoxNews"
dv <- "bias_num"
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
              dv = "H3a: Liberal media bias")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H3a")


########################################################
### Media Trust H3b: Media liberal bias for HP treatment
########################################################

# creating numeric dvs to avoid labelled error
svy$bias_num <- as.numeric(svy$bias)

trt <- "HuffPost"
dv <- "bias_num"
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
              dv = "H3b: Liberal media bias")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H3b")


########################################################
### Media Trust RQ1
########################################################

# H1a
heterogeneous_effect(dv = "trust_fox", 
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "trust_fox", 
                     moderator = "ideo", trt = "FoxNews")
# H1b
heterogeneous_effect(dv = "trust_hp", 
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "trust_hp", 
                     moderator = "ideo", trt = "FoxNews")
# H2a
heterogeneous_effect(dv = "trust_hp", 
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "trust_hp", 
                     moderator = "ideo", trt = "HuffPost")
# H2b
heterogeneous_effect(dv = "trust_fox", 
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "trust_fox", 
                     moderator = "ideo", trt = "HuffPost")
# H3a
heterogeneous_effect(dv = "bias", 
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "bias", 
                     moderator = "ideo", trt = "FoxNews")
# H3b
heterogeneous_effect(dv = "bias", 
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "bias", 
                     moderator = "ideo", trt = "HuffPost")


########################################################
### Media Trust H4a: Media trust for FN group
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_num <- as.numeric(svy$trust)

trt <- "FoxNews"
dv <- "trust_num"
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
              dv = "H4a: Media trust")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H4a")


# substantive magnitude?
svy %>% group_by(ideo) %>% summarize(avg_trust = mean(trust, na.rm = TRUE))
.123/(3.2-1.94) # .098
.566/(3.2-1.94) # .45

svy %>% group_by(educ) %>% summarize(avg_trust = mean(trust, na.rm = TRUE))
.123/(2.63-2.33) # .41
.566/(2.63-2.33) # 1.89x

svy$diet_mean_cats <- cut(svy$diet_mean_pre, breaks = quantile(svy$diet_mean_pre, probs = seq(0, 1, by = 0.25)), include.lowest = TRUE, labels = 1:4)
svy$diet_mean_cats2 <- cut(svy$diet_mean_pre, breaks = c(min(svy$diet_mean_pre), -.5, 0, .5, max(svy$diet_mean_pre)), include.lowest = TRUE)
svy %>% group_by(diet_mean_cats2) %>% summarize(avg_trust = mean(trust, na.rm = TRUE),
                                                      avg_diet = mean(diet_mean_pre))
.123/(2.2-1.52) # .18
.566/(2.2-1.52) # .83

########################################################
### Media Trust H4b: Media trust for HP group
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_num <- as.numeric(svy$trust)

trt <- "HuffPost"
dv <- "trust_num"
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
              dv = "H4b: Media trust")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Media Trust H4b")


########################################################
### Media Trust H4a: Media trust for FN group (w7)
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_w7_num <- as.numeric(svy$trust_w7)

trt <- "FoxNews"
dv <- "trust_w7_num"
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
              dv = "H4a: Media trust")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "Media Trust H4a-7w")


########################################################
### Media Trust H4b: Media trust for HP group (w7)
########################################################

# creating numeric dvs to avoid labelled error
svy$trust_w7_num <- as.numeric(svy$trust_w7)

trt <- "HuffPost"
dv <- "trust_w7_num"
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
              dv = "H4b: Media trust")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "Media Trust H4b-7w")


########################################################
### Factual knowledge H1: % foreign born
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA430a_num <- as.numeric(svy$W4_PATA430a)

trt <- "FoxNews"
dv <- "W4_PATA430a_num"
D <- "comp_fn"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "HuffPost", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control="HuffPost")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt, control="HuffPost")

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt) 

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H1: \\% foreign born")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Factual Knowledge H1")


########################################################
### Factual knowledge H2: % unemployment rate
########################################################

# creating numeric dvs to avoid labelled error
svy$W4_PATA430b_num <- as.numeric(svy$W4_PATA430b)

trt <- "HuffPost"
dv <- "W4_PATA430b_num"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "HuffPost", 
                            condition2 = trt))
## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, control="FoxNews")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt, control="FoxNews")

# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = TRUE, D = D, trt = trt)

format_latex2(dim, itt, cace, trt, pwr,
              dv = "H2: \\% unemployment")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Factual Knowledge H2")


########################################################
### Factual knowledge RQ1a: news reception for HP group
########################################################

trt <- "HuffPost"
dv <- "event_mokken"
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Factual Knowledge RQ1a")


########################################################
### Factual knowledge RQ1b: news reception for FN group
########################################################

trt <- "FoxNews"
dv <- "event_mokken"
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

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Factual Knowledge RQ1b")


########################################################
### Knowledge RQ2
########################################################

# H1
heterogeneous_effect(dv = "W4_PATA430a", 
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "W4_PATA430a", 
                     moderator = "ideo", trt = "FoxNews")

# H2
heterogeneous_effect(dv = "W4_PATA430b", 
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "W4_PATA430b", 
                     moderator = "ideo", trt = "HuffPost")

# RQ1a
heterogeneous_effect(dv = "event_mokken", dv_pre = "event_pre_mokken",
                     moderator = "party7", trt = "HuffPost")
heterogeneous_effect(dv = "event_mokken", dv_pre = "event_pre_mokken",
                     moderator = "ideo", trt = "HuffPost")

# RQ1b
heterogeneous_effect(dv = "event_mokken", dv_pre = "event_pre_mokken",
                     moderator = "party7", trt = "FoxNews")
heterogeneous_effect(dv = "event_mokken", dv_pre = "event_pre_mokken",
                     moderator = "ideo", trt = "FoxNews")


########################################################
### Factual knowledge RQ1a: immigration knowledge for FN group
########################################################

svy$know_immig_w2_TRUE <- svy$W2_PATA2_10_m_3 == 1
svy$know_immig_w4_TRUE <- svy$W4_PATA409_1 == 1

trt <- "FoxNews"
dv <- "know_immig_w4_TRUE"
dv_pre <- "know_immig_w2_TRUE"
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
              dv = "RQ1b: Immigration event knowledge")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "Factual Knowledge RQ1a-immig")


########################################################
### Factual knowledge RQ1b: immigration knowledge for HP group
########################################################

svy$know_immig_w2_TRUE <- svy$W2_PATA2_10_m_3 == 1
svy$know_immig_w4_TRUE <- svy$W4_PATA409_1 == 1

trt <- "HuffPost"
dv <- "know_immig_w4_TRUE"
dv_pre <- "know_immig_w2_TRUE"
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
              dv = "RQ1b: Immigration event knowledge")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "No", hypothesis = "Factual Knowledge RQ1b-immig")



########################################################
### Election prediction H1
########################################################

# Subjects assigned to the Fox News treatment...

# H1a: ... will be more likely to predict the Republican Party as winning the control of the House of Representatives
trt <- "FoxNews"
dv <- "predicted_house_winner_gop"
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
              dv = "H1a: GOP House winner")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Election Prediction H1a")

# H1b: ... will predict a higher House vote share for the Republican Party 

# creating numeric dvs to avoid labelled error
svy$predicted_vote_share_gop_num <- as.numeric(svy$predicted_vote_share_gop)

trt <- "FoxNews"
dv <- "predicted_vote_share_gop_num"
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
              dv = "H1b: GOP House vote share")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Election Prediction H1b")


# H1c: ... will be more likely to predict the Republican candidate to win the race for the House seat in their Congressional district

trt <- "FoxNews"
dv <- "predicted_district_winner_gop"
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
              dv = "H1c: GOP House district winner")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Election Prediction H1c")


########################################################
### Election prediction H2
########################################################

# Subjects assigned to the HuffPost treatment...

# H2a: ... will be more likely to predict the Democratic Party as winning the control of the House of Representatives
trt <- "HuffPost"
dv <- "predicted_house_winner_dem"
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
              dv = "H2a: DEM House winner")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Election Prediction H2a")



# H2b: ... will predict a higher national two-party vote share for the Democratic Party

# creating numeric dvs to avoid labelled error
svy$predicted_vote_share_dem_num <- as.numeric(svy$predicted_vote_share_dem)

trt <- "HuffPost"
dv <- "predicted_vote_share_dem_num"
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
              dv = "H2b: DEM House vote share")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Election Prediction H2b")


# H2c: ... will be more likely to predict the Democratic candidate to win the race for the House seat in their Congressional district

trt <- "HuffPost"
dv <- "predicted_district_winner_dem"
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
              dv = "H2c: DEM House district winner")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Election Prediction H2c")



########################################################
### Election prediction H3
########################################################

# Pre-treatment Republican partisans in the Fox News treatment group and pre-treatment Democratic partisans in the HuffPost treatment group will report a higher average certainty in their predictions than subjects assigned to the control group.

# creating numeric dvs to avoid labelled error
svy$prediction_certainty_num <- as.numeric(svy$prediction_certainty)

# creating combined treatment variable
svy$treatment_any <- as.character(svy$W3_PATA306_treatment_w3)
svy$treatment_any[svy$treatment_any %in% c("FoxNews", "HuffPost")] <- "Treatment"

svy_sub <- filter(svy, (W3_PATA306_treatment_w3 == "FoxNews" & partylean == "Republican") | (W3_PATA306_treatment_w3 == "HuffPost" & partylean == "Democrat") | W3_PATA306_treatment_w3 == "Control")

trt <- "Treatment"
dv <- "prediction_certainty_num"
D <- "comp_fn"

## DIM WITHOUT BLOCKING (data sparsity issue)
(dim <- difference_in_means(formula(paste0(dv, " ~ treatment_any")), 
                            #blocks = W3_Browser_treatment_w3, 
                            data = svy_sub, 
                            condition1 = "Control", 
                            condition2 = trt))

## ITT WITHOUT Lin's covariate adjustment (data sparsity issue)
form <- as.formula("prediction_certainty ~ treatment_any")
itt <- lm_robust(form, data = svy_sub)


## CACE WITHOUT Lin's covariate adjustment (data sparsity issue)
form <- as.formula("prediction_certainty ~ comp_fn | treatment_any")
cace <- iv_robust(form, data = svy_sub)


# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = FALSE, D, trt, trt_var = "treatment_any")
pwr <- data.frame(pwr_itt = c(0.351, 0.153))

format_latex2(dim, list(itt), cace, trt, pwr,
              dv = "H3: \\ Prediction certainty")

# collect p values
update_pvals(dv, trt, dim, itt, cace, prereg = "Study 2", hypothesis = "Prediction uncertainty H3")




########################################################
### Election prediction H4
########################################################

# Pre-treatment Republican partisans in the HuffPost treatment group and pre-treatment Democratic partisans in the Fox News treatment group will report a lower average certainty in their predictions than subjects assigned to the control group.

svy_sub <- filter(svy, (W3_PATA306_treatment_w3 == "FoxNews" & partylean == "Democrat") | (W3_PATA306_treatment_w3 == "HuffPost" & partylean == "Republican") | W3_PATA306_treatment_w3 == "Control")

trt <- "Treatment"
dv <- "prediction_certainty_num"
D <- "comp_fn"

## DIM WITHOUT BLOCKING (data sparsity issue)
(dim <- difference_in_means(formula(paste0(dv, " ~ treatment_any")), 
                            #blocks = W3_Browser_treatment_w3, 
                            data = svy_sub, 
                            condition1 = "Control", 
                            condition2 = trt))

## ITT WITHOUT Lin's covariate adjustment (data sparsity issue)
form <- as.formula("prediction_certainty ~ treatment_any")
itt <- lm_robust(form, data = svy_sub)


## CACE WITHOUT Lin's covariate adjustment (data sparsity issue)
form <- as.formula("prediction_certainty ~ comp_fn | treatment_any")
cace <- iv_robust(form, data = svy_sub)


# power analysis
pwr <- power2(dv, dim, itt, cace, covariates = FALSE, D, trt, trt_var = "treatment_any")
pwr <- data.frame(pwr_itt = c(0.363, 0.158))

format_latex2(dim, list(itt), cace, trt, pwr,
              dv = "H4: \\ Prediction certainty")

# collect p values
update_pvals(dv, trt, dim, itt, cace, prereg = "Study 2", hypothesis = "Prediction uncertainty H4")




########################################################
### Bombing H1
########################################################

# Bombing H1a: Fox News treatment vs. Control/HuffPost

# creating numeric dvs to avoid labelled error
svy$W4_PATA462_a_num <- as.numeric(svy$W4_PATA462_a)

trt <- "FoxNews"
dv <- "W4_PATA462_a_num"
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
              dv = "H1a: Mail-bombing is a false flag")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Mail-Bombing H1a")


# Bombing H1b: Fox News treatment vs. HuffPost treatment

trt <- "FoxNews"
dv <- "W4_PATA462_a_num"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "HuffPost", 
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
              dv = "H1b: Mail-bombing is a false flag")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Mail-Bombing H1b")


########################################################
### Bombing H2
########################################################

# Bombing H2a: Fox News treatment vs. Control

# creating numeric dvs to avoid labelled error
svy$W4_PATA462_b_num <- as.numeric(svy$W4_PATA462_b)

trt <- "FoxNews"
dv <- "W4_PATA462_b_num"
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
              dv = "H2a: Media is accountable")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Mail-Bombing H2a")


# Bombing H2b: Fox News treatment vs. HuffPost treatment

trt <- "FoxNews"
dv <- "W4_PATA462_b_num"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "HuffPost", 
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
              dv = "H2b: Media is accountable")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Mail-Bombing H2b")


########################################################
### Bombing H3a
########################################################

# Bombing H3a: Fox News treatment vs. Control

# creating numeric dvs to avoid labelled error
svy$W4_PATA462_c_num <- as.numeric(svy$W4_PATA462_c)

trt <- "FoxNews"
dv <- "W4_PATA462_c_num"
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
              dv = "H3a: Trump is accountable")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Mail-Bombing H3a")


# Bombing H3b: Fox News treatment vs. HuffPost treatment

trt <- "FoxNews"
dv <- "W4_PATA462_c_num"
D <- "comp_hp"

## DIM
(dim <- difference_in_means(formula(paste0(dv, " ~ W3_PATA306_treatment_w3")), 
                            blocks = W3_Browser_treatment_w3, 
                            data = svy, 
                            condition1 = "HuffPost", 
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
              dv = "H3b: Trump is accountable")

# collect p values
update_pvals(dv, trt, dim, itt[[1]], cace, prereg = "Study 2", hypothesis = "Mail-Bombing H3b")



# Multiple testing / p values analysis ---------------------------------------------------------------

# collect all p values into data frame
pval_df <- data.frame(
  dv = c(dv_fn_vec, dv_hp_vec, dv_any_vec),
  trt = c(rep("FoxNews", length(dv_fn_vec)), rep("HuffPost", length(dv_hp_vec)), rep("Any", length(dv_any_vec))),
  dim_pval = as.numeric(c(dim_fn_pval_vec, dim_hp_pval_vec, dim_any_pval_vec)),
  itt_pval = as.numeric(c(itt_fn_pval_vec, itt_hp_pval_vec, itt_any_pval_vec)),
  cace_pval = as.numeric(c(cace_fn_pval_vec, cace_hp_pval_vec, cace_any_pval_vec)),
  itt_cohd = as.numeric(c(itt_fn_cohd_vec, itt_hp_cohd_vec, itt_any_cohd_vec)),
  prereg = c(prereg_fn_vec, prereg_hp_vec, prereg_any_vec),
  hypothesis = c(hypothesis_fn_vec, hypothesis_hp_vec, hypothesis_any_vec),
  stringsAsFactors = FALSE)
pval_df$hypothesis_group <- str_replace(pval_df$hypothesis, "H[:digit:].*", "") %>% str_trim()

# store data
#  save(pval_df, file = "surveydata/pval_df_updated.RData")
# save(pval_df, file = "surveydata/pval_df.RData")

# remove RQs and non-pre-registered hypotheses 
pval_df <- filter(pval_df, !str_detect(hypothesis, "RQ"), prereg != "No") 

# p-values adjustment for multiple comparisons, BH method

# all p-values
pval_df$itt_pval_bh <- p.adjust(pval_df$itt_pval, method = "BH")
pval_df$cace_pval_bh <- p.adjust(pval_df$cace_pval, method = "BH")

# pre-registered hypotheses
pval_df$itt_pval_bh_prereg <- p.adjust(pval_df$itt_pval, method = "BH")
pval_df$cace_pval_bh_prereg <- p.adjust(pval_df$cace_pval, method = "BH")

# pre-registered hypotheses, by study
pval_df$itt_pval_bh_study1[pval_df$prereg == "Study 1"] <- p.adjust(pval_df$itt_pval[pval_df$prereg == "Study 1"], method = "BH")
pval_df$cace_pval_bh_study1[pval_df$prereg == "Study 1"] <- p.adjust(pval_df$cace_pval[pval_df$prereg == "Study 1"], method = "BH")
pval_df$itt_pval_bh_study2[pval_df$prereg == "Study 2"] <- p.adjust(pval_df$itt_pval[pval_df$prereg == "Study 2"], method = "BH")
pval_df$cace_pval_bh_study2[pval_df$prereg == "Study 2"] <- p.adjust(pval_df$cace_pval[pval_df$prereg == "Study 2"], method = "BH")


# p-values adjustment for multiple comparisons, two-stage step-up (Benjamini, Krieger and Yekutieli 2006)
# BiocManager::install("multtest")

# all p-values
pval_df$itt_pval_tsbh <- multtest::mt.rawp2adjp(as.numeric(pval_df$itt_pval), proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$itt_pval)))]
pval_df$cace_pval_tsbh <- multtest::mt.rawp2adjp(as.numeric(pval_df$cace_pval), proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$cace_pval)))]

# pre-registered hypotheses
pval_df$itt_pval_tsbh_prereg <- multtest::mt.rawp2adjp(as.numeric(pval_df$itt_pval), proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$itt_pval)))]
pval_df$cace_pval_tsbh_prereg <- multtest::mt.rawp2adjp(as.numeric(pval_df$cace_pval), proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$cace_pval)))]

# pre-registered hypotheses, by study
pval_df$itt_pval_tsbh_study1[pval_df$prereg == "Study 1"] <- multtest::mt.rawp2adjp(as.numeric(pval_df$itt_pval)[pval_df$prereg == "Study 1"], proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$itt_pval)[pval_df$prereg == "Study 1"]))]
pval_df$cace_pval_tsbh_study1[pval_df$prereg == "Study 1"] <- multtest::mt.rawp2adjp(as.numeric(pval_df$cace_pval)[pval_df$prereg == "Study 1"], proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$cace_pval)[pval_df$prereg == "Study 1"]))]
pval_df$itt_pval_tsbh_study2[pval_df$prereg == "Study 2"] <- multtest::mt.rawp2adjp(as.numeric(pval_df$itt_pval)[pval_df$prereg == "Study 2"], proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$itt_pval)[pval_df$prereg == "Study 2"]))]
pval_df$cace_pval_tsbh_study2[pval_df$prereg == "Study 2"] <- multtest::mt.rawp2adjp(as.numeric(pval_df$cace_pval)[pval_df$prereg == "Study 2"], proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$cace_pval)[pval_df$prereg == "Study 2"]))]

# pre-registered hypotheses, by hypothesis group
hypothesis_groups <- unique(pval_df$hypothesis_group)
pval_tsbh_hypothesis_groups_itt <- list()
pval_tsbh_hypothesis_groups_cace <- list()
for (i in seq_along(hypothesis_groups)) {
  pval_tsbh_hypothesis_groups_itt[[i]] <- multtest::mt.rawp2adjp(as.numeric(pval_df$itt_pval)[pval_df$hypothesis_group == hypothesis_groups[i]], proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$itt_pval)[pval_df$hypothesis_group == hypothesis_groups[i]]))]
  pval_tsbh_hypothesis_groups_cace[[i]] <- multtest::mt.rawp2adjp(as.numeric(pval_df$cace_pval)[pval_df$hypothesis_group == hypothesis_groups[i]], proc = "TSBH", alpha = 0.05, na.rm = TRUE)$adjp[,2][order(order(as.numeric(pval_df$cace_pval)[pval_df$hypothesis_group == hypothesis_groups[i]]))]
}
pval_df$itt_pval_tsbh_hypothesis_groups <- unlist(pval_tsbh_hypothesis_groups_itt)
pval_df$cace_pval_tsbh_hypothesis_groups <- unlist(pval_tsbh_hypothesis_groups_cace)


# put in right order
pval_df$hypothesis <- factor(pval_df$hypothesis, levels = hypothesis_all_vec)
pval_df <- arrange(pval_df, hypothesis)

# plot itt results
pval_res_df <- dplyr::select(filter(pval_df, prereg != "No"), itt_pval, itt_pval_tsbh_prereg, itt_pval_tsbh_hypothesis_groups, itt_pval_tsbh_study1, itt_pval_tsbh_study2)
pval_res_mat <- as.matrix(pval_res_df)
rownames(pval_res_mat) <- pval_df$hypothesis
#pval_res_mat <- pval_res_mat[rev(seq_len(nrow(pval_res_mat))),]
colnames_mat <- c("Unadjusted\n", "Two-stage BH\n", "\nTwo-stage BH\n(Domain)", "Two-stage BH\n(Study 1)", "\nTwo-stage BH\n(Study 2)")
get_palette <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))

pdf(file="graphs/appendix_fig18.pdf", height=14, width=12, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(1,13,3,4))
par(yaxs="i", xaxs="i", bty="n")
plot(pval_res_mat, axis.row = NULL, axis.col = NULL, main = "", ylab = "", xlab = "", breaks = 20, col=get_palette(20), digits = 2, fmt.cell='%.2f', fmt.key='%.2f', na.print = FALSE)
## Draw the y-axis.
axis(side = 2,
     ## Rotate the labels.
     las = 2,
     at = rev(seq_along(rownames(pval_res_mat))),
     label = rownames(pval_res_mat))
axis(side = 3, 
     at = seq_along(colnames_mat),
     label = colnames_mat,
     line = -.5,
     tick = FALSE)
dev.off()

# plot cace results
pval_res_df <- dplyr::select(filter(pval_df, prereg != "No"), cace_pval, cace_pval_tsbh_prereg, cace_pval_tsbh_hypothesis_groups, cace_pval_tsbh_study1, cace_pval_tsbh_study2)
pval_res_mat <- as.matrix(pval_res_df)
rownames(pval_res_mat) <- pval_df$hypothesis
#pval_res_mat <- pval_res_mat[rev(seq_len(nrow(pval_res_mat))),]
colnames_mat <- c("Unadjusted\n", "Two-stage BH\n", "\nTwo-stage BH\n(Domain)", "Two-stage BH\n(Study 1)", "\nTwo-stage BH\n(Study 2)")
get_palette <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))

pdf(file="graphs/appendix_fig19.pdf", height=14, width=12, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(1,13,3,4))
par(yaxs="i", xaxs="i", bty="n")
plot(pval_res_mat, axis.row = NULL, axis.col = NULL, main = "", ylab = "", xlab = "", breaks = 20, col=get_palette(20), digits = 2, fmt.cell='%.2f', fmt.key='%.2f', na.print = FALSE)
## Draw the y-axis.
axis(side = 2,
     ## Rotate the labels.
     las = 2,
     at = rev(seq_along(rownames(pval_res_mat))),
     label = rownames(pval_res_mat))
axis(side = 3, 
     at = seq_along(colnames_mat),
     label = colnames_mat,
     line = -.5,
     tick = FALSE)
dev.off()

# plot minimum detectable effects
cohd_itt_res_df <- dplyr::select(filter(pval_df, prereg != "No"), itt_cohd)
cohd_itt_res_mat <- as.matrix(cohd_itt_res_df)
rownames(cohd_itt_res_mat) <- pval_df$hypothesis
colnames_mat <- c("MDE (ITT)")
get_palette <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))

pdf(file="graphs/appendix_fig20.pdf", height=14, width=6, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(1,13,3,4))
par(yaxs="i", xaxs="i", bty="n")
plot(cohd_itt_res_mat, axis.row = NULL, axis.col = NULL, main = "", ylab = "", xlab = "",  breaks = 20, col=get_palette(20), digits = 2, fmt.cell='%.2f', fmt.key='%.2f', na.print = FALSE)
## Draw the y-axis.
axis(side = 2,
     ## Rotate the labels.
     las = 2,
     at = rev(seq_along(rownames(cohd_itt_res_mat))),
     label = rownames(cohd_itt_res_mat))
axis(side = 3, 
     at = seq_along(colnames_mat),
     label = colnames_mat,
     line = -.5,
     tick = FALSE)
dev.off()

## Summary table ##

results_table <- pval_df %>% select(9, 1:2, 8, 4:5, 13, 15)
results_table <- results_table %>% mutate(support = case_when(itt_pval <= 0.05 ~ "+", 
                                                              TRUE ~ ""))
results_table %>% select(1:4, 9) %>% 
  knitr::kable("latex", digits = 3, caption = "Results Summary: Hypotheses Tested")
