#===============================================================================
#  File:    07-other-appendix-graphs.R
#  Date:    Feb 3, 2021
#  Purpose: replicate appendix analyses: graphs with HTE
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
### Issue RQ1: heterogeneous treatment effects
########################################################

dv <- 'issue_scale'
dv_pre <- 'issue_scale_pre'
moderator <- 'ideo'
trt <- 'HuffPost'

# H2
heterogeneous_effect(dv = dv, dv_pre = dv_pre,
                     moderator = moderator, trt = trt)

# compute standard itt
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, verbose=FALSE)
vars <- unique(c(extract_covariates(itt), moderator))
# now compute itt but adding moderator (if it wasn't included)
lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
res <- lm_lin(lin_formula, covariates = lin_covars,
              data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])

# computing marginal effects
main <- "W3_PATA306_treatment_w3HuffPost"
inter <- "W3_PATA306_treatment_w3HuffPost:ideo_c"
xvars <- ((1:5)-res$scaled_center['ideo'])
ideo_cats <- c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative")

marg.effects <- coef(res)[main] + 
  coef(res)[inter] *
  xvars
sd <- sqrt(
  vcov(res)[main, main] +
    vcov(res)[inter, inter] * xvars^2 +
    2 * xvars * vcov(res)[main, inter]
)
df <- data.frame(
  ideo = xvars,
  ideo_ct = factor(ideo_cats, levels=ideo_cats),
  mean = marg.effects,
  sd = sd
)

p <- ggplot(df, aes(x=ideo_ct, y=mean))
pq <- p + theme_minimal() +
  geom_pointrange(aes(ymin = mean - 1.96 * sd, 
                      ymax = mean + 1.96 * sd)) +
  scale_x_discrete("Self-reported ideology") +
  scale_y_continuous("Estimated marginal effect (95% CI)") +
  geom_hline(yintercept = 0, size=1, color="blue", linetype=6)
pq

ggsave(pq, 
       file="graphs/appendix_fig14.pdf",
       height=5, width=7)

########################################################
### Media Trust RQ1
########################################################

#### PARTY ID

dv <- 'bias'
moderator <- 'party7'
trt <- 'FoxNews'

# H2
heterogeneous_effect(dv = dv, 
                     moderator = moderator, trt = trt)

# compute standard itt
itt <- run_model(dv = dv, trt = trt, verbose=FALSE)
vars <- unique(c(extract_covariates(itt), moderator))
# now compute itt but adding moderator (if it wasn't included)
lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
res <- lm_lin(lin_formula, covariates = lin_covars,
              data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])
res <- lm_robust(bias ~ party7 * W3_PATA306_treatment_w3,
                 data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])

# computing marginal effects
main <- "W3_PATA306_treatment_w3FoxNews"
inter <- "W3_PATA306_treatment_w3FoxNews:party7_c"
inter <- "party7:W3_PATA306_treatment_w3FoxNews"
xvars <- ((1:7)-res$scaled_center['party7'])
xvars <- 1:7
ideo_cats <- c("Strong\nDemocrat", "Not very\nstrong\nDemocrat",
               "Lean\nDemocrat", "Independent",
               "Lean\nRepublican", "Not very\nstrong\nRepublican",
               "Strong\nRepublican")

marg.effects <- coef(res)[main] + 
  coef(res)[inter] *
  xvars
sd <- sqrt(
  vcov(res)[main, main] +
    vcov(res)[inter, inter] * xvars^2 +
    2 * xvars * vcov(res)[main, inter]
)
df <- data.frame(
  facet = "interaction with party ID",
  ideo = xvars,
  ideo_ct = factor(ideo_cats, levels=ideo_cats),
  mean = marg.effects,
  sd = sd
)

library(margins)
res <- lm(bias ~ party7 * W3_PATA306_treatment_w3,
          data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])
margins(res, variables = "party7")
cplot(res, "party7", what = "effect")

#### IDEOLOGY

dv <- 'bias'
moderator <- 'ideo'
trt <- 'FoxNews'

# H2
heterogeneous_effect(dv = dv, 
                     moderator = moderator, trt = trt)

# compute standard itt
itt <- run_model(dv = dv, trt = trt, verbose=FALSE)
vars <- unique(c(extract_covariates(itt), moderator))
# now compute itt but adding moderator (if it wasn't included)
lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
res <- lm_lin(lin_formula, covariates = lin_covars,
              data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])
res <- lm_robust(bias ~ ideo * W3_PATA306_treatment_w3,
                 data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])

# computing marginal effects
main <- "W3_PATA306_treatment_w3FoxNews"
inter <- "W3_PATA306_treatment_w3FoxNews:ideo_c"
inter <- "ideo:W3_PATA306_treatment_w3FoxNews"
xvars <- ((1:5)-res$scaled_center['ideo'])
xvars <- (1:5)
ideo_cats <- c("Very\nliberal", "Liberal", "Moderate", "Conservative", "Very\nconservative")

marg.effects <- coef(res)[main] + 
  coef(res)[inter] *
  xvars
sd <- sqrt(
  vcov(res)[main, main] +
    vcov(res)[inter, inter] * xvars^2 +
    2 * xvars * vcov(res)[main, inter]
)
df <- rbind(df, data.frame(
  facet = "Interaction with ideology",
  ideo = xvars,
  ideo_ct = factor(ideo_cats, levels=ideo_cats),
  mean = marg.effects,
  sd = sd
))

p <- ggplot(df, aes(x=ideo_ct, y=mean))
pq <- p + theme_minimal() +
  geom_pointrange(aes(ymin = mean - 1.96 * sd, 
                      ymax = mean + 1.96 * sd)) +
  facet_wrap(~facet, scales="free_x") +
  scale_x_discrete("Self-reported party ID or ideology") +
  scale_y_continuous("Estimated marginal effect (95% CI)") +
  geom_hline(yintercept = 0, size=1, color="blue", linetype=6)
pq

ggsave(pq, 
       file="graphs/appendix_fig15.pdf",
       height=5, width=12)


########################################################
### Media Trust RQ1 (W5)
########################################################

#### PARTY ID

dv <- 'trust_hp_w5'
moderator <- 'party7'
trt <- 'FoxNews'

# H2
heterogeneous_effect(dv = dv, 
                     moderator = moderator, trt = trt)

# compute standard itt
itt <- run_model(dv = dv, trt = trt, verbose=FALSE)
vars <- unique(c(extract_covariates(itt), moderator))
# now compute itt but adding moderator (if it wasn't included)
lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
res <- lm_lin(lin_formula, covariates = lin_covars,
              data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])

# computing marginal effects
main <- "W3_PATA306_treatment_w3FoxNews"
inter <- "W3_PATA306_treatment_w3FoxNews:party7_c"
xvars <- ((1:7)-res$scaled_center['party7'])
ideo_cats <- c("Strong\nDemocrat", "Not very\nstrong\nDemocrat",
               "Lean\nDemocrat", "Independent",
               "Lean\nRepublican", "Not very\nstrong\nRepublican",
               "Strong\nRepublican")

marg.effects <- coef(res)[main] + 
  coef(res)[inter] *
  xvars
sd <- sqrt(
  vcov(res)[main, main] +
    vcov(res)[inter, inter] * xvars^2 +
    2 * xvars * vcov(res)[main, inter]
)
df <- data.frame(
  facet = "interaction with party ID",
  ideo = xvars,
  ideo_ct = factor(ideo_cats, levels=ideo_cats),
  mean = marg.effects,
  sd = sd
)

p <- ggplot(df, aes(x=ideo_ct, y=mean))
pq <- p + theme_minimal() +
  geom_pointrange(aes(ymin = mean - 1.96 * sd, 
                      ymax = mean + 1.96 * sd)) +
  #facet_wrap(~facet, scales="free_x") +
  scale_x_discrete("Self-reported party ID") +
  scale_y_continuous("Estimated marginal effect (95% CI)") +
  geom_hline(yintercept = 0, size=1, color="blue", linetype=6)
pq

ggsave(pq, 
       file="graphs/appendix_fig16.pdf",
       height=5, width=12)



########################################################
### MEDIA TRUST -- SINGLE GRAPH WITH ALL RESULTS ACROSS WAVES
########################################################

# wave 4
wave <- "Wave 4"
est <- c(-0.123, -0.081, -0.538, -0.378)
se <- c(0.052, 0.054, 0.232, 0.223)
trt <- c("Fox News", "HuffPost", "Fox News", "HuffPost")
method <- c("ITT", "ITT", "CACE", "CACE")

df1 <- data.frame(
  est, se, trt, method, wave
)

# wave 7
wave <- "Wave 7"
est <- c(-0.129, -0.154, -0.557, -0.701)
se <- c(0.058, 0.060, 0.262, 0.276)
trt <- c("Fox News", "HuffPost", "Fox News", "HuffPost")
method <- c("ITT", "ITT", "CACE", "CACE")

df2 <- data.frame(
  est, se, trt, method, wave
)


# wave 8
wave <- "Wave 8"
est <- c(-0.091, -0.105, -0.512, -0.565)
se <- c(0.057, 0.060, 0.267, 0.267)
trt <- c("Fox News", "HuffPost", "Fox News", "HuffPost")
method <- c("ITT", "ITT", "CACE", "CACE")

df3 <- data.frame(
  est, se, trt, method, wave
)

df <- rbind(df1, df2, df3)


p <- ggplot(df, aes(x=trt, y=est, group=method, color=method))
pq <- p + geom_point(position = position_dodge(width = 0.5), size=5) +
  geom_linerange(aes(ymin=est-1*se, ymax=est+1*se), size=3,
                 position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin=est-1.96*se, ymax=est+1.96*se), size=1,
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values=c("black", "gray80")) +
  facet_grid(~wave) +
  theme_minimal() +
  geom_hline(yintercept=0, size=1) +
  theme(axis.title.y = element_blank(),
        axis.title = element_blank(),
        legend.position="none",
        panel.grid.major = element_blank()) +
  ggtitle("Media trust")
pq

ggsave(pq, file="graphs/appendix_fig17.pdf", height=4, width=6)