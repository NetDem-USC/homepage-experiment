#===============================================================================
#  File:    01-first-stage.R
#  Date:    Feb 3, 2021
#  Purpose: replicate Fig 2 and paper results re: impact of encouragement on
#            visits to news sites (compliance)
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
sessionInfo()

# DATA
#===============================================================================
pulse <- read_csv("data/daily_pulse_data.csv")
svy <- read_csv("data/survey_data.csv")

# SIZE OF TREATMENT GROUPS
#===============================================================================
table(svy$W3_PATA306_treatment_w3)
nrow(svy)

# SAMPLE SIZE
#===============================================================================
# respondents who received the encouragement and consented to wave 4
sum(!is.na(svy$W3_PATA306_treatment_w3) & !is.na(svy$W4_consent))


# DATA WRANGLING
#===============================================================================
# Dropping observations where treatment is missing
svy <- svy[!is.na(svy$W3_PATA306_treatment_w3),]

# Merge pulse data with survey data
pulse <- merge(pulse, 
                   dplyr::select(svy, id, W3_PATA306_treatment_w3, starttime_w3, W3_Browser_treatment_w3),
                   by.x = "id", by.y = "id", all.x = TRUE) %>%
  mutate(treatment = factor(W3_PATA306_treatment_w3, 
                            labels = c("Control group", "Fox News group", "HuffPost group")),
         starttime_w3 = as.Date(starttime_w3),
         timing = as.numeric(date - as.Date("2018-10-05"))) %>%
  as_tibble()

# FIGURE 2
#===============================================================================

pd <- pulse %>% 
  filter(!is.na(treatment)) %>% # dropping those who are not part of the experiment: NA
  filter(date!="2018-11-19") %>% # drop the data from 2018-11-19 because of missing data
  filter(timing >= (-30) & timing <= 50) %>%
  mutate(week = ifelse(timing>=0, ceiling((timing+1)/7),
                       floor(timing/7))) %>% 
  group_by(week, treatment) %>% 
  summarize(mean_fn = mean(log_fn_day),
            se_fn = sd(log_fn_day)/sqrt(n()),
            mean_hp = mean(log_hp_day),
            se_hp = sd(log_hp_day)/sqrt(n())
  ) %>% 
  ungroup()
plot_data <- merge(
  melt(pd, id.vars = c("week", "treatment"),
       mesure.vars = c("mean_fn", "mean_hp"), value.name = "mean",
       factorsAsStrings = FALSE) %>% 
    filter(variable %in% c("mean_fn", "mean_hp")) %>% 
    mutate(variable = ifelse(variable == "mean_fn", "Visits to FoxNews.com",
                             "Visits to HuffingtonPost.com"))
  ,
  melt(pd, id.vars = c("week", "treatment"),
       mesure.vars = c("se_fn", "se_hp"), value.name = "se",
       factorsAsStrings = FALSE) %>% 
    filter(variable %in% c("se_fn", "se_hp")) %>% 
    mutate(variable = ifelse(variable == "se_fn", "Visits to FoxNews.com",
                             "Visits to HuffingtonPost.com"))
)

p_fn <- plot_data %>%
  filter(variable == "Visits to FoxNews.com") %>% 
  ggplot(aes(week, mean, color = treatment, group=treatment)) +
  scale_x_continuous("Weeks before and after treatment was administered",
                     limits=c(-4.2, +7.2), 
                     breaks=c(seq(-4, -1, by=1), seq(1, 7, by=1)),
                     expand=c(0,0)) +
  scale_y_continuous("Average count of total web visits per day (log)", limits = c(0, 0.32)) +
  geom_linerange(aes(ymin=mean-1*se, ymax=mean+1*se, color=treatment), size=1) +
  geom_linerange(aes(ymin=mean-2*se, ymax=mean+2*se, color=treatment)) +
  scale_color_manual(values = c("darkgray", "red", "blue")) + 
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, alpha=.5) +
  facet_grid(treatment~variable) +
  theme_bw() +
  theme(legend.position="none", panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(), axis.title.x = element_blank(),
        strip.text.y=element_blank())
p_fn
p_hp <- plot_data %>%
  filter(variable == "Visits to HuffingtonPost.com") %>% 
  ggplot(aes(week, mean, color = treatment, group=treatment)) +
  scale_x_continuous("Weeks before and after treatment was administered",
                     limits=c(-4.2, +7.2), 
                     breaks=c(seq(-4, -1, by=1), seq(1, 7, by=1)),
                     expand=c(0,0)) +
  scale_y_continuous("Average count of total web visits per day (log)", limits = c(0, 0.32)) +
  geom_linerange(aes(ymin=mean-1*se, ymax=mean+1*se, color=treatment), size=1) +
  geom_linerange(aes(ymin=mean-2*se, ymax=mean+2*se, color=treatment)) +
  scale_color_manual("Treatment group", values = c("darkgray", "red", "blue")) + 
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, alpha=.5) +
  facet_grid(treatment~variable) +
  theme_bw() +
  theme(legend.position = "none", panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(), axis.title = element_blank())
p_hp

plot_row <- plot_grid(p_fn, p_hp, rel_widths = c(.50, .50))

# now add the title
title <- ggdraw() + 
  draw_label(
    "Weeks before and after treatment was administered",
    x = .5
  ) + theme(plot.margin = margin(0, 0, 0, 0))
p_all <- plot_grid(
  plot_row, title,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(1, .1)
)

ggsave(p_all, file="graphs/main-fig2.pdf", height=5, width=8)
ggsave(p_all, file="graphs/main-fig2.png", height=5, width=8)


# FIRST STAGE: CHANGE IN VISITS TO FOX NEWS / HUFFINGTON POST
#===============================================================================

pd <- pulse %>% 
  filter(!is.na(treatment)) %>% # dropping those who are not part of the experiment: NA
  filter(date!="2018-11-19") %>% # drop the data from 2018-11-19 because of missing data
  filter(timing >= (-30) & timing <= 50) %>%
  mutate(week = ifelse(timing>=0, ceiling((timing+1)/7),
                       floor(timing/7))) %>% 
  filter(week == 1) %>%
  group_by(id, treatment) %>% 
  summarize(mean_fn = mean(totalfn_day),
            mean_hp = mean(totalhp_day),
  ) %>% 
  ungroup()


# Change in visits in week after encouragement, compared to control
pd %>% filter(treatment %in% c("Fox News group", "Control group")) %>% 
  lm(mean_fn ~ treatment, data=.) %>% summary()

pd %>% filter(treatment %in% c("HuffPost group", "Control group")) %>% 
  lm(mean_hp ~ treatment, data=.) %>% summary()

# FIRST STAGE: CHANGE IN DURATION OF VISITS TO FOX NEWS / HUFFINGTON POST
#===============================================================================

pd <- pulse %>% 
  filter(!is.na(treatment)) %>% # dropping those who are not part of the experiment: NA
  filter(date!="2018-11-19") %>% # drop the data from 2018-11-19 because of missing data
  filter(timing >= (-30) & timing <= 50) %>%
  mutate(week = ifelse(timing>=0, ceiling((timing+1)/7),
                       floor(timing/7))) %>% 
  filter(week == 1) %>%
  group_by(id, treatment) %>% 
  summarize(meandurfn_week=mean(dur_fn_day),
            meandurhp_week=mean(dur_hp_day)
  ) %>% 
  ungroup()

# Change in duration of visits to treatment sites after encouragement
pd %>% filter(treatment %in% c("Fox News group", "Control group")) %>% 
  lm(meandurfn_week ~ treatment, data=.) %>% summary()

pd %>% filter(treatment %in% c("HuffPost group", "Control group")) %>% 
  lm(meandurhp_week ~ treatment, data=.) %>% summary()

# CHANGE IN VISITS TO FOX NEWS / HUFFINGTON POST 8 WEEKS AFTER TRTMENT
#===============================================================================

pd <- pulse %>% 
  filter(!is.na(treatment)) %>% # dropping those who are not part of the experiment: NA
  filter(date!="2018-11-19") %>% # drop the data from 2018-11-19 because of missing data
  filter(timing >= (-30) & timing <= 50) %>%
  mutate(week = ifelse(timing>=0, ceiling((timing+1)/7),
                       floor(timing/7))) %>% 
  filter(week == 8) %>%
  group_by(id, treatment) %>% 
  summarize(mean_fn = mean(totalfn_day),
            mean_hp = mean(totalhp_day),
  ) %>% 
  ungroup()


# Change in visits in week after encouragement, compared to control
pd %>% filter(treatment %in% c("Fox News group", "Control group")) %>% 
  lm(mean_fn ~ treatment, data=.) %>% summary()

pd %>% filter(treatment %in% c("HuffPost group", "Control group")) %>% 
  lm(mean_hp ~ treatment, data=.) %>% summary()

# Number of compliers according to pre-registered metric
#===============================================================================

(svy %>% 
  filter(W3_PATA306_treatment_w3=="FoxNews") %>% 
  summarise(mean(comp_fn, na.rm=TRUE))) - # treated = compliers + always-takers
(svy %>% 
  filter(W3_PATA306_treatment_w3=="Control") %>% 
  summarise(mean(comp_fn, na.rm=TRUE))) # always-takers

(svy %>% 
    filter(W3_PATA306_treatment_w3=="HuffPost") %>% 
    summarise(mean(comp_hp, na.rm=TRUE))) - # treated = compliers + always-takers
  (svy %>% 
     filter(W3_PATA306_treatment_w3=="Control") %>% 
     summarise(mean(comp_hp, na.rm=TRUE))) # always-takers

# Similar computation but with model
lm_robust(comp_fn ~ W3_PATA306_treatment_w3, data = 
            filter(svy, W3_PATA306_treatment_w3 != "HuffPost"))

lm_robust(comp_hp ~ W3_PATA306_treatment_w3, data = 
            filter(svy, W3_PATA306_treatment_w3 != "FoxNews"))

# % of users who increased visits to assigned site
svy %>% 
  filter(!is.na(log_fn_6w) & !is.na(log_fn_pre)) %>% 
  # computing raw counts from logs
  mutate(fn_6w = exp(log_fn_6w) - 1,
         fn_pre = exp(log_fn_pre) - 1,
         hp_6w = exp(log_hp_6w) - 1,
         hp_pre = exp(log_hp_pre) - 1) %>% 
  group_by(W3_PATA306_treatment_w3) %>% 
  summarise(fn_increase = mean(fn_6w>fn_pre, na.rm=TRUE),
            hp_increase = mean(hp_6w>hp_pre, na.rm=TRUE))

# FIGURE 3
#===============================================================================

# potential control variables across all models
vars <- c("party7", "age", "agesq", "female", "raceeth", "educ",
          "ideo", "income", "employ", "state", "polint", "freq_tv", "freq_np", 
          "freq_rad", "freq_net", "freq_disc", "log_news_pre", "diet_mean_pre")

########################################################
### News visits
########################################################

dvs <- c("weekly_log_news_plus1_1w", "weekly_log_news_plus1_4w", 
         "weekly_log_news_plus1_6w", "log_cons_1w", "weekly_log_cons_4w",
         "weekly_log_cons_6w", "log_lib_1w", "weekly_log_lib_4w", "weekly_log_lib_6w")

ests <- NULL
for(dv in dvs) {
  dat <- tidy(run_model(dv = dv, trt = "FoxNews")[[1]])[2,]
  dat$trt <- "Fox News"
  ests <- rbind(ests, dat)
  dat <- tidy(run_model(dv = dv)[[1]])[2,]
  dat$trt <- "HuffPost"
  ests <- rbind(ests, dat)
}

rownames(ests) <- NULL
ests <- ests %>% select(estimate, conf.low, conf.high, std.error, p.value, outcome, trt)

########################################################
### Twitter link shares (lib)
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

dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat$outcome <- "Link shares on Twitter"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat)

########################################################
### Twitter link shares (cons)
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
itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, 
                 blocks="chrome", more_vars = "log_total_links_pre")
compute_proportion_missing_covars(itt)
## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% 
  dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat$outcome <- "Link shares on Twitter"
dat$trt <- "Fox News"
ests <- rbind(ests, dat)

########################################################
### Event knowledge: HP group
########################################################

trt <- "HuffPost"
dv <- "event_mokken"
dv_pre <- "event_pre_mokken"
D <- "comp_hp"

# standardizing event knowledge variable
svy$event_mokken <- svy$event_mokken/
  sd(svy$event_mokken[which(
    svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, dv_pre = dv_pre)

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat <- cace %>% tidy() %>% filter(term == "comp_hp") %>% dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat$outcome <- "Event knowledge"
dat$trt <- "HuffPost"
ests <- rbind(ests, dat)


########################################################
### Event knowledge: FN group
########################################################

trt <- "FoxNews"
dv <- "event_mokken"
dv_pre <- "event_pre_mokken"
D <- "comp_fn"

# standardizing event knowledge variable
svy$event_mokken <- svy$event_mokken/
  sd(svy$event_mokken[which(
    svy$W3_PATA306_treatment_w3 == "Control")], na.rm = TRUE)

## ITT, with Lin's covariate adjustment
itt <- run_model(dv = dv, trt = trt, dv_pre = dv_pre)

## CACE
cace <- estimate_cace(Y=dv, D = D, Z = "W3_PATA306_treatment_w3",
                      X = extract_covariates(itt), trt=trt)

dat <- cace %>% tidy() %>% filter(term == "comp_fn") %>% 
  dplyr::select(estimate, conf.low, conf.high, std.error, p.value)
dat$outcome <- "Event knowledge"
dat$trt <- "Fox News"
ests <- rbind(ests, dat)

########################################################
### Producing graph
########################################################

ests$type <- str_sub(ests$outcome, 1, -4)
ests$type <- str_remove(ests$type, "weekly_")
ests$type <- c("News visits", "Conservative news visits", "Liberal news visits", 
               "Liberal/conservative\n link shares on Twitter", "Event knowledge")[match(ests$type, unique(ests$type))]
ests$outcome[seq(1, 21, by = 2)] <- c("1 week", "4 weeks", "6 weeks",
                                      "1 week", "4 weeks", "6 weeks", 
                                      "1 week", "4 weeks", "6 weeks", "4 weeks", "Wave 4")
ests$outcome[seq(2, 22, by = 2)] <- c("1 week", "4 weeks", "6 weeks",
                                      "1 week", "4 weeks", "6 weeks", 
                                      "1 week", "4 weeks", "6 weeks", "4 weeks", "Wave 4")

ests$type <- factor(ests$type, levels = c("News visits", "Conservative news visits", "Liberal news visits", 
                                          "Liberal/conservative\n link shares on Twitter", "Event knowledge"))

g <- ggplot(data = filter(ests), aes(y=estimate, x=outcome, color=trt)) + 
  facet_grid(~ type, scales = "free_x") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high), position=position_dodge(width=-.5))+
  scale_color_manual("Treatment", values=c("red", "blue")) +
  ylim(-.7, 1.5) + xlab("") + ylab("") +
  theme_minimal() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45))
g

ggsave(g, file = "graphs/main-fig3.pdf", width = 10, height = 6)
ggsave(g, file = "graphs/main-fig3.png", width = 10, height = 6)

# results highlighted in manuscript text
ests %>% filter(trt == "Fox News" & 
                  type == "Conservative news visits")
