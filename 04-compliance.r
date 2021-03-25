#===============================================================================
#  File:    04-compliance.R
#  Date:    Feb 3, 2021
#  Purpose: replicate appendix analyses: compliance
#  Data In: 
#           ./data/survey_data.csv
#           ./data/daily_pulse_data.csv
#===============================================================================

# PACKAGES
#===============================================================================
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ivdesc)
library(lemon)

# DATA
#===============================================================================
pulse <- read_csv("data/daily_pulse_data.csv")
svy <- read_csv("data/survey_data.csv")

# COMPLIANCE: results reported in appendix
#===============================================================================

# Compliance according to pre-registered metric - Fox News
# Always takers
svy %>% 
  filter(W3_PATA306_treatment_w3=="Control") %>% 
  summarise(mean(comp_fn, na.rm=TRUE))
# Treated
svy %>% 
  filter(W3_PATA306_treatment_w3=="FoxNews") %>% 
  summarise(mean(comp_fn, na.rm=TRUE))

# Compliance according to pre-registered metric - GHuffPost
# Always takers
svy %>% 
  filter(W3_PATA306_treatment_w3=="Control") %>% 
  summarise(mean(comp_hp, na.rm=TRUE))
# Treated
svy %>% 
  filter(W3_PATA306_treatment_w3=="HuffPost") %>% 
  summarise(mean(comp_hp, na.rm=TRUE))

# Figure 7
#===============================================================================

# deleting missing web tracking data
svy <- svy %>% filter(!is.na(log_fn_6w) & !is.na(log_fn_pre)) %>% 
  mutate(fn_6w = exp(log_fn_6w) - 1,
         fn_pre = exp(log_fn_pre) - 1,
         hp_6w = exp(log_hp_6w) - 1,
         hp_pre = exp(log_hp_pre) - 1)

# descriptive statistics: change in number of visits
svy %>% group_by(W3_PATA306_treatment_w3) %>% 
  summarise(fn_increase = mean(fn_6w>fn_pre, na.rm=TRUE),
            hp_increase = mean(hp_6w>hp_pre, na.rm=TRUE))

### From respondents with 0 visits to FN or HP before treatment,
####### 35% of those in FNC group visited FN
####### 23% of those in HP group visited HP
svy %>% group_by(W3_PATA306_treatment_w3) %>% 
  summarise(fn_increase_from_zero = sum(
    fn_6w>0 & fn_pre==0, na.rm=TRUE)/
      sum(fn_pre==0, na.rm=TRUE),
    hp_increase_from_zero = sum(
      hp_6w>0 & hp_pre==0, na.rm=TRUE)/
      sum(hp_pre==0, na.rm=TRUE))

### The treatment increase number of visits by at least 10x for
####### 35% of the FN treatment group
####### 30% of the HP treatment group
svy %>% group_by(W3_PATA306_treatment_w3) %>% 
  summarise(fn_increase = mean(fn_6w>fn_pre*10, na.rm=TRUE),
            hp_increase = mean(hp_6w>hp_pre*10, na.rm=TRUE))

## graph with compliance
fn_n <- sum(svy$W3_PATA306_treatment_w3 == "FoxNews")
control_n <- sum(svy$W3_PATA306_treatment_w3 == "Control")
hp_n <- sum(svy$W3_PATA306_treatment_w3 == "HuffPost")

svy <- svy %>% 
  mutate(fn_change = fn_6w - fn_pre,
         hp_change = hp_6w - hp_pre)
df <- data.frame(
  x = rep(c(1:hp_n, 1:fn_n, 1:control_n),times=2),
  value = c(
    sort(svy$fn_change[svy$W3_PATA306_treatment_w3 == "HuffPost"], na.last = FALSE),
    sort(svy$fn_change[svy$W3_PATA306_treatment_w3 == "FoxNews"], na.last = FALSE),
    sort(svy$fn_change[svy$W3_PATA306_treatment_w3 == "Control"], na.last = FALSE),
    sort(svy$hp_change[svy$W3_PATA306_treatment_w3 == "HuffPost"], na.last = FALSE),
    sort(svy$hp_change[svy$W3_PATA306_treatment_w3 == "FoxNews"], na.last = FALSE),
    sort(svy$hp_change[svy$W3_PATA306_treatment_w3 == "Control"], na.last = FALSE)
  ),
  outlet = rep(c("FoxNews", "HuffPost"), each=nrow(svy)),
  group = rep(c(rep("HuffPost", hp_n), rep("FoxNews", fn_n), 
                rep("Control", control_n)), times=2)
)

p <- ggplot(df, aes(x=x, y=value, color=group))
pq <- p + geom_point(size=.75) +
  facet_wrap( ~ outlet, scales="free_y") +
  theme_bw() +
  scale_x_continuous(
    "Respondents (from lowest to highest change in visit count)") +
  scale_y_continuous("Change in visits to news domain",
                     limits=c(-200, 500)) +
  theme(legend.position = "bottom", 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_color_manual("Treatment group", 
                     values = c("gray80", "red", "darkgreen"))
pq

ggsave(pq, file="graphs/appendix_fig7.pdf", height=4, width=6.5)
ggsave(pq, file="graphs/appendix_fig7.png", height=4, width=6.5)

# outliers excluded from graph
table(df$value>500, df$outlet)
table(df$value<(-200), df$outlet)

# Figure 8
#===============================================================================

# Merge pulse data with survey data
pulse <- merge(pulse, 
               dplyr::select(svy, id, W3_PATA306_treatment_w3, starttime_w3, W3_Browser_treatment_w3),
               by.x = "id", by.y = "id", all.x = TRUE) %>%
  mutate(treatment = factor(W3_PATA306_treatment_w3, 
                            labels = c("Control group", "Fox News group", "HuffPost group")),
         starttime_w3 = as.Date(starttime_w3),
         timing = as.numeric(date - as.Date("2018-10-05"))) %>%
  as_tibble()

# Create plot data
plot_data <- pulse %>% 
  filter(!is.na(treatment)) %>% # dropping those who are not part of the experiment: NA
  filter(date!="2018-11-19" & date!= "2018-11-18") %>% # drop the data from 2018-11-19 because of missing data
  filter(date >= as.Date("2018-09-01") & timing <= 50) %>%   
  group_by(date, treatment) %>% 
  summarize(mean = mean(log_fn_day),
            se = sd(log_fn_day)/sqrt(n()))

p_fn <- plot_data %>%
  ggplot(aes(date, mean, color = treatment, group=treatment)) +
  scale_y_continuous("Average count of FoxNews.com visits per day (log; 95% CIs)") +
  geom_linerange(aes(ymin=mean-1*se, ymax=mean+1*se, color=treatment), size=1) +
  geom_linerange(aes(ymin=mean-2*se, ymax=mean+2*se, color=treatment)) +
  scale_color_manual("Treatment group", values = c("darkgray", "red", "blue")) + 
  geom_vline(xintercept = as.Date("2018-10-05"), lty = 2) +
  geom_hline(yintercept = 0, alpha=.5) +
  ggtitle("Change in visits to FoxNews.com per day after treatment") +
  facet_grid(treatment~.) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank())

ggsave(p_fn, file="graphs/appendix_fig8.pdf", height=5, width=8)
ggsave(p_fn, file="graphs/appendix_fig8.png", height=5, width=8)


# Figure 9
#===============================================================================

plot_data <- pulse %>% 
  filter(!is.na(treatment)) %>% # dropping those who are not part of the experiment: NA
  filter(date!="2018-11-19" & date!= "2018-11-18") %>% # drop the data from 2018-11-19 because of missing data
  filter(date >= as.Date("2018-09-01") & timing <= 50) %>%   
  group_by(date, treatment) %>% 
  summarize(mean = mean(log_hp_day),
            se = sd(log_hp_day)/sqrt(n()))

p_hp <- plot_data %>%
  ggplot(aes(date, color = treatment, group=treatment)) +
  scale_y_continuous("Average count of HuffingPost.com visits per day (log; 95% CIs)") +
  geom_linerange(aes(ymin=mean-1*se, ymax=mean+1*se, color=treatment), size=1) +
  geom_linerange(aes(ymin=mean-2*se, ymax=mean+2*se, color=treatment)) +
  scale_color_manual("Treatment group", values = c("darkgray", "red", "blue")) + 
  geom_vline(xintercept = as.Date("2018-10-05"), lty = 2) +
  geom_hline(yintercept = 0, alpha=.5) +
  ggtitle("Change in visits to HuffPost.com per day after treatment") +
  facet_grid(treatment~.) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank())

ggsave(p_hp, file="graphs/appendix_fig9.pdf", height=5, width=8)
ggsave(p_hp, file="graphs/appendix_fig9.png", height=5, width=8)


# Figure 12
#===============================================================================

# import survey data
svy_recoded <- svy %>% 
  mutate(ideo3 = factor(case_when(
    ideo %in% c(1, 2) ~ 'liberal',
    ideo == 3 ~ 'moderate',
    ideo %in% c(4, 5) ~ 'conservative',
  ), levels=c("moderate", "liberal", "conservative")),
  educ3 = factor(case_when(
    educ %in% c(1,2) ~ 'HS or less',
    educ %in% c(3,4) ~ 'Some college',
    educ %in% c(5,6) ~ 'College',
  ), levels=c('Some college', 'HS or less', 'College')))

# compliance with Fox News treatment
summary(reg1 <- lm(log_fn_4w ~ log_news_pre + log_fn_pre + log_hp_pre + ideo3 + I(age>=60) + 
                     female + educ3 + I(polint==4) + I(income>=8), 
                   data=svy_recoded %>% filter(W3_PATA306_treatment_w3 == "FoxNews")))

# compliance with Huffington Post treatment
summary(reg2 <- lm(log_hp_4w ~ log_news_pre + log_fn_pre + log_hp_pre + ideo3 + I(age>=60) + 
                     female + educ3 + I(polint==4) + I(income>=8), 
                   data=svy_recoded %>% filter(W3_PATA306_treatment_w3 == "HuffPost")))

vars <- c("Intercept", "Pre-treatment visits\nto news sites (log)",
          "Pre-treatment visits\nto Fox News (log)",
          "Pre-treatment visits\nto HuffPost (log)",
          "Ideology = Liberal", "Ideology = Conservative",
          "Age >= 60", "Gender = female", "Educ: HS or less",
          "Educ: 4-year college or more", 
          "Political interest = very high", "Above median hh income")

# graph with estimated coefficients
df <- data.frame(
  coef = c(coef(reg1), coef(reg2)),
  se = c(sqrt(diag(vcov(reg1))), sqrt(diag(vcov(reg2)))),
  var = c(names(coef(reg1)), names(coef(reg2))),
  var_label = rep(vars, times=2),
  outcome = rep(c("Fox News treatment group", 
                  "HuffPost treatment group"), each=length(coef(reg1))),
  stringsAsFactors=FALSE
)
df$var_label <- factor(df$var_label, levels=rev(vars))

p <- ggplot(df, aes(x=var_label, y=coef, color=outcome))
pq <- p + coord_flip() +
  geom_point() +
  # adding first (long, thin) line for coef +- 2 sd
  geom_linerange(aes(ymin=coef-1.96*se, ymax=coef+1.96*se, color=outcome)) +
  # adding second (short, thick) line for coef +- 1 sd
  geom_linerange(aes(ymin=coef-se, ymax=coef+se, color=outcome), size=1) +
  facet_grid(~outcome) +
  scale_color_manual("Treatment group", values = c("red", "blue")) +
  ggtitle("DV = log visits to Fox News/HuffPost, during 4 weeks after encouragement") +
  theme_bw() +
  scale_y_continuous("Estimated % increase in visits during four weeks after encouragement") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  geom_hline(yintercept=0, linetype=5, color="grey50")


ggsave(pq, file="graphs/appendix_fig12.pdf", height=5.5, width=8)
ggsave(pq, file="graphs/appendix_fig12.png", height=5.5, width=8)

# Figure 13
#===============================================================================

## prepare data ------------------------------
svy <- svy %>% 
  mutate(ideo3 = factor(case_when(
    ideo %in% c(1, 2) ~ 'liberal',
    ideo == 3 ~ 'moderate',
    ideo %in% c(4, 5) ~ 'conservative',
  ), levels=c("moderate", "liberal", "conservative")),
  educ3 = factor(case_when(
    educ %in% c(1,2) ~ 'HS or less',
    educ %in% c(3,4) ~ 'Some college',
    educ %in% c(5,6) ~ 'College',
  ), levels=c('Some college', 'HS or less', 'College')))

# recode treatment variable
svy$assignment_fn <- ifelse(svy$W3_PATA306_treatment_w3 == "FoxNews", 1, 0)
svy$assignment_hp <- ifelse(svy$W3_PATA306_treatment_w3 == "HuffPost", 1, 0)
svy$assignment_ct <- ifelse(svy$W3_PATA306_treatment_w3 == "Control", 1, 0)

# prepare covars
covar_names <- c("log_news_pre", "log_fn_pre", "log_hp_pre", 
                 "I(ideo3 == 'liberal')", "I(ideo3 == 'conservative')",
                 "I(age>=60)", "female", 
                 "I(educ3 == 'HS or less')", "I(educ3 == 'College')", 
                 "I(polint==4)", "I(income>=8)")

covar_names_labels <- c("Pre-treatment visits to:\n News sites (log)",
                        "Pre-treatment visits to:\n Fox News (log)",
                        "Pre-treatment visits to:\n HuffPost (log)",
                        "Ideology:\nLiberal", "Ideology:\nConservative",
                        "Age:\n60+", "Gender:\nFemale", "Education:\nHS or less",
                        "Education:\n4-year college or more", 
                        "Political interest:\nVery high", "Household income:\nAbove median")



## run FN treatment models ------------------------------

# keep only FN assignment + controls
dat <- filter(svy, assignment_fn == 1 | assignment_ct == 1)

# loop
out_list <- list()
for (i in seq_along(covar_names)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covar_names[i])), 
                                    D = comp_fn, 
                                    Z = assignment_fn),
                        boot = TRUE, bootn = 3000) %>%
    as.data.frame()
  out_list[[i]]$covar <- covar_names[i]
  out_list[[i]]$covar_label <- covar_names_labels[i]
  print(paste0(i, "..."))
}
out_df_fn <- bind_rows(out_list)
out_df_fn$treatment <- "Fox News"


## run HP treatment models ------------------------------

# keep only HP assignment + controls
dat <- filter(svy, assignment_hp == 1 | assignment_ct == 1)

# loop
out_list <- list()
for (i in seq_along(covar_names)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covar_names[i])), 
                                    D = comp_hp, 
                                    Z = assignment_hp),
                        boot = TRUE, bootn = 3000) %>%
    as.data.frame()
  out_list[[i]]$covar <- covar_names[i]
  out_list[[i]]$covar_label <- covar_names_labels[i]
  print(paste0(i, "..."))
}
out_df_hp <- bind_rows(out_list)
out_df_hp$treatment <- "Huffington Post"


## plot outcomes ---------------------------

# join data frames
out_df <- bind_rows(out_df_fn, out_df_hp)

# re-define group variable
out_df$group_label <- recode_factor(out_df$group, sample = "Sample", 
                                    co = "Compliers", nt = "Never-takers", 
                                    at = "Always-takers")
out_df$covar_label <- factor(out_df$covar_label, levels = covar_names_labels)

# plot
p <- ggplot(out_df, aes(group_label, mu, color = treatment)) + 
  geom_pointrange(aes(ymin = mu - 1.96*mu_se, ymax = mu + 1.96*mu_se), shape = 19, fatten = 1, size = 0.75, stat="identity", position = position_dodge(0.5)) + 
  geom_pointrange(aes(ymin = mu - 1*mu_se, ymax = mu + 1*mu_se), shape = 19, fatten = 1, size = 1, stat="identity", position = position_dodge(0.5)) + 
  facet_rep_wrap(~ covar_label,  scales='free_x', repeat.tick.labels = 'none', ncol = 3) +
  coord_flip() + 
  #theme_ipsum() + 
  scale_color_manual(values = c("red", "blue")) + 
  labs(y = substitute(y, list(y = "\nMean and 68%/95% confidence intervals"))) + 
  theme(panel.background = element_blank(), 
        axis.title.x = element_text(hjust = 0.5, size = 12), 
        axis.title.y = element_blank(), 
        axis.text = element_text(size = 12), 
        plot.caption = element_text(size = 7), 
        strip.text.x = element_text(size = 12, colour = "black"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.spacing = unit(0.25, "cm"),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.key.size = unit(1, "line"),
        panel.border = element_rect(colour = "black", fill = NA))

ggsave("graphs/appendix_fig13.pdf", height = 5, width = 4, dpi = 300, scale = 2)
ggsave("graphs/appendix_fig13.png", height = 5, width = 4, dpi = 300, scale = 2)
