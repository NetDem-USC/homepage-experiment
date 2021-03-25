#===============================================================================
#  File:    03-descriptive-statistics.R
#  Date:    Feb 3, 2021
#  Purpose: replicate appendix analyses: panel attrition, issue scaling, agenda
#            setting, and compiling descriptive statistics
#  Data In: 
#           ./data/survey_data.csv
#           ./data/daily_pulse_data.csv
#===============================================================================

# PACKAGES
#===============================================================================
library(readr)
library(tidyverse)
library(estimatr)
library(ggplot2)
library(psych)
library(haven)
# devtools::install_github("hofnerb/papeR")
library(papeR)
source('code/functions.r')

# DATA
#===============================================================================
pulse <- read_csv("data/daily_pulse_data.csv")
svy <- read_csv("data/survey_data.csv")

# FIGURE 2: PANEL ATTRITION
#===============================================================================

# completes
cmp <- list()
w1_extra <- rep(TRUE, 212)

cmp[[1]] <- c(!is.na(svy$W1_endtime), w1_extra)
cmp[[2]] <- c(!is.na(svy$W2_endtime), !w1_extra)
cmp[[3]] <- c(!is.na(svy$endtime_w3), !w1_extra)
cmp[[4]] <- c(!is.na(svy$endtime), !w1_extra)
cmp[[5]] <- c(!is.na(svy$W5_endtime), !w1_extra)
cmp[[6]] <- c(!is.na(svy$W6_endtime), !w1_extra)
cmp[[7]] <- c(!is.na(svy$endtime_w7), !w1_extra)
cmp[[8]] <- c(!is.na(svy$endtime_w8), !w1_extra)

# how many completed waves 2, 3, 4?
(tab <- table(cmp[[2]] & cmp[[3]] & cmp[[4]]))
(1339 - 1037)/1339

res <- expand.grid(x = 1:8, y = 1:8, n = NA, mis = NA)

for (i in 1:8){
  for (j in 1:8){
    sl <- which(res$x==i & res$y==j)
    if (i == j){
      res$n[sl] <- sum(cmp[[i]])
      res$mis[sl] <- 1 - mean(cmp[[i]])
    }
    if (i != j){
      xy <- cmp[[i]][ cmp[[j]] ]
      res$n[sl] <- sum(xy)
      res$mis[sl] <- 1 - mean(xy)
    }
  }
  
}
res$label <- paste0(res$n, "\n(", display(res$mis, pct=TRUE), "%)")
res$label[res$x==res$y] <- paste0("N=", res$n[res$x==res$y])

p <- ggplot(res[res$x>=res$y,], aes(x=x, y=y, fill=mis))
pq <- p + geom_tile() + theme_linedraw() +
  geom_text(aes(label=label), color="white", size=5) +
  scale_fill_continuous("% attrition", 
                        labels=scales::percent_format(accuracy=1)) +
  scale_x_continuous("Respondent wave of comparison",
                     breaks=1:8, expand=c(0,0)) +
  scale_y_continuous("Respondent's first wave", breaks=1:8,
                     expand=c(0,0)) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank())
pq

ggsave(pq, file="graphs/appendix_fig2.pdf", width=10, height=6)
ggsave(pq, file="graphs/appendix_fig2.png", width=10, height=6)


# TABLE 4: ISSUE OPINIONS SCALE
#===============================================================================

psych::principal(svy %>% dplyr::select("policy1_gc_pre", "policy3_nafta_pre", "policy4_pp_pre", 
                                             "policy5_biz1_pre", "policy6_iso_pre", "policy8_biz2_pre", "policy7_ss",
                                             "policy9_nk_pre", "policy10_harass_pre", "policy11_islam_pre", "policy12_cc_pre", 
                                             "policy13_fbi_pre", "policy14_imm_pre"), 
                 nfactors = 2, rotate = "varimax", missing=TRUE, impute = "mean")$loadings


# TABLE 5: IMMIGRATION SCALE
#===============================================================================

principal(filter(svy, W3_PATA306_treatment_w3 == "Control") %>% 
                   dplyr::select(policy14_imm, imm2, imm3), 
          nfactors = 2, rotate = "varimax")$loadings


# FIGURE 5: AGENDA-SETTING
#===============================================================================

# agenda setting
agendas <- array(NA, 21)
for(i in 1:21) {
  tmp <- as.numeric(gsub(2, 1, (as.numeric(gsub(2, 0, 
              get(paste0("W2_PATA2_1_m_", i), svy))) + 
                as.numeric(gsub(2, 0, get(paste0("W3_PATA300_", i, "_w3"), svy))))))
  agendas[i] <- mean(tmp[which(svy$partylean == "Democrat")], na.rm = TRUE) - 
    mean(tmp[which(svy$partylean == "Republican")], na.rm = TRUE)
}

df <- data.frame(
  topics = c("Economy/unemployment", "Relationship with North Korea",
             "Relationship with Western countries", "Intl trade imbalances",
             "Immigration", "Terrorism", "Inequality",
             "Racism", "Morality and values", "Health care",
             "Crime", "Islam", "Fake news", "Political polarization",
             "Donald Trump and his administration", "Gun control",
             "Women's rights", "Identity politics", "Alt-right movement",
             "Black Lives Matter", "Free speech"),
  lean = agendas,
  color = ifelse(agendas>0, "blue", "red")
)
df <- df[order(df$lean),]
df$topics <- factor(df$topics, levels=df$topics)

p <- ggplot(df, aes(x=topics, y=lean, fill=color))
pq <- p + geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("blue", "red")) +
  theme_minimal() +
  geom_text(data=df[df$lean>0,],
            aes(label=topics, x=topics, y=-0.01), hjust=1, size=3) +
  geom_text(data=df[df$lean<0,],
            aes(label=topics, x=topics, y=0.01), hjust=0, size=3) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="none", 
        panel.grid.major.y = element_blank()) +
  scale_y_continuous("Partisan asymmetry in agenda setting, by topic")
pq

ggsave(pq, file="graphs/appendix_fig5.pdf", width=10, height=4)
ggsave(pq, file="graphs/appendix_fig5.png", width=10, height=4)


# TABLE 6: DESCRIPTIVE STATISTICS
#===============================================================================

# Dropping observations where treatment is missing
svy <- svy[!is.na(svy$W3_PATA306_treatment_w3),]

svy$age_labels <- cut(svy$age, breaks = c(min(svy$age, na.rm = TRUE), 29, 44, 59, 
                                   max(svy$age, na.rm = TRUE)), 
               labels = rev(c("60+", "45-59", "30-44", "18-29")), 
               include.lowest = TRUE, right = TRUE)
svy$pid3 <- as_factor(svy$W1_pid3)
svy$gender <- as_factor(svy$W1_gender)
svy$educ_factor <- as_factor(svy$W1_educ)
svy$raceeth <- as_factor(svy$raceeth)

print.xtable(xtable(papeR::summarize(svy, 
      variable.labels = c("Party ID", "Gender", "Race", "Education level", "Age group"),
      type = "factor", 
      variables = c("pid3", "gender", "raceeth", "educ_factor", "age_labels"))), 
      include.rownames = FALSE, hline.after = c(3, 5, 9, 15), 
    only.contents = TRUE, include.colnames = FALSE)

# TABLES 7 & 8: COVARIATE BALANCE
#===============================================================================

# Dropping observations where treatment is missing
svy <- svy[!is.na(svy$W3_PATA306_treatment_w3),]

vars <- c("party7", "age", "agesq", "female", "raceeth", "educ",
          "ideo", "income", "employ", "state", "polint", "freq_tv", "freq_np", 
          "freq_rad", "freq_net", "freq_disc", "log_news_pre", "diet_mean_pre")

dat <- svy %>% select(W3_PATA306_treatment_w3, W3_Browser_treatment_w3, vars[1:2], vars[4], 
                      vars[6:8], vars[12:18]) %>% 
  filter(W3_PATA306_treatment_w3 != "HuffPost")

dat %>% select(-W3_PATA306_treatment_w3, -W3_Browser_treatment_w3) %>%
  map(~ difference_in_means(.x ~ W3_PATA306_treatment_w3, blocks = W3_Browser_treatment_w3, 
                            data = dat)) %>% map_df(tidy, .id = "var") %>% 
  select(var, estimate, p.value) %>% knitr::kable("latex", digits = 3, 
         caption = "Balance: Fox News treatment vs. Control")


dat <- svy[svy$W3_PATA306_treatment_w3 != "FoxNews",
           c("W3_PATA306_treatment_w3", "W3_Browser_treatment_w3", 
             vars[1:2], vars[4], vars[6:8], vars[12:18])]

dat %>% select(-W3_PATA306_treatment_w3, -W3_Browser_treatment_w3) %>%
  map(~ difference_in_means(.x ~ W3_PATA306_treatment_w3, blocks = W3_Browser_treatment_w3, 
                            data = dat)) %>% map_df(tidy, .id = "var") %>% 
  select(var, estimate, p.value) %>% knitr::kable("latex", digits = 3, 
      caption = "Balance: HuffPost treatment vs. Control")

