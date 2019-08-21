rm(list = ls())
library(tidyverse)
library(optimx)


# fitting to all models

cohort <- read_csv("cohort.csv")


# Need to bust cohort apart into individual animals and strip out omissions, additionally switch sides acccording to learning or reversal

# High in learning = 1, high in reversal = 0
# Also need reward vector, rewarded = 1, nonrewarded = 0
# and day vecto, as long as days are diffrent from each other, it's all good.

dfs <- cohort %>%
  filter(Response != "Omit") %>%
  mutate(Reward = as.numeric(Reward == "Win")) %>%
  mutate(Response = case_when(SessionOrder %in% c("80:20 Learning", "70:30 Learning", "60:40 Learning") & Response == "High" ~ 1,
                              SessionOrder %in% c("80:20 Learning", "70:30 Learning", "60:40 Learning") & Response == "Low" ~ 0,
                              SessionOrder %in% c("80:20 Reversal", "70:30 Reversal", "60:40 Reversal") & Response == "High" ~ 0,
                              SessionOrder %in% c("80:20 Reversal", "70:30 Reversal", "60:40 Reversal") & Response == "Low" ~ 1)) %>%
  select(Day, Subject, Response, Reward, SessionOrder)
# subjects 2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16


# qlearning

source("simulate_player.R")
source("qlearning_model.R")
source("fit_qlearning.R")
source("eval_loglike.R")
inx = c(0.5, 1)
d = as.data.frame(list("alpha" = NA, "alphase" = NA, "beta" = NA, "betase" = NA, "aic" = NA, "bic" = NA))
for (i in c(2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16)){
  m2 <- dfs %>%
    filter(Subject == i)
  choice = m2$Response
  reward = m2$Reward
  day = m2$Day
  result_qlearn = fit_qlearning(inx, choice, reward)
  d2 = as.data.frame(list("alpha" = result_qlearn$alpha, "alphase" = result_qlearn$alphase, "beta" = result_qlearn$beta, "betase" = result_qlearn$betase, "aic" = result_qlearn$AIC, "bic" = result_qlearn$BIC))
  d = rbind(d, d2)
}
qlearning = d %>%
  mutate(Subject = c(NA,2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16))


#perseveration
source("qlearning_model.R")
source("simulate_perseverative.R")
source("fit_perseverative.R")
source("eval_perseverative.R")

inx = c(0.5, 1, 0)
d = as.data.frame(list("alpha" = NA, "alphase" = NA, "beta" = NA, "betase" = NA, "delta" = NA, "deltase" = NA, "aic" = NA, "bic" = NA))
for (i in c(2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16)){
  m2 <- dfs %>%
    filter(Subject == i)
  choice = m2$Response
  reward = m2$Reward
  day = m2$Day
  result_qlearn = fit_perseverative(inx, choice, reward)
  d2 = as.data.frame(list("alpha" = result_qlearn$alpha, "alphase" = result_qlearn$alphase, "beta" = result_qlearn$beta, "betase" = result_qlearn$betase,  "delta" = result_qlearn$delta, "deltase" = result_qlearn$deltase,  "aic" = result_qlearn$AIC, "bic" = result_qlearn$BIC))
  d = rbind(d, d2)
}
perseveration = d %>%
  mutate(Subject = c(NA,2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16))

#dualalphas

source("simulate_dualalphas.R")
source("fit_dualalphas.R")
source("dualalphas_model.R")
source("eval_loglike.R")

inx = c(0.5, 0.5, 1)

d = as.data.frame(list("alphapos" = NA, "alphaposse" = NA,"alphaneg" = NA, "alphanegse" = NA, "beta" = NA, "betase" = NA, "aic" = NA, "bic" = NA))
for (i in c(2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16)){
  m2 <- dfs %>%
    filter(Subject == i)
  choice = m2$Response
  reward = m2$Reward
  day = m2$Day
  result_qlearn = fit_dualalphas(inx, choice, reward)
  d2 = as.data.frame(list("alphapos" = result_qlearn$alpha_pos, "alphaposse" = result_qlearn$alpha_pos_se, "alphaneg" = result_qlearn$alpha_neg, "alphanegse" = result_qlearn$alpha_neg_se, "beta" = result_qlearn$beta, "betase" = result_qlearn$betase,  "aic" = result_qlearn$AIC, "bic" = result_qlearn$BIC))
  d = rbind(d, d2)
}
dualalphas = d %>%
  mutate(Subject = c(NA,2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16))

# dual alphas + perseveration

source("fit_dualpers.R")
source("dualalphas_model.R")
source("eval_perseverative.R")
inx = c(0.5, 0.5, 1, 0)
d = as.data.frame(list("alphapos" = NA, "alphaposse" = NA,"alphaneg" = NA, "alphanegse" = NA, "beta" = NA, "betase" = NA, "delta" = NA, "deltase" = NA, "aic" = NA, "bic" = NA))

for (i in c(2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16)){
  m2 <- dfs %>%
    filter(Subject == i)
  choice = m2$Response
  reward = m2$Reward
  day = m2$Day
  result_qlearn = fit_dualpers(inx, choice, reward)
  d2 = as.data.frame(list("alphapos" = result_qlearn$alpha_pos, "alphaposse" = result_qlearn$alpha_pos_se,"alphaneg" = result_qlearn$alpha_neg, "alphanegse" = result_qlearn$alpha_neg_se, "beta" = result_qlearn$beta, "betase" = result_qlearn$betase,  "delta" = result_qlearn$delta, "deltase" = result_qlearn$deltase,  "aic" = result_qlearn$AIC, "bic" = result_qlearn$BIC))
  d = rbind(d, d2)
}
dualpers = d %>%
  mutate(Subject = c(NA,2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 16))


