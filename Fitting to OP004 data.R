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
# subjects 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16
m2 <- dfs %>%
  filter(Subject == 9)

choice = m2$Response
reward = m2$Reward
day = m2$Day

source("simulate_player.R")
source("qlearning_model.R")
source("fit_qlearning.R")
source("eval_loglike.R")
inx = c(0.5, 1) #starting values for alpha and beta
result_qlearn = fit_qlearning(inx, choice, reward)

source("qlearning_model.R")
source("simulate_perseverative.R")
source("fit_perseverative.R")
source("eval_perseverative.R")

inx = c(0.5, 1, 0)
result_perseverative = fit_perseverative(inx, choice, reward)


source("simulate_dualalphas.R")
source("fit_dualalphas.R")
source("dualalphas_model.R")
source("eval_loglike.R")

inx = c(0.5, 0.5, 1)
result_dualalphas = fit_dualalphas(inx, choice, reward)

source("simulate_slippage.R")
source("fit_slippage.R")
source("slippage_model.R")
source("eval_loglike.R")

inx = c(0.5, 10, 1)
result_slippage = fit_slippage(inx, choice, reward, day)

source("simulate_slippers.R")
source("fit_slippers.R")
source("slippage_model.R")
source("eval_perseverative.R")


inx = c(0.5, 10, 0, 0.5)
result_slippers = fit_slippers(inx, choice, reward, day)

source("simulate_slippers.R")
source("fit_2slippers.R")
source("slippers2_model.R")
source("eval_perseverative.R")

inx = c(0.5, 0.5, 10, 0, 0.5)
result_2slippers = fit_2slippers(inx, choice, reward, day)

source("fit_dualpers.R")
source("dualalphas_model.R")
source("eval_perseverative.R")

inx = c(0.5, 0.5, 1, 0)
result_dualpers = fit_dualpers(inx, choice, reward)



source("fit_dualslip.R")
source("dualslip_model.R")
source("eval_loglike.R")
inx = c(0.5, 0.5, 1, 0.5)
result_dualslip = fit_dualslip(inx, choice, reward, day)



df = as.data.frame(choice)
df$SessionOrder = m2$SessionOrder
df$trial = (1:length(choice))
df <- df %>%
  #mutate(probchoice_slippers = result_slippers$probchoice) %>%
  #mutate(probchoice_2slippers = result_2slippers$probchoice) %>%
  #mutate(probchoice_qlearning = result_qlearn$probchoice) %>%
  mutate(probchoice_dualalphas = result_dualalphas$probchoice) %>%
  mutate(probchoice_perseverative = result_perseverative$probchoice) %>%
  mutate(probchoice_dualpers = result_dualpers$probchoice)
  #mutate(probchoice_dualslip = result_dualslip$probchoice) %>%
  #mutate(day = day)
ggplot(df, aes(x = trial , y = choice)) +
  geom_rect(aes(xmin = trial, xmax = dplyr::lead(trial), ymin = -Inf, ymax = Inf, fill = SessionOrder),alpha = 0.3)+
  geom_smooth(method = "loess", method.args = list(degree = 1), span = 0.005 ,se = FALSE)  +
  #geom_smooth(aes(y = probchoice_qlearning), method = "loess", method.args = list(degree = 1), span = 0.005, colour = "red") +
  #geom_smooth(aes(y = probchoice_dualalphas), method = "loess", method.args = list(degree = 1), span = 0.005, colour = "green") +
  #geom_smooth(aes(y = probchoice_perseverative), method = "loess", method.args = list(degree = 1), span = 0.005, colour = "purple") +
  #geom_smooth(aes(y = probchoice_slippers), method = "loess", method.args = list(degree = 1), span = 0.005, colour = "orange") +
  #geom_smooth(aes(y = probchoice_2slippers), method = "loess", method.args = list(degree = 1), span = 0.005, colour = "deeppink1")+
  geom_smooth(aes(y = probchoice_dualpers), method = "loess", method.args = list(degree = 1), span = 0.005, colour = "red",se = FALSE)+
  #geom_smooth(aes(y = probchoice_dualslip), method = "loess", method.args = list(degree = 1), span = 0.005, colour = "olivedrab")+
  theme_bw()



library(zoo)
df <- df %>%
  mutate(rolledchoice = rollmean(choice, 25, fill = 0.5)) %>%
  mutate(rolledpers = rollmean(probchoice_perseverative, 25, fill = 0.5)) %>%
  mutate(rolleddual = rollmean(probchoice_dualalphas, 25, fill = 0.5)) %>%
  mutate(rolleddualpers = rollmean(probchoice_dualpers, 25, fill = 0.5))
ggplot(df, aes(x = trial , y = choice)) +
  geom_rect(aes(xmin = trial, xmax = dplyr::lead(trial), ymin = -Inf, ymax = Inf, fill = SessionOrder), alpha = 0.2)+
  geom_line(aes(y = rolledchoice), colour = "black")+
  geom_line(aes(y = rolledpers), colour = "red")+
  geom_line(aes(y = rolleddual), colour = "green")+
  geom_line(aes(y = rolleddualpers), colour = "blue")

#plot weights
df$pers = result_perseverative$weight[,1]
  df$dual = result_dualpers$weight[,1]
df <- df %>%
  mutate(TrueValue = case_when(SessionOrder == "80:20 Learning"~0.8,
                               SessionOrder == "80:20 Reversal"~0.2,
                               SessionOrder == "70:30 Learning"~0.7,
                               SessionOrder == "70:30 Reversal"~0.3,
                               SessionOrder == "60:40 Learning"~0.6,
                               SessionOrder == "60:40 Reversal"~0.4
                               ))
df <- df %>%
  mutate(probchoice_perseverative = result_perseverative$probchoice) %>%
  mutate(probchoice_dualpers = result_dualpers$probchoice)
df$dualfit = predict(loess(choice~trial, df, span = 0.1))
df$probpers = predict(loess(pers~trial, df, span = 0.1))
df$probdual = predict(loess(dual~trial, df, span = 0.1))
df$probpers = predict(loess(probchoice_perseverative~trial, df, span = 0.1))
df$probdual = predict(loess(probchoice_dualpers~trial, df, span = 0.1))
df <- df %>%
  mutate(dualfit = case_when(dualfit >= 1~1,
                             dualfit < 0~0,
                             dualfit >= 0 & dualfit < 1~dualfit)) %>%
  mutate(pers = case_when(pers >= 1~1,
                             pers < 0~0,
                             pers >= 0 & pers < 1~pers)) %>%
  mutate(dual = case_when(dual >= 1~1,
                          dual < 0~0,
                          dual >= 0 & dual < 1~dual)) %>%
  mutate(probpers = case_when(probpers >= 1~1,
                              probpers < 0~0,
                              probpers >= 0 & probpers < 1~probpers)) %>%
  mutate(probdual = case_when(probdual >= 1~1,
                              probdual < 0~0,
                              probdual >= 0 & probdual < 1~probdual))


ggplot(df, aes(x = trial, y = dualfit))+
  geom_point()

ggplot(df, aes(x = trial)) +
  #geom_rect(aes(xmin = trial, xmax = dplyr::lead(trial), ymin = -Inf, ymax = Inf, fill = SessionOrder), alpha = 0.3)+
  geom_line(aes(y = dual), colour = "purple", size = 1)+
  geom_line(aes(y = alpha), colour = "red", size = 1)+
  geom_line(aes(y = dualfit), colour = "blue", size = 1)+
  theme_bw()

sub16 <- list(result_qlearn, result_perseverative, result_dualalphas, result_2slippers)
save(sub16, file = "sub16.Rda")
sub2
sub2[1]

write_csv(df, "df9.csv")
transpoints <- df %>%
  filter(SessionOrder != lag(SessionOrder))
