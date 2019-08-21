rm(list = ls())
library(tidyverse)






# Reinforcement learning demo
#
# Simulates a reinforcement learner choosing in an environment that
# regularly reverses the high reward option and fits a Q-learning model and
# a linear model to behavior
#
# Refer to the following articles for further information:
#
#  Rutledge, R.B., Lazzaro, S.C., Lau, B., Myers, C.E., Gluck, M.A., and
#  Glimcher, P.W. (2009). Dopaminergic drugs modulate learning rates and
#  perseveration in Parkinson's patients in a dynamic foraging task. The
#  Journal of Neuroscience, 29(48): 15104-15114.
#
#  Lau, B. and Glimcher, P.W. (2005) Dynamic response-by-response models of
#  matching behavior in rhesus monkeys. Journal of the Experimental Analysis
#  of Behavior, 84(3): 555-79, 2005.
#
# Robb Rutledge (robb@nyu.edu)
# October 8, 2010



######################################################
#set the probabilities and number of trials
p1 = 0.8      #probability of reward for high reward option
p2 = 0.2      #probability of reward for low reward option
blocklen = 100 #number of trials per block
nblock = 10   #number of blocks, should be an even number

#set reward probabilities
P1 = rep_len(p1, blocklen)
P2 = rep_len(p2, blocklen)
P_1 = c(P1, P2)
P_2 = c(P2, P1)
rewardprob = cbind(P_1, P_2)
rewardprob = do.call("rbind", replicate(nblock/2, rewardprob, simplify = FALSE))
rm(P1, P2, P_1, P_2)

###############################################################################
# Q_LEARNING

source("simulate_player.R")
source("qlearning_model.R")
source("fit_qlearning.R")
source("eval_loglike.R")

alpha = 0.2  #learning rates
beta = 10     #noise parameter

#simulate a Q-learning player - returns a vector of the player's choices
#and the sequence of rewards received
simulation = simulate_player(alpha, beta, rewardprob)
choice = simulation$choice
reward = simulation$reward


#fit the Q-learning model to player data - 2 parameters
inx = c(0.5, 1) #starting values for alpha and beta
result_qlearn = fit_qlearning(inx, choice, reward)



#plot the player choices and the model fit

df = as.data.frame(choice)
df$trial = (1:(blocklen*nblock))
df <- df %>%
  mutate(probchoice = result_qlearn$probchoice)
ggplot(df, aes(x = trial , y = choice)) +
  geom_smooth(method = "loess", method.args = list(degree = 1), span = 0.1) +
  geom_smooth(aes(y = probchoice), method = "loess", method.args = list(degree = 1), span = 0.1, colour = "red")



#plot the fit Q-values for every trial
df$weight1 = result_qlearn$weight[,1]
df$weight2 = result_qlearn$weight[,2]
ggplot(df, aes(x = trial)) +
  geom_line(aes(y = weight1), colour = "blue") +
  geom_line(aes(y = weight2), colour = "red")

###############################################################################
# PERSEVERATION
source("qlearning_model.R")
source("simulate_perseverative.R")
source("fit_perseverative.R")
source("eval_perseverative.R")

alpha = 0.4   #learning rate
beta = 20     #noise parameter
delta = -0.3  #perseveration parameter
#perseverative model - 3 parameters

simulation = simulate_perseverative(alpha, beta, delta, rewardprob)
choice = simulation$choice
reward = simulation$reward

inx = c(0.5, 1, 0)
result_perseverative = fit_perseverative(inx, choice, reward)

# plot choice and fit
df = as.data.frame(choice)
df$trial = (1:(blocklen*nblock))
df <- df %>%
  mutate(probchoice = result_perseverative$probchoice)
ggplot(df, aes(x = trial , y = choice)) +
  geom_smooth(method = "loess", method.args = list(degree = 1), span = 0.1) +
  geom_smooth(aes(y = probchoice), method = "loess", method.args = list(degree = 1), span = 0.1, colour = "red")

#plot weights
df$weight1 = result_perseverative$weight[,1]
df$weight2 = result_perseverative$weight[,2]
ggplot(df, aes(x = trial)) +
  geom_line(aes(y = weight1), colour = "blue") +
  geom_line(aes(y = weight2), colour = "red")

###############################################################################
# DUAL ALPHAS

source("simulate_dualalphas.R")
source("fit_dualalphas.R")
source("dualalphas_model.R")

alpha_pos = 0.2 # for positive rpe
alpha_neg = 0.2 # for negative rpe
beta = 10 # noise parameter

simulation = simulate_dualalphas(alpha_pos, alpha_neg, beta, rewardprob)
choice = simulation$choice
reward = simulation$reward

inx = c(0.5, 0.5, 1)
result_dualalphas = fit_dualalphas(inx, choice, reward)

# plot choice and fit
df = as.data.frame(choice)
df$trial = (1:(blocklen*nblock))
df <- df %>%
  mutate(probchoice = result_dualalphas$probchoice)
ggplot(df, aes(x = trial , y = choice)) +
  geom_smooth(method = "loess", method.args = list(degree = 1), span = 0.1) +
  geom_smooth(aes(y = probchoice), method = "loess", method.args = list(degree = 1), span = 0.1, colour = "red")

#plot weights
df$weight1 = result_dualalphas$weight[,1]
df$weight2 = result_dualalphas$weight[,2]
ggplot(df, aes(x = trial)) +
  geom_line(aes(y = weight1), colour = "blue") +
  geom_line(aes(y = weight2), colour = "red")


###############################################################################
# Slippage

source("simulate_slippage.R")
source("fit_slippage.R")
source("slippage_model.R")
source("eval_loglike.R")

alpha = 0.05
beta = 10
gamma = 0.7
days = c(1:nblock)
day = rep(days, each = blocklen)

simulation = simulate_slippage(alpha, beta, gamma, rewardprob, day)
choice = simulation$choice
reward = simulation$reward

inx = c(0.5, 10, 0.5)

result_slippage = fit_slippage(inx, choice, reward, day)

# plot choice and fit
df = as.data.frame(choice)
df$trial = (1:(blocklen*nblock))
df <- df %>%
  mutate(probchoice = result_slippage$probchoice)
ggplot(df, aes(x = trial , y = choice)) +
  geom_smooth(method = "loess", method.args = list(degree = 1), span = 0.1) +
  geom_smooth(aes(y = probchoice), method = "loess", method.args = list(degree = 1), span = 0.1, colour = "red")

#plot weights
df$weight1 = result_slippage$weight[,1]
df$weight2 = result_slippage$weight[,2]
ggplot(df, aes(x = trial)) +
  geom_line(aes(y = weight1), colour = "blue") +
  geom_line(aes(y = weight2), colour = "red")


###############################################################################
# Slippage + Perseveration (slippers)
rm(list = ls())
source("simulate_slippers.R")
source("fit_slippers.R")
source("slippage_model.R")
source("eval_perseverative.R")

p1 = 0.8      #probability of reward for high reward option
p2 = 0.2      #probability of reward for low reward option
blocklen = 100 #number of trials per block
nblock = 10   #number of blocks, should be an even number

#set reward probabilities
P1 = rep_len(p1, blocklen)
P2 = rep_len(p2, blocklen)
P_1 = c(P1, P2)
P_2 = c(P2, P1)
rewardprob = cbind(P_1, P_2)
rewardprob = do.call("rbind", replicate(nblock/2, rewardprob, simplify = FALSE))
rm(P1, P2, P_1, P_2)


alpha = 0.05
beta = 10
delta = 0.3
gamma = 0.7
days = c(1:nblock)
day = rep(days, each = blocklen)

simulation = simulate_slippers(alpha, beta, delta, gamma, rewardprob, day)
choice = simulation$choice
reward = simulation$reward

inx = c(0.5, 10, 0, 0.5)

result_slippers = fit_slippers(inx, choice, reward, day)

df <- df %>%
  mutate(probchoice = result_slippers$probchoice)
ggplot(df, aes(x = trial , y = choice)) +
  geom_smooth(method = "loess", method.args = list(degree = 1), span = 0.1) +
  geom_smooth(aes(y = probchoice), method = "loess", method.args = list(degree = 1), span = 0.1, colour = "red")

