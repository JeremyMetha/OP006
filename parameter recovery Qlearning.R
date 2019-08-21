rm(list = ls())

source("simulate_player.R")
source("qlearning_model.R")
source("fit_qlearning.R")
source("eval_loglike.R")

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


alphas = seq(from = 0.01, to = 1, by = 0.05)  #learning rates
betas = seq(from= 1, to = 100, by = 5)

df = tibble(true_alpha = numeric(),
                true_beta = numeric(),
                est_alpha = numeric(),
                se_alpha = numeric(),
                est_beta = numeric(),
                se_beta = numeric(),
                diff_alpha = numeric(),
                diff_beta= numeric(),
                fit = numeric(),
                flag = character(), stringsAsFactors = FALSE)
for (i in 1:length(alphas)){
  alpha = alphas[i]
  for (j in 1:length(betas)){
    beta = betas[j]
    print(i)
    print(j)
    simulation = simulate_player(alpha, beta, rewardprob)
    choice = simulation$choice
    reward = simulation$reward
    inx = c(0.5, 1) #starting values for alpha and beta
    result_qlearn = fit_qlearning(inx, choice, reward)
    result = list(true_alpha = alpha,
         true_beta = beta,
         est_alpha = result_qlearn$alpha,
         se_alpha = result_qlearn$alphase,
         est_beta = result_qlearn$beta,
         se_beta = result_qlearn$betase,
         diff_alpha = alpha - result_qlearn$alpha,
         diff_beta= beta - result_qlearn$beta,
         fit = result_qlearn$pseudoR2,
         flag = as.character(result_qlearn$exitflag))
    df <- rbind(df, result, stringsAsFactors = FALSE)
  }
}
save(df, file ="qlearning recovery.Rda")
