rm(list = ls())


source("simulate_dualalphas.R")
source("fit_dualalphas.R")
source("dualalphas_model.R")
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


alphas_pos = seq(from = 0.1, to = 0.9, by = 0.1)  #learning rates
alphas_neg = seq(from = 0.1, to = 0.9, by = 0.1)
betas = seq(from= 1, to = 100, by = 10)

df = tibble(true_alpha_pos = numeric(),
            true_alpha_neg = numeric(),
            true_beta = numeric(),
            est_alpha_pos = numeric(),
            se_alpha_pos = numeric(),
            est_alpha_neg = numeric(),
            se_alpha_neg = numeric(),
            est_beta = numeric(),
            se_beta = numeric(),
            diff_alpha_pos = numeric(),
            diff_alpha_neg = numeric(),
            diff_beta = numeric(),
            fit = numeric(),
            flag = character(), stringsAsFactors = FALSE)
for (i in 1:length(alphas_pos)){
  alpha_pos = alphas_pos[i]
  for (j in 1:length(betas)){
    beta = betas[j]
    for (k in 1:length(alphas_neg)){
      alpha_neg = alphas_neg[k]
      print(i)
      print(j)
      print(k)
      simulation = simulate_dualalphas(alpha_pos, alpha_neg, beta, rewardprob)
      choice = simulation$choice
      reward = simulation$reward
      inx = c(0.5, 0.5, 1)
      try({result_qlearn = fit_dualalphas(inx, choice, reward)
      result = list(true_alpha_pos = alpha_pos,
         true_alpha_neg = alpha_neg,
         true_beta = beta,
         est_alpha_pos = result_qlearn$alpha_pos,
         se_alpha_pos = result_qlearn$alpha_pos_se,
         est_alpha_neg = result_qlearn$alpha_neg,
         se_alpha_neg = result_qlearn$alpha_neg_se,
         est_beta = result_qlearn$beta,
         se_beta = result_qlearn$betase,
         diff_alpha_pos = alpha_pos - result_qlearn$alpha_pos,
         diff_alpha_neg = alpha_neg - result_qlearn$alpha_neg,
         diff_beta= beta - result_qlearn$beta,
         fit = result_qlearn$pseudoR2,
         flag = as.character(result_qlearn$exitflag))
    df <- rbind(df, result, stringsAsFactors = FALSE)}, silent = TRUE)
    }
  }
}
save(df, file ="dualalphasrecovery.Rda")


