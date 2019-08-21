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
days = c(1:nblock)
day = rep(days, each = blocklen)


alphas = seq(from = 0.1, to = 0.9, by = 0.1)  #learning rates
betas = seq(from= 1, to = 100, by = 10)
gammas = seq(from = 0, to = 1, by = 0.1)
deltas = seq(from = -1, to = 1, by = 0.2)
inx = c(0.5, 10, 0, 0.5)

df = tibble(true_alpha = numeric(),
            true_beta = numeric(),
            true_delta = numeric(),
            true_gamma = numeric(),
            est_alpha= numeric(),
            se_alpha = numeric(),
            est_beta = numeric(),
            se_beta = numeric(),
            est_delta = numeric(),
            se_delta= numeric(),
            est_gamma = numeric(),
            se_gamma= numeric(),
            diff_alpha = numeric(),
            diff_beta = numeric(),
            diff_delta = numeric(),
            diff_gamma= numeric(),
            fit = numeric(),
            flag = character(), stringsAsFactors = FALSE)
for (i in 1:length(alphas)){
  alpha = alphas[i]
  for (j in 1:length(betas)){
    beta = betas[j]
    for (k in 1:length(deltas)){
      delta = deltas[k]
      for (l in 1:length(gammas)){
        gamma = gammas[l]
        print(i)
        print(j)
        print(k)
        print(l)
        simulation = simulate_slippers(alpha, beta, delta, gamma, rewardprob, day)
        choice = simulation$choice
        reward = simulation$reward
        try({result_qlearn = fit_slippers(inx, choice, reward, day)
        result = list(true_alpha = alpha,
                      true_beta = beta,
                      true_delta = delta,
                      true_gamma = gamma,
                      est_alpha = result_qlearn$alpha,
                      se_alpha = result_qlearn$alphase,
                      est_beta = result_qlearn$beta,
                      se_beta = result_qlearn$betase,
                      est_delta = result_qlearn$delta,
                      se_delta = result_qlearn$deltase,
                      est_gamma = result_qlearn$gamma,
                      se_gamma = result_qlearn$gammase,
                      diff_alpha = alpha - result_qlearn$alpha,
                      diff_beta = beta - result_qlearn$beta,
                      diff_delta = delta - result_qlearn$delta,
                      diff_gamma = gamma - result_qlearn$gamma,
                      fit = result_qlearn$pseudoR2,
                      flag = as.character(result_qlearn$exitflag))
        df <- rbind(df, result, stringsAsFactors = FALSE)}, silent = TRUE)
      }
    }
  }
}

save(df, file ="slippersrecovery.Rda")


