simulate_dualalphas = function(alpha_pos, alpha_neg, beta, rewardprob){

#Inputs:
#   ALPHAPOS - learning rate constant for positive RPE
#   ALPHANEG - learning rate constant for negative RPE
#   BETA - noise parameter, the inverse temperature
#   REWARDPROB - matrix with 2 columns, first column is probs of reward
#      for option 1 for each trial (p<=1), second column is for option 2
#
#Outputs:
  #   CHOICE - a vector of choices with 1s for choices to option 1, 0s
#      for choices to option 2
#   REWARD - a vector of rewards with 1s for rewarded and 0s for unrewarded
#      trials
#
#Robb Rutledge (robb@nyu.edu)
#July 23, 2009



  ntrial = dim(rewardprob)[1]
  choice = rep_len(0, ntrial)    #1 for option 1, 0 for option 2

  reward = rep_len(0, ntrial)
  w1 = rep_len(0, ntrial)        #weight for option 1
  w2 = rep_len(0, ntrial)        #weight for option 2
  logodds = rep_len(0, ntrial)
  p1choice = rep_len(0, ntrial)
  rpe = rep_len(0, ntrial)

  for (n in 1:ntrial){
    if (n > 1){ #compute log odds of choice for current trial
      logodds[n] = beta * (w1[n] - w2[n])
      } else {
      logodds[1] = 0
      }
    p1choice[n] = 1 / (1 + exp(-logodds[n])) #convert to prob
    choice[n] = as.numeric(runif(1) < p1choice[n])           #pick rand and determine choice
    if (choice[n] == 1){                        #if option 1 chosen
      reward[n] =  as.numeric(runif(1) < rewardprob[n, 1])    #determine if rewarded
      rpe[n] = reward[n] - w1[n]           #compute rpe

    } else {
      reward[n] =  as.numeric(runif(1) < rewardprob[n, 2])    #determine if rewarded
      rpe[n] = reward[n] - w2[n]
    }
    if (rpe[n] > 0) {
      alpha = alpha_pos
    } else {
      alpha = alpha_neg
    }
    if (n < ntrial){
      if (choice[n] == 1){
        w1[n+1] = w1[n] + alpha * rpe[n] #update Q-value
        w2[n+1] = w2[n]                  #unchosen option Q-value stays the same
      } else {
        w2[n+1] = w2[n] + alpha * rpe[n] #update Q-value
        w1[n+1] = w1[n]                  #unchosen option Q-value stays the same
      }
      }
  }
  return(list(choice = choice, reward = reward))
}

# DONE!


