dualslip_model = function(choice, reward, day, alpha_pos, alpha_neg, gamma){
#[weight] = qlearning_model(choice, reward, alpha)
#
#Inputs:
  #   CHOICE - choice vector (1 for option 1, 0 or some other number for
                              #      option 2)
#   REWARD - reward vector (1 or a positive number for reward, 0 for no
                            #      reward)
#   ALPHA - the learning rate constant
#
#Outputs:
  #   WEIGHT - a matrix with 2 columns of weights, all >=0, computed using
#      the standard Q-learning model; the first column is the weights for
#      option 1 and the second is the weights for option 2; weights are
#      initiated as 0s
#
#Robb Rutledge (robb@nyu.edu)
#July 23, 2009


  ntrial = length(choice)
  weight = rep_len(1, ntrial)
  weight = cbind(weight,weight)                                    #assume 0 for starting conditions (could fit)
  rpe = rep_len(0, ntrial)
  oldweight = c(1, 1)
  for (n in 1:ntrial-1){
    if (n > 1){
      if (day[n] != day[n-1]){
        weight[n, 1] = gamma * weight[n, 1] + (1-gamma)* oldweight[1]
        weight[n, 2] = gamma * weight[n, 2] + (1-gamma)* oldweight[2]
        oldweight = c(weight[n-1, 1], weight[n-1, 2])
      }
    }
    rpe[n] = reward[n] - weight[n, 2-choice[n]]                    #compute rpe
    if (isTRUE(rpe[n] >= 0)) {
      weight[n+1, 2-choice[n]] = (weight[n, 2-choice[n]] + alpha_pos * rpe[n]) # update chosen
      weight[n+1, 1+choice[n]] = weight[n, 1+choice[n]]
    } else {
      weight[n+1, 2-choice[n]] = (weight[n, 2-choice[n]] + alpha_neg * rpe[n]) # update chosen
      weight[n+1, 1+choice[n]] = weight[n, 1+choice[n]]
    }
  }
  return(weight)
}

# DONE!
