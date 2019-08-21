eval_perseverative = function(choice, weight, beta, delta){

  #Inputs:
  #   CHOICE - a vector of choices with 1s for choices to option 1 (choices to option 2 could be 0s or 2s or whatever)
  #   WEIGHT - a matrix with 2 columns of weights, all >=0, computed using the standard Q-learning model; the first column is      #   the weights for option 1 and the second is the weights for option 2
  #BETA - a noise parameter, the inverse temperature
  #
  #Outputs:
  #   LOGLIKE - the total (negative) loglikelihood of a model given the
  #      player's choices, the Q-learning model weight matrix, and the
  #      player's noise parameter
  #   PROBCHOICE - a vector with the probability of choosing option 1 on each
  #      trial according to the model
  #
  #Robb Rutledge (robb@nyu.edu)
  #July 23, 2009

  eps = 1e-10
  logodds = rep(0, length(choice))
  for (n in 1:length(choice)){
    if (n > 1){ #compute log odds of choice for current trial
      logodds[n] = beta * (weight[n,1] - weight[n,2]) - delta * (1-2*choice[n-1])
    }
    else {
      logodds[1] = 0
    }
  } #compute log odds of choice for each trial
  probchoice = 1 / (1 + exp(-logodds))        #convert log odds to probability
  probchoice[probchoice == 0] = eps      #to prevent fmin crashing from a log zero
  probchoice[probchoice == 1] = 1 - eps
  loglike = - ((choice %*% log(probchoice) + (1-choice) %*% log(1-probchoice)))

  return(loglike)
}

eval_perseverative_both = function(choice, weight, beta, delta){

  #Inputs:
  #   CHOICE - a vector of choices with 1s for choices to option 1 (choices to option 2 could be 0s or 2s or whatever)
  #   WEIGHT - a matrix with 2 columns of weights, all >=0, computed using the standard Q-learning model; the first column is      #   the weights for option 1 and the second is the weights for option 2
  #BETA - a noise parameter, the inverse temperature
  #
  #Outputs:
  #   LOGLIKE - the total (negative) loglikelihood of a model given the
  #      player's choices, the Q-learning model weight matrix, and the
  #      player's noise parameter
  #   PROBCHOICE - a vector with the probability of choosing option 1 on each
  #      trial according to the model
  #
  #Robb Rutledge (robb@nyu.edu)
  #July 23, 2009

  eps = 1e-10
  logodds = rep(0, length(choice))
  for (n in 1:length(choice)){
    if (n > 1){ #compute log odds of choice for current trial
      logodds[n] = beta * (weight[n,1] - weight[n,2]) - delta * (1-2*choice[n-1])
    }
    else {
      logodds[1] = 0
    }
    }
  probchoice = 1 / (1 + exp(-logodds))        #convert log odds to probability
  probchoice[probchoice == 0] = eps      #to prevent fmin crashing from a log zero
  probchoice[probchoice == 1] = 1 - eps
  loglike = - ((choice %*% log(probchoice) + (1-choice) %*% log(1-probchoice)))

  return(list(loglike = loglike, probchoice = probchoice))
}
