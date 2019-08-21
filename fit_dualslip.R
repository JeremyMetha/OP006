fit_dualslip = function(inx, choice, reward, day){

#[result] = fit_qlearning(inx, choice, reward)
#
#Inputs:
  #   INX - starting values for alpha, beta and gamma
#   CHOICE - choice vector (1 for option 1, 0 or some other number for
                            #      option 2)
#   REWARD - reward vector (1 or a positive number for reward, 0 for no
                            #      reward)
#   DAY - vector of day in which the trial is taking place, used to caluclate degree of forgetting
#Outputs:
  #   RESULT - a struct of the data and all information return by the model
#      fitting procedure
#
#Requires:
  #   qlearning_model, eval_loglike
#
#Robb Rutledge (robb@nyu.edu)
#July 23, 2009



lb = c(0, 0, 0, 0)     #lower limits for alpha, beta and gamma
ub = c(1.2, 1.2, 100, 1) #upper limits
inx0 = inx
dof = length(inx)

result = list(choice = choice,
              reward = reward,
              day = day,
              inx = inx,
              lb = lb,
              ub = ub)


optimal = optimr(par = inx0,
           fn = model,
           method = "L-BFGS-B",
           lower = lb,
           upper = ub,
           hessian = TRUE,
           choice = choice,
           reward = reward)

se = sqrt(diag(solve(optimal$hessian)))
result$AIC = 2 * dof + 2 * optimal$value
result$BIC = 2 * log(length(choice))*dof + 2 * optimal$value
result$alpha_pos = optimal$par[1]
result$alpha_pos_se = se[1]
result$alpha_neg = optimal$par[2]
result$alpha_neg_se = se[2]
result$beta = optimal$par[3]
result$betase = se[3]
result$gamma = optimal$par[4]
result$gammase = se[4]
result$modelLL = -optimal$value
result$nullmodelLL = log(0.5)*length(choice)         #LL of random-choice model
result$pseudoR2 = 1 + result$modelLL / (result$nullmodelLL) #pseudo-R2 statistic
result$exitflag = optimal$message
result$H = optimal$hessian #Hessian, sometimes bad and SE smaller than they should be
best = bestmodel(result$alpha_pos, result$alpha_neg, result$beta, result$gamma, choice, reward, day) #best fitting model
result$probchoice = best$probchoice #prob of choosing option 1 on each trial
result$weight = best$weight         #model fits Q-values for each trial

return(result)
}


model = function(x0, choice, reward){

#function to evaluate the loglikelihood of the model for parameters alpha
#and beta given the data
  alpha_pos = x0[1]
  alpha_neg = x0[2]
  beta = x0[3]
  gamma = x0[4]

  weight = dualslip_model(choice, reward, day, alpha_pos, alpha_neg, gamma) #compute the weights
  eval = eval_loglike(choice, weight, beta) #compute the likelihood
  return(eval)
}

bestmodel = function(alpha_pos, alpha_neg, beta, gamma, choice, reward, day){
  #function to evaluate the loglikelihood of the model for parameters alpha
  #and beta given the data
  alpha = alpha
  beta = beta
  gamma = gamma

  weight = dualslip_model(choice, reward, day, alpha_pos, alpha_neg, gamma) #compute the weights
  eval = eval_both(choice, weight, beta) #compute the likelihood
  eval$weight = weight
  return(eval)


}

