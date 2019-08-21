fit_perseverative = function(inx, choice, reward){

  #[result] = fit_qlearning(inx, choice, reward)
  #
  #Inputs:
  #   INX - starting values for alpha and beta
  #   CHOICE - choice vector (1 for option 1, 0 or some other number for
  #      option 2)
  #   REWARD - reward vector (1 or a positive number for reward, 0 for no
  #      reward)
  #
  #Outputs:
  #   RESULT - a struct of the data and all information return by the model
  #      fitting procedure
  #
  #Requires:
  #   qlearning_model, eval_loglike
  #
  #Robb Rutledge (robb@nyu.edu)
  #July 23, 2009



  lb = c(0, 0, -Inf)     #lower limits for alpha, beta and delta
  ub = c(1.2, 100, Inf) #upper limits
  inx0 = inx
  dof = length(inx)

  result = list(choice = choice,
                reward = reward,
                inx = inx,
                lb = lb,
                ub = ub)


  optimal = optimr(par = inx0,
                   fn = model_perseverative,
                   method = "L-BFGS-B",
                   lower = lb,
                   upper = ub,
                   hessian = TRUE,
                   choice = choice,
                   reward = reward)

  se = sqrt(diag(solve(optimal$hessian)))
  result$AIC = 2 * dof + 2 * optimal$value
  result$BIC = 2 * log(length(choice))*dof + 2 * optimal$value
  result$alpha = optimal$par[1]
  result$alphase = se[1]
  result$beta = optimal$par[2]
  result$betase = se[2]
  result$delta = optimal$par[3]
  result$deltase = se[3]
  result$modelLL = -optimal$value
  result$nullmodelLL = log(0.5)*length(choice)         #LL of random-choice model
  result$pseudoR2 = 1 + result$modelLL / (result$nullmodelLL) #pseudo-R2 statistic
  result$exitflag = optimal$message
  result.H = optimal$hessian #Hessian, sometimes bad and SE smaller than they should be
  best = bestmodel_perseverative(result$alpha, result$beta, result$delta, choice, reward) #best fitting model
  result$probchoice = best$probchoice #prob of choosing option 1 on each trial
  result$weight = best$weight         #model fits Q-values for each trial

  return(result)
}



model_perseverative = function(x0, choice, reward){

  #function to evaluate the loglikelihood of the model for parameters alpha
  #and beta given the data
  alpha = x0[1]
  beta = x0[2]
  delta = x0[3]

  weight = qlearning_model(choice, reward, alpha) #compute the weights
  eval = eval_perseverative(choice, weight, beta, delta) #compute the likelihood
  return(eval)
}

bestmodel_perseverative = function(alpha, beta, delta, choice, reward){
  #function to evaluate the loglikelihood of the model for parameters alpha
  #and beta given the data
  alpha = alpha
  beta = beta
  delta = delta

  weight = qlearning_model(choice, reward, alpha) #compute the weights
  eval = eval_perseverative_both(choice, weight, beta, delta) #compute the likelihood
  eval$weight = weight
  return(eval)
}
