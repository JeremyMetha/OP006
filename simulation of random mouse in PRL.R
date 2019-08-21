library(MASS)
reversal600 = NULL
for(i in 1:100000){
  count = 0
  reversals = 0
  trial = runif(600)
  trials = ifelse(trial > 0.5, 1, 0)
  for(j in 1:600){
    if(trials[j]  == 1){
      count = count +1
      if (count == 8){
        reversals = reversals + 1
      }
    } else{
      count = 0
    }

  }
  reversal600 = c(reversal600, reversals)
}
hist(reversal600)
mean(reversal400)
sd(reversal)
hist(reversal)
nb <- fitdistr(reversals, "negative binomial")
pois <- fitdistr(reversal, "poisson")

print(pois)

nb.density <- length(reversal)*dnbinom(size=nb$estimate["size"],mu=nb$estimate["mu"])
reversals600 <- table(reversal600)



reversals600=as.data.frame(reversals00) %>%
  mutate(FreqRatio = Freq/100000)
plot <- ggplot(reversals200, aes(x = reversal200, y = FreqRatio))+
  geom_col()
plot

1-exp(-0.77)-(exp(-0.77)*(1^0.77))
