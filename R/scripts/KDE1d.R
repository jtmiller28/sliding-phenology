# Simulating phenology distributions:

# Lognormal distribution:  if X ~ Normal(mu, sigsq), then 
# Y = exp(X) is lognormal with parameters (mu, sigsq)

# Mean of a log-normal is exp(mu+ sigsq/2), where mu, sigsq are the 
# parameters of a normal distribution

nsims <- 50000
mu<- 2
sigsq <- .15
E.lognorm <- exp(mu+sigsq/2)
V.lognorm <- (exp(sigsq)-1)*exp(2*mu+sigsq)
dists.sims <- rlnorm(n=50000, meanlog=mu, sdlog=sqrt(sigsq))

mean(dists.sims)
var(dists.sims)

#install.packages("kde1d")
library("kde1d")

kde.sims <- kde1d(x=dists.sims)
plot(kde.sims)

quant80 <- qkde1d(p=0.80, obj=kde.sims)
prob.quant80 <- pkde1d(q=quant80, obj=kde.sims)

abline(v=quant80)



