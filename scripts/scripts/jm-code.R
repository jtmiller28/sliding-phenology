# Simulating phenology 

# Lognormal distribution: if X ~ Normal(mu, sigsq)
# Y = exp(X) is lognormal with parameters(mu, sigsq)
# Mean of a lognormal is exp()

nsims <- 50000
mu <- 2
siqsq <- 1.5
E.lognorm(e)

library("kdeld")

kde.sims <- kdeld(x = dists.sims)
plot(kde.sims)

quant <- qkdeld(p = 0.80, obj=kde.sims)
prob.quant80 <- pkdeld(q=quant80, obj = kde.sims)
abline(v=quant80)
 
emp.rand.samp <- rkdeld(n=10000m obj=kde.sims)
var(emp.rand.samp)