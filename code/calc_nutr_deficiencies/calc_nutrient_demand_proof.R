# Simulate some data

# Simulation
n <- 3000
req_avg <- 1200
req_sd <- 180
intake_avg <- 1600
intake_sd <- 450
reqs <- rnorm(n=n, mean=req_avg, sd=req_sd)
intakes <- rnorm(n=n, mean=intake_avg, sd=intake_sd)
df <- tibble(req=reqs, intake=intakes)

# Plot
g <- ggplot(df, aes(x=intakes, y=reqs)) +
  geom_point(col="grey40") +
  labs(x="Usual intakes", y="Requirements") +
  geom_vline(xintercept = req_avg) +
  theme_bw()
g

pdeficient_est <- pnorm(q=req_avg, mean=intake_avg, sd=intake_sd)
pdeficient_obs <- sum(intakes<req_avg) / n

# Now try to solve the intake average that results in a threshhold

minDiff <- function(par){
  supply_mu <- par[1]
  supply_sd <- intake_sd
  pdeficient <- pnorm(q=req_avg, mean=supply_mu, sd=supply_sd)
  pdeficient_target <- 0.05
  pdeficient_diff <- abs(pdeficient_target - pdeficient)
  return(pdeficient_diff)
}

fit <- optimize(f=minDiff, lower=0, upper=intake_avg*2)
intake_avg_req <- fit$minimum

# 

# Simulation
n <- 3000
req_avg <- 1200
req_sd <- 180
intake_avg <- intake_avg_req
intake_sd <- 450
reqs <- rnorm(n=n, mean=req_avg, sd=req_sd)
intakes <- rnorm(n=n, mean=intake_avg, sd=intake_sd)
df <- tibble(req=reqs, intake=intakes)

# Plot
g <- ggplot(df, aes(x=intakes, y=reqs)) +
  geom_point(col="grey40") +
  labs(x="Usual intakes", y="Requirements") +
  geom_vline(xintercept = req_avg) +
  theme_bw()
g

pdeficient_est <- pnorm(q=req_avg, mean=intake_avg, sd=intake_sd)
pdeficient_obs <- sum(intakes<req_avg) / n

