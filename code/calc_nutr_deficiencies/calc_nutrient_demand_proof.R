
# Packages
library(tidyverse)

# Directories
plotdir <- "code/calc_nutr_deficiencies/figures"

# Simulate data
################################################################################

# Simulate intake/requirement joint distribution
n <- 3000
req_avg <- 1200
req_sd <- 180
intake_avg <- 1600
intake_sd <- 450
reqs <- rnorm(n=n, mean=req_avg, sd=req_sd)
intakes <- rnorm(n=n, mean=intake_avg, sd=intake_sd)
df <- tibble(req=reqs, intake=intakes)

# Plot intake/requirement joint distribution
g <- ggplot(df, aes(x=intakes, y=reqs)) +
  geom_point(col="grey40") +
  labs(x="Usual intakes", y="Requirements") +
  geom_vline(xintercept = req_avg) +
  theme_bw()
g

# Calculate nutrient deficicieny using EAR and observations
pdeficient_est <- pnorm(q=req_avg, mean=intake_avg, sd=intake_sd)
pdeficient_obs <- sum(intakes<req_avg) / n


# Solve for the required mean uptake
################################################################################

# Function to calculate difference between target and candidate deficiency rates
minDiff <- function(par){
  supply_mu <- par[1]
  supply_sd <- intake_sd
  pdeficient <- pnorm(q=req_avg, mean=supply_mu, sd=supply_sd)
  pdeficient_target <- 0.05
  pdeficient_diff <- abs(pdeficient_target - pdeficient)
  return(pdeficient_diff)
}

# Calculate required mean uptake rate
fit <- optimize(f=minDiff, lower=0, upper=intake_avg*2)
intake_avg_req <- fit$minimum


# Simulate with new mean uptake to confirm performance
################################################################################

# Simulation
n <- 3000
req_avg <- 1200
req_sd <- 180
intake_avg2 <- intake_avg_req
intake_sd <- 450
reqs2 <- rnorm(n=n, mean=req_avg, sd=req_sd)
intakes2 <- rnorm(n=n, mean=intake_avg2, sd=intake_sd)
df2 <- tibble(req=reqs2, intake=intakes2)

# Plot intake/requirement joint distribution
g <- ggplot(df2, aes(x=intakes, y=reqs)) +
  geom_point(col="grey40") +
  labs(x="Usual intakes", y="Requirements") +
  geom_vline(xintercept = req_avg) +
  theme_bw()
g

# Calculate nutrient deficicieny using EAR and observations
pdeficient_est <- pnorm(q=req_avg, mean=intake_avg2, sd=intake_sd)
pdeficient_obs <- sum(intakes2<req_avg) / n


# Nice plot
################################################################################

# Build data
data <- bind_rows(df %>% mutate(type="Current intake"),
                  df2 %>% mutate(type="Target intake"))


# Plot data
g <- ggplot(data, aes(x=intake, y=req, color=type)) +
  geom_point(alpha=0.4) +
  # Add reference lines
  geom_vline(xintercept = req_avg, linetype="dotted") +
  geom_hline(yintercept = req_avg, linetype="dotted") +
  geom_abline(slope=1) +
  # Add mean lines
  geom_vline(xintercept = intake_avg, color="black") +
  geom_vline(xintercept = intake_avg2, color="darkred") +
  # Axis
  lims(x=c(0, max(data$intake)), y=c(0, max(data$intake))) +
  # Labels
  labs(x="Usual intakes", y="Requirements") +
  # Legend
  scale_color_manual(name="", values=c("grey60", "red")) +
  theme_bw() +
  theme(legend.position="bottom")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "usual_intake_requirement.png"), 
       width=4.5, height=4.5, units="in", dpi=600)


