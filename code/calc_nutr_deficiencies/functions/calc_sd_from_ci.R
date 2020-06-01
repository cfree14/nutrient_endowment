
calc_sd_from_si <- function(mu, lci, uci){
  
  # If supply is normally distributed...
  # And CIs represent 95% confidence intervals (0.025 and 0.975)...
  # Then they are each 1.96 standard deviations from the mean
  sd1 <- abs(mu-lci) / 1.96
  sd2 <- abs(mu-uci) / 1.96
  sd_derived <- mean(c(sd1, sd2))
  
  # Plot distribution to verify performance of method
  if(F){
    xmin <- lci - lci * 0.1
    xmax <- uci + uci * 0.1
    xs <- seq(xmin, xmax, length.out=300)
    ys <- dnorm(xs, mean=mu, sd=sd_derived)
    lci_derived <- mu - 1.96 * sd_derived
    uci_derived <- mu + 1.96 * sd_derived
    plot(ys ~ xs, type="l", xlab="Supply")
    abline(v=mu, lty=1)
    abline(v=c(uci, lci), lty=2)
    abline(v=c(uci_derived, lci_derived), lty=2, col="red")
  }
  
  # Return
  return(sd_derived)
  
}


