

# ptarget <- 0.05; ear <- 10; intake_cv=0.25
calc_nutr_demand <- function(ptarget, ear, intake_cv=0.25, plot=F){
  
  # Calculate difference between p(deficient) target
  # and the p(deficient) produced by an evaluated mean intake
  minDiff <- function(intake_mu){
    intake_sd <- intake_mu * intake_cv
    pdeficient <- pnorm(q=ear, mean=intake_mu, sd=intake_sd)
    pdeficient_diff <- abs(ptarget - pdeficient)
    return(pdeficient_diff)
  }
  
  # Perform calculation
  fit <- optimize(f=minDiff, lower=0, upper=ear*4)
  intake_req <- fit$minimum
  
  # Plot
  if(plot==T){
    
    # Simulate data
    n <- 3000
    req_avg <- ear
    req_cv <- 0.10
    req_sd <- req_avg * req_cv
    intake_avg <- intake_req 
    intake_sd <- intake_req * intake_cv
    reqs <- rnorm(n=n, mean=req_avg, sd=req_sd)
    intakes <- rnorm(n=n, mean=intake_avg, sd=intake_sd)
    df <- tibble(req=reqs, intake=intakes)
    
    # Calculate proportions deficient
    pdeficient_est <- pnorm(q=req_avg, mean=intake_req, sd=intake_sd)
    pdeficient_obs <- sum(intakes<req_avg) / n
    
    # Create labels
    pdeficient_label <-  paste0(format(round(pdeficient_est*100,1), nsmall=1), "% nutrient deficient")
    
    # Plot simulated data
    ymax <- max(intakes)
    g <- ggplot(df, aes(x=intakes, y=reqs)) +
      geom_point(col="grey40", alpha=0.4) +
      labs(x="Usual intakes", y="Requirements") +
      geom_vline(xintercept = req_avg, linetype="dotted") +
      geom_hline(yintercept = req_avg, linetype="dotted") +
      geom_abline(slope=1, intercept=0) +
      annotate("text", label=pdeficient_label, x=ymax, y=0, hjust=1) +
      lims(x=c(0, ymax), y=c(0, ymax)) +
      theme_bw()
  print(g)
    
  }
  
  # Return
  return(intake_req)
  
}

