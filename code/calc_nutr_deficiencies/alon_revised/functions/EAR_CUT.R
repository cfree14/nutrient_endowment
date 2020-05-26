
# Function to....
EAR_CUT <- function(Nut, CV, EAR) {
  def <- c(1:length(CV))
  for (i in 1:length(Nut)){
    def[i] <- pnorm(EAR[i], Nut[i], CV[i])}
  return(def)
}

