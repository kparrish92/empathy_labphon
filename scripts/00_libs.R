# Load libraries --------------------------------------------------------------

library(here)
library(tidyverse)

# -----------------------------------------------------------------------------


score_lextale <- function(
    n_real = NULL, 
    n_nonse = NULL, 
    n_real_correct = NULL, 
    n_nonse_correct = NULL, 
    n_nonse_incorrect = NULL) {
  
  if (is.null(n_nonse_incorrect)) {
    avg_real <-  (n_real_correct / n_real * 100)
    avg_nonse <- (n_nonse_correct / n_nonse * 100)
    val <- (avg_real + avg_nonse) / 2
  } else {
    val <- n_real_correct - (2 * n_nonse_incorrect)
  }
  return(val)
}
