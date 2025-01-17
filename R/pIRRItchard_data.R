#' pIRRitchard_data
#'
#' Create a sample data.frame in wide form to test the function.
#'
#' @param criteria number of criteria/variables to simulate
#' @param raters number of raters to simulate
#' @param units number of unit to simulate
#'
#' @return A unit X raters*criteria data.frame
#' @export
pIRRitchard_data <- function(criteria, raters, units){
  dat <- data.frame(id = 1:units)
  vars <- replicate(raters*criteria, round(runif(units)))
  crit <- paste0("Criteria", 1:criteria)
  rate <- paste0("Rater", 1:raters)
  colnames(vars) <- apply(expand.grid(rate, crit), 1, paste, collapse=".")
  library(tidyverse)
  dat <- cbind(dat, vars)
  dat <- dat %>%
    mutate_all(as.factor)
  return(dat)
}

