#' pIRRitchard_data
#'
#' Create a sample data.frame in wide form to test the function.
#'
#' @param criteria number of criteria/variables to simulate
#' @param raters number of raters to simulate
#' @param units number os unit to simulate
#'
#' @return A unit X critera*raters data.frame
#' @export
piRRitchard_data <- function(criteria, raters, units){
  dat <- data.frame(id = 1:units)
  vars <- replicate(criteria*raters, round(runif(units)))
  crit <- paste0("Criteria", 1:criteria)
  rate <- paste0("Rater", 1:raters)
  colnames(vars) <- apply(expand.grid(crit, rate), 1, paste, collapse=".")
  library(tidyverse)
  dat <- cbind(dat, vars)
  dat <- dat %>%
    mutate_all(as.factor)
  return(dat)
}

