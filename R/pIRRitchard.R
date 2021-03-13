## Our function
pIRRitchard <- function(data, n_raters, type){#need three inputs
  library(irrCAC)
  library(tidyverse)
  type <- type
  ### Easy way to create sequences so reliability functions call the correct rows
  sequences <-data.frame(first = seq(from = 2,
                                     to = (ncol(data)-(n_raters-1)),
                                     by= n_raters),
                         last = seq(from = 2+(n_raters-1),
                                    to = ncol(data),
                                    by = n_raters))

  #### Allowing type to select appropriate estimate
  func <<- ifelse(type == "ac1", gwet.ac1.raw, fleiss.kappa.raw)

  ### nested function to bring out the value of the estimate...may add CI and pvalues
  get_kappa_gwet <- function(first, last){
    values <-  func(data[,first:last])$est[4]
    return(values)
  }

  #### outputting the sequences estimate as a data frame
  output <- data.frame(pmap(sequences, get_kappa_gwet)) %>%
    t() %>%
    data.frame()


  ### adjusting variable names

  names(output)[1] <- ifelse(type == "ac1", "ac1", "fleiss")
  output[,1] <- ifelse(output[,1] < 0, 0, output[,1])

  output2 <- rownames_to_column(output, var = "variable")
  return(output2)
}
