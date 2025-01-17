#' pIRRitchard_plot
#'
#' Create a histogram of reliability estimates with or without shading boundaries.
#'
#' @param data an output from the main pIRRitchard function. Otherwise, a data.frame where column 1 = criteria/variable name and column 2 = fleiss/AC1 value
#' @param benchmarks TRUE or FALSE. Include shaded boundaries of .2 width.
#'
#' @export

pIRRitchard_plot <- function(data, benchmarks = F){
  mean_x <- round(mean(data[,2]), 3)
  median_x <- round(median(data[,2]), 3)

  if (benchmarks == T){

    plot <- ggplot(data, aes_string(colnames(data)[2]))+
      geom_histogram(fill = "white", color = "black", boundary = 0, binwidth = .05) +
      coord_cartesian(xlim=c(-1,1))+
      scale_x_continuous(breaks=seq(-1, 1, .2))+
      theme_minimal(15) +
      geom_vline(xintercept = mean_x, linetype=2, color = "red")+
      labs(x = paste(gsub("\\..*", "", colnames(data)[2]), "values"),
           y = "Frequency", title = "Reliability Values",
           subtitle = paste("Mean", gsub("\\..*", "", colnames(data)[2]),
                            "= ", mean_x, "and Median", gsub("\\..*", "", colnames(data)[2]), "=", median_x)) +
      annotate(geom = "rect", xmin=0, xmax=.2, ymin=0, ymax=Inf, fill = "darkred", alpha=.1) +
      annotate(geom = "rect", xmin=0.20001, xmax=.4, ymin=0, ymax=Inf, fill = "red", alpha=.1) +
      annotate(geom = "rect", xmin=0.40001, xmax=.6, ymin=0, ymax=Inf, fill = "yellow", alpha=.1) +
      annotate(geom = "rect", xmin=0.60001, xmax=.8, ymin=0, ymax=Inf, fill = "darkgreen", alpha=.1) + annotate(geom = "rect", xmin=0.80001, xmax=1, ymin=0, ymax=Inf, fill = "green", alpha=.1)
  }

  else if (benchmarks == F){
    plot <- ggplot(data, aes_string(colnames(data)[2]))+
      geom_histogram(fill = "white", color = "black", boundary = 0, binwidth = .05) +
      coord_cartesian(xlim=c(-1,1))+
      scale_x_continuous(breaks=seq(-1, 1, .2))+
      theme_minimal(15) +
      geom_vline(xintercept = mean_x, linetype=2, color = "red")+
      labs(x = paste(gsub("\\..*", "", colnames(data)[2]), "values"),
           y = "Frequency", title = "Reliability Values",
           subtitle = paste("Mean", gsub("\\..*", "", colnames(data)[2]),
                            "= ", mean_x, "and Median", gsub("\\..*", "", colnames(data)[2]), "=", median_x))
  }

  return(plot)
}
