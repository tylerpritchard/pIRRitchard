\name{pIRRitchard_plot}
\alias{pIRRitchard_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quick histogram of reliability estimates.
%%  ~~function to do ... ~~
}
\description{Uses ggplot to quickly plot your reliability estimates.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pIRRitchard_plot(data, benchmarks = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Calls a data.frame that is created from the main pIRRitchard() function. Can also be any n x 2 data.frame wherein column 1 is criteria/variable and column 2 is the reliability estimate.
%%     ~~Describe \code{data} here~~
}
  \item{benchmarks}{TRUE or FALSE. Plot a poor gradient outlining common benchmarks for Fleiss's Kappa
%%     ~~Describe \code{benchmarks} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Tyler R. Pritchard
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
