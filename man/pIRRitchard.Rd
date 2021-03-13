\name{pIRRitchard}
\alias{pIRRitchard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Easily calculate IRR for multiple criteria and raters.
%%  ~~function to do ... ~~
}
\description{This function is a work in progress. It seeks to quickly calculate and summarize two major reliability estimates: Gwet's AC1 and Fleiss's Kappa.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pIRRitchard(data, n_raters, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data.frame in wide form. Column 1 = ID of units. Column 2 through N represents the variable x and rater y progression. Cx1y1, Cx1y2, Cx2y1, Cx2y2... Cxnyn (see details).
%%     ~~Describe \code{data} here~~
}
  \item{n_raters}{Number of raters. Used to sequence the columns used in reliability estimates
%%     ~~Describe \code{n_raters} here~~
}
  \item{type}{"fleiss" or "ac1"
%%     ~~Describe \code{type} here~~
}
}
\details{This file serves wide data frames wherein the columns are linked to a specific rater and criteria/outcome of interest. Column 1 should be a unique ID for the units being rated. Columns 2 through n should progress such that each rater follows one another for variable/outcome 1, follow by variable/outcome 2. E.g., for three raters on two variables/outcomes: Colx1y1, Colx1y2, Colx2y3, Colx2y1, Colx2y2, Colx2y3, Colx3y1, Colx3y2, Colx3y3. Rows represent each unique unit being rated.
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
\author{ Tyler R. Pritchard
%%  ~~who you are~~
}
\note{Dependencies: irrCAC, Tidyverse
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dat <- data.frame(id = 1:100)
vars <- replicate(12, round(runif(400)))
criteria <- c("C1", "C2", "C3", "C4")
raters <- c("R1", "R2", "R3")
colnames(vars) <- paste(rep(criteria, each = length(raters)), raters, sep = ".")
library(tidyverse)
dat <- cbind(dat, vars)
dat <- dat %>%
  mutate_all(as.factor)

reliability_estimates <- pIRRitchard(data = dat, n_raters = 3, type = "fleiss")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
