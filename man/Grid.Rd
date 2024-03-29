% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculator.R
\name{Grid}
\alias{Grid}
\title{Display Funz grid}
\usage{
Grid()
}
\value{
Funz grid status
}
\description{
Display Funz grid
}
\examples{
\dontrun{
Grid() # ..Will display no calculator
# This will start 5 instances of calculator (waiting for a "Run()" call)
calcs = startCalculators(5)
Grid() # ...Will now display the 5 calculators started.
stopCalcualtors(calcs)
Grid() # ...Will now display no calculator.
}
}
