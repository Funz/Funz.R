% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/client.R
\name{ParseInput}
\alias{ParseInput}
\title{Convenience method to find variables & related info. in parametrized file.}
\usage{
ParseInput(model, input.files)
}
\arguments{
\item{model}{name of the code wrapper to use. See .Funz.Models global var for a list of possible values.}

\item{input.files}{files to give as input for the code.}
}
\value{
list of variables & their possible default value
}
\description{
Convenience method to find variables & related info. in parametrized file.
}
\examples{
\dontrun{
ParseInput(model = "R",
           input.files = file.path(Funz:::FUNZ_HOME,"samples","branin.R"))
}
}
