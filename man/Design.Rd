% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/client.R
\name{Design}
\alias{Design}
\title{Apply a design of experiments through Funz environment on a response surface.}
\usage{
Design(
  fun,
  design,
  options = NULL,
  input.variables,
  fun.control = list(cache = FALSE, vectorize = "fun", vectorize.by = 1, foreach.options
    = NULL),
  monitor.control = list(results.tmp = TRUE),
  archive.dir = NULL,
  out.filter = NULL,
  verbosity = 1,
  ...
)
}
\arguments{
\item{fun}{response surface as a target (say objective when optimization) function of the DoE. This should include calls to Funz_Run() function.}

\item{design}{Design of Experiments (DoE) given by its name (for instance ""). See .Funz.Designs global var for a list of possible values.}

\item{options}{list of options to pass to the DoE. All options not given are set to their default values. Note that '_' char in names will be replaced by ' '.}

\item{input.variables}{list of variables definition in a String (for instance x1="[-1,1]")}

\item{fun.control}{list of fun usage options:
'cache' set to TRUE if you wish to search in previous evaluations of fun before launching a new experiment. Sometimes useful when design asks for same experiments many times. Always FALSE if fun is not repeatible.
'vectorize' set to "fun" (by default) if fun accepts nrows>1 input. Set to "foreach" if delegating to 'foreach' loop the parallelization of separate 'fun' calls (packages foreach required, and some Do* needs to be registered and started before, and shutdown after). Set to "parallel" if delegating to 'parallel' the parallelization of separate 'fun' calls. Set to FALSE or "apply" means apply() will be used for serial launch of experiments.
'vectorize.by' set the number of parallel execution if fun.control$vectorize is set to "foreach" or "parallel". By default, set to the number of core of your computer (if known by R, otherwise set to 1).
'foreach.options optional parameters to pass to the foreach DoPar. Should include anything needed for 'fun' evaluation.}

\item{monitor.control}{list of control parameters: 'results.tmp' list of design results to display at each batch. TRUE means "all", NULL/FALSE means "none".}

\item{archive.dir}{define an arbitrary output directory where results (log, images) are stored.}

\item{out.filter}{what output(s) to retreive in returned object.}

\item{verbosity}{print (lot of) information while running.}

\item{...}{optional parameters passed to 'fun'}
}
\value{
list of results from this DoE.
}
\description{
Apply a design of experiments through Funz environment on a response surface.
}
\examples{
\dontrun{
# Download on github the GradientDescent algorithm, and install it:
install.Design("GradientDescent")
Design(fun = function(X){abs(X$x1*X$x2)},
  design = "GradientDescent", options = list(max_iterations=10),
  input.variables = list(x1="[0,1]",x2="[1,2]"))
}
}
