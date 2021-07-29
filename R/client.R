#' Call an external code wrapped through Funz.
#'
#' @param model name of the code wrapper to use. See .Funz.Models global var for a list of possible values.
#' @param input.files list of files to give as input for the code.
#' @param input.variables data.frame of input variable values. If more than one experiment (i.e. nrow >1), experiments will be launched simultaneously on the Funz grid.
#' @param all.combinations if FALSE, input.variables variables are grouped (default), else, if TRUE experiments are an expaanded grid of input.variables
#' @param output.expressions list of interest output from the code. Will become the names() of return list.
#' @param run.control list of control parameters:
#'   'force.retry' is number of retries before failure,
#'   'cache.dir' setup array of directories to search inside before real launching calculations.
#' @param monitor.control list of monitor parameters: sleep (delay time between two checks of results), display.fun (function to display project cases status).
#' @param archive.dir define an arbitrary output directory where results (cases, csv files) are stored.
#' @param verbosity 0-10, print information while running.
#'
#' @return list of array results from the code, arrays size being equal to input.variables arrays size.
#' @export
#' @examples
#' \dontrun{
#' # Basic response surface of Branin function. R is used as the model, for testing purpose.
#' calcs = startCalculators(5) # Will start calculator instances later used by Run()
#' Run(model = "R",
#'   input.files = file.path(Funz:::FUNZ_HOME,"samples","branin.R"),
#'   input.variables = list(x1=seq(0,1,by=0.1),x2=seq(0,1,by=0.1)),
#'   all.combinations=TRUE,
#'   output.expressions = "z")
#' persp(z=matrix(unlist(.Funz.Last.run$results$z),nrow=11),zlab="z")
#' stopCalculators(calcs) # Shutdown calculators (otherwise stay in background)
#'
#' # More realistic case using Modelica. Assumes that (Open)Modelica is already installed.
#' install.Model("Modelica") # Install Modelica plugin
#' calcs = startCalculators(5) # Will start calculator instances later used by Run()
#' NewtonCooling = Run(model = "Modelica",
#'   input.files = file.path(Funz:::FUNZ_HOME,"samples","NewtonCooling.mo.par"),
#'   input.variables = list(convection=seq(0.001,1,by=0.05)),
#'   output.expressions = "min(T)")
#' plot(NewtonCooling$convection, NewtonCooling$`min(T)`)
#' stopCalculators(calcs) # Shutdown calculators (otherwise stay in background)
#' }
Run <- function(model=NULL,input.files,
                input.variables=NULL,all.combinations=FALSE,
                output.expressions=NULL,
                run.control=list(force.retry=2,cache.dir=NULL),
                monitor.control=list(sleep=5,display.fun=NULL),
                archive.dir=NULL,verbosity=1) {
    return(Funz_Run(model=model,input.files=input.files,
             input.variables=input.variables,all.combinations=all.combinations,output.expressions=output.expressions,
             run.control=run.control,monitor.control=monitor.control,
             archive.dir=archive.dir,verbosity=verbosity,log.file=F))
}


#' Get last Funz Run(...) call
#'
#' @return last Funz Run(...) call
#' @export
.Last.run <- function() {
    return(.Funz.Last.run)
}

#' Apply a design of experiments through Funz environment on a response surface.
#'
#' @param design Design of Experiments (DoE) given by its name (for instance ""). See .Funz.Designs global var for a list of possible values.
#' @param input.variables list of variables definition in a String (for instance x1="[-1,1]")
#' @param options list of options to pass to the DoE. All options not given are set to their default values. Note that '_' char in names will be replaced by ' '.
#' @param fun response surface as a target (say objective when optimization) function of the DoE. This should include calls to Funz_Run() function.
#' @param fun.control list of fun usage options:
#'   'cache' set to TRUE if you wish to search in previous evaluations of fun before launching a new experiment. Sometimes useful when design asks for same experiments many times. Always FALSE if fun is not repeatible.
#'   'vectorize' set to "fun" (by default) if fun accepts nrows>1 input. Set to "foreach" if delegating to 'foreach' loop the parallelization of separate 'fun' calls (packages foreach required, and some Do* needs to be registered and started before, and shutdown after). Set to "parallel" if delegating to 'parallel' the parallelization of separate 'fun' calls. Set to FALSE or "apply" means apply() will be used for serial launch of experiments.
#'   'vectorize.by' set the number of parallel execution if fun.control$vectorize is set to "foreach" or "parallel". By default, set to the number of core of your computer (if known by R, otherwise set to 1).
#'   'foreach.options optional parameters to pass to the foreach DoPar. Should include anything needed for 'fun' evaluation.
#' @param monitor.control list of control parameters: 'results.tmp' list of design results to display at each batch. TRUE means "all", NULL/FALSE means "none".
#' @param archive.dir define an arbitrary output directory where results (log, images) are stored.
#' @param verbosity print (lot of) information while running.
#' @param ... optional parameters passed to 'fun'
#'
#' @return list of results from this DoE.
#' @export
#' @examples
#' \dontrun{
#' # Download on github the GradientDescent algorithm, and install it:
#' install.Design("GradientDescent")
#' Design(fun = function(X){abs(X$x1*X$x2)},
#'   design = "GradientDescent", options = list(max_iterations=10),
#'   input.variables = list(x1="[0,1]",x2="[1,2]"))
#' }
Design <- function(fun, design, options=NULL,
                   input.variables,
                   fun.control=list(cache=FALSE,vectorize="fun",vectorize.by=1,foreach.options=NULL),
                   monitor.control=list(results.tmp=TRUE),
                   archive.dir=NULL,verbosity=1,...) {
    return(Funz_Design(fun=fun,design=design,options=options,
                input.variables=input.variables,
                fun.control=fun.control,monitor.control=monitor.control,
                archive.dir=archive.dir,verbosity=verbosity,log.file=FALSE,...))
}

#' Get last Funz Design(...) call
#'
#' @return last Funz Design(...) call
#' @export
.Last.design <- function() {
    return(.Funz.Last.design)
}

#' Call an external (to R) code wrapped through Funz environment.
#'
#' @param model name of the code wrapper to use. See .Funz.Models global var for a list of possible values.
#' @param input.files list of files to give as input for the code.
#' @param input.variables list of variables definition in a String (for instance x1="[-1,1]"), or array of fixed values (will launch a design for each combination).#' @param all.combinations if FALSE, input.variables variables are grouped (default), else, if TRUE experiments are an expaanded grid of input.variables
#' @param output.expressions list of interest output from the code. Will become the names() of return list.
#' @param design Design of Experiments (DoE) given by its name (for instance ""). See .Funz.Designs global var for a list of possible values.
#' @param design.options list of options to pass to the DoE. All options not given are set to their default values. Note that '_' char in names will be replaced by ' '.
#' @param run.control list of control parameters:
#'   'force.retry' is number of retries before failure,
#'   'cache.dir' setup array of directories to search inside before real launching calculations.
#' @param monitor.control list of monitor parameters: sleep (delay time between two checks of results), display.fun (function to display project cases status).
#' @param archive.dir define an arbitrary output directory where results (cases, csv files) are stored.
#' @param verbosity 0-10, print information while running.
#'
#' @return list of array design and results from the code.
#' @export
#' @examples
#' \dontrun{
#' # Search for minimum of Branin function, taken as the model (test case)
#' install.Design("GradientDescent") # Download on github the GradientDescent algorithm
#' startCalculators(5) # start calculator instances to run model
#' RunDesign(model="R",
#'           input.files=file.path(Funz:::FUNZ_HOME,"samples","branin.R"),
#'           output.expressions="cat", design = "GradientDescent",
#'           design.options = list(max_iterations=10),input.variables = list(x1="[0,1]",x2="[0,1]"))
#'
#' # More realistic case using inversion of Modelica:
#' #  find convection coefficient that gives minimal temperature of 40 degrees.
#' install.Model("Modelica") # Install Modelica plugin (Assumes that Modelica is already installed)
#' install.Design("Brent")   # Install Brent algorithm for inversion
#' calcs = startCalculators(5) # Will start calculator instances, later used by Run()
#' NewtonCooling = RunDesign(model = "Modelica",
#'   input.files = file.path(Funz:::FUNZ_HOME,"samples","NewtonCooling.mo.par"),
#'   input.variables = list(convection="[0.0001,1]"), output.expressions = "min(T)",
#'   design="Brent",design.options=list(ytarget=40.0))
#' plot(NewtonCooling$convection[[1]], NewtonCooling$`min(T)`[[1]])
#' abline(h=40.0)
#' abline(v=NewtonCooling$analysis.root)
#' stopCalculators(calcs) # Shutdown calculators (otherwise stay in background)
#' }
RunDesign <- function(model=NULL,input.files,
                      input.variables=NULL,output.expressions=NULL,
                      design=NULL,design.options=NULL,
                      run.control=list(force.retry=2,cache.dir=NULL),
                      monitor.control=list(results.tmp=TRUE,sleep=5,display.fun=NULL),
                      archive.dir=NULL,verbosity=1) {
    return(Funz_RunDesign(model=model,input.files=input.files,output.expressions=output.expressions,
                   design=design,input.variables=input.variables,design.options=design.options,
                               run.control=run.control,monitor.control=monitor.control,
                               archive.dir=archive.dir,verbosity=verbosity,log.file=FALSE))
}

#' Get last Funz RunDesign(...) call
#'
#' @return last Funz RunDesign(...) call
#' @export
.Last.rundesign <- function() {
    return(.Funz.Last.rundesign)
}

#' Convenience method to find variables & related info. in parametrized file.
#'
#' @param model name of the code wrapper to use. See .Funz.Models global var for a list of possible values.
#' @param input.files files to give as input for the code.
#'
#' @return list of variables & their possible default value
#' @export
#' @examples
#' ParseInput(model = "R",
#'            input.files = file.path(Funz:::FUNZ_HOME,"samples","branin.R"))
ParseInput <- function(model,input.files) {
    return(Funz_ParseInput(model=model,input.files=input.files))
}

#' Convenience method to compile variables in parametrized file.
#'
#' @param model name of the code wrapper to use. See .Funz.Models global var for a list of possible values.
#' @param input.files files to give as input for the code.
#' @param input.values list of variable values to compile.
#' @param output.dir directory where to put compiled files.
#'
#' @export
#' @examples
#' CompileInput(model = "R",
#'              input.files = file.path(Funz:::FUNZ_HOME,"samples","branin.R"),
#'              input.values = list(x1=0.5, x2=0.6))
#' CompileInput(model = "R",
#'              input.files = file.path(Funz:::FUNZ_HOME,"samples","branin.R"),
#'              input.values = list(x1=c(0.5,.55), b=c(0.6,.7)))
CompileInput <- function(model,input.files,input.values,output.dir=".") {
    return(Funz_CompileInput(model=model,input.files=input.files,input.values=input.values,output.dir=output.dir))
}

#' Convenience method to find variables & related info. in parametrized file.
#'
#' @param model name of the code wrapper to use. See .Funz.Models global var for a list of possible values.
#' @param input.files files given as input for the code.
#' @param output.dir directory where calculated files are.
#'
#' @return list of outputs & their value
#' @export
#' @examples
#' \dontrun{
#' ReadOutput(model = "R", input.files = "branin.R",output.dir=".")
#' }
ReadOutput <- function(model, input.files, output.dir) {
    return(Funz_ReadOutput(model=model, input.files=input.files, output.dir=output.dir))
}
