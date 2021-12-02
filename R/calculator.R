#' Display Funz grid
#'
#' @return Funz grid status
#' @export
#' @examples
#' \dontrun{
#' Grid() # ..Will display no calculator
#' # This will start 5 instances of calculator (waiting for a "Run()" call)
#' calcs = startCalculators(5)
#' Grid() # ...Will now display the 5 calculators started.
#' stopCalcualtors(calcs)
#' Grid() # ...Will now display no calculator.
#' }
Grid <- function(){
    Funz_GridStatus()
}


#' Start calculator instances (also named as "funz daemons")
#'
#' @param n number of calculators to start
#' @param stdout calculators output stream: NULL (default) or "|"
#' @param stderr calculators error stream: NULL (default) or "|"
#' @return processx objects of started calculators
#' @export
#' @import processx
#'
#' @examples
#' \dontrun{
#' # This will start 5 instances of calculator waiting for a "Run()" call
#' startCalculators(5)
#' }
startCalculators <- function(n=1, stdout=NULL, stderr=NULL) {
    p=NULL
    if (Sys.info()[['sysname']]=="Windows")
        for (i in 1:n)
        p = c(p,process$new("cmd.exe", c("/c", "call", gsub("/","\\\\",file.path(FUNZ_HOME,"FunzDaemon.bat"))),
                            wd = FUNZ_HOME, stdout = stdout, stderr = stderr))
    else
        for (i in 1:n)
        p = c(p,process$new("/bin/bash", c("-c", file.path(FUNZ_HOME,"FunzDaemon.sh")),
                            wd = FUNZ_HOME, stdout = stdout, stderr = stderr))
    p
}

#' Shutdown calculator instances (also named as "funz daemons")
#'
#' @param px array of processx objects to stop (returned values of startCalculators())
#' @export
#' @examples
#' \dontrun{
#' calcs = startCalculators(5)
#' # ...
#' stopCalculators(calcs)
#' }
stopCalculators <- function(px) {
    for (p in px)
        p$kill_tree()
}


