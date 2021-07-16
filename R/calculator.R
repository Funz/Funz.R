#' Display Funz grid
#'
#' @return Funz grid status
#' @export
Grid <- function(){
    Funz_GridStatus()
}


#' Start calculator instances (also named as "funz daemons")
#'
#' @param n number of calculators to start
#' @return processx objects of started calculators
#' @export
#' @import processx
#'
#' @examples
#' \dontrun{
#' # This will start 5 instances of calculator waiting for a "Run()" call
#' startCalculators(5)
#' }
startCalculators <- function(n=1) {
    p=NULL
    if (Sys.info()[['sysname']]=="Windows")
        for (i in 1:n)
        p = c(p,process$new(file.path(system.file("Funz",package = "Funz"),"FunzDaemon.bat"),
                stdout = NULL, stderr = NULL))
    else
        for (i in 1:n)
        p = c(p,process$new(file.path(system.file("Funz",package = "Funz"),"FunzDaemon.sh"),
                stdout = NULL, stderr = NULL))
    p
}

#' Shutdown calculator instances (also named as "funz daemons")
#'
#' @param px array of processx objects to stop (returned values of startCalculators()^)
#' @export
stopCalculators <- function(px) {
    for (p in px)
        p$kill_tree()
}


