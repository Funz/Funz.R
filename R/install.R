#' @import rJava
#' @import Rserve
  if (!("rJava" %in% utils::installed.packages()) || !is.function(rJava::.jinit))
      stop("rJava package must be installed.")
#  rJava.version = utils::packageDescription('rJava')$Version
#  if (utils::compareVersion(rJava.version, "1.0-0") < 0)
#      stop(paste("rJava version (",rJava.version,") is too old. Please update to >=1.0"))

  if (!("Rserve" %in% utils::installed.packages()) || !is.function(Rserve::Rserve))
      stop("Rserve package must be installed.")
#  Rserve.version = utils::packageDescription('Rserve')$Version
#  if (utils::compareVersion(Rserve.version, "1.7-5") < 0)
#      stop(paste("Rserve version (",Rserve.version,") is too old. Please update to >=1.7-5 or 1-8"))

.onLoad <- function(libname, pkgname) {
  assign("FUNZ_HOME",system.file("Funz", package = "Funz"), envir = parent.env(environment()))
  source(file.path(FUNZ_HOME,"Funz.R"),local=parent.env(environment()))
}

.onAttach <- function(libname, pkgname) {
  APP_USER=file.path(Sys.getenv("HOME"),".Funz")
  if (!dir.exists(APP_USER)) APP_USER=tempdir()
  Xmx=Sys.getenv("FUNZ_Xmx")
  if (nchar(Xmx)==0) Xmx="512m"
  verb=as.integer(Sys.getenv("FUNZ_verbosity"))
  if (is.na(verb)) verb=0
  Funz.init(FUNZ_HOME,
            verbosity=verb,
            java.control=if (Sys.info()[['sysname']]=="Windows")
                           list(Xmx=Xmx, Xss="256k", app.user=APP_USER, USE_RSERVE_FROM_CRAN="true")
                         else
                           list(Xmx=Xmx,             app.user=APP_USER, USE_RSERVE_FROM_CRAN="true"))

  # That should cleanup remaining Funz processes (incl. Rserve)
  reg.finalizer(.env, function(x){try(x$.jclassFunz$end())}, onexit = TRUE)
}

.github_pattern <- "https://github.com/Funz/__TYPE__-__MODEL__/releases/download/v__MAJOR__-__MINOR__/__TYPE__-__MODEL__.zip"
.github_release <- function(type,model,major,minor) {
  gsub("__TYPE__",type,
       gsub("__MODEL__",model,
            gsub("__MAJOR__",major,
                 gsub("__MINOR__",minor,
                      .github_pattern))))
}

# should blok at package install/lazy load if not connected: .github_repos <- gh::gh("/orgs/Funz/repos",.token=NA, per_page=100)
if (length(.github_repos)==0) .github_repos <- NA

############################ Models #################################

#' Get available Funz Models
#'
#' @return array of available Models in Funz environment.
#' @export
#' @examples
#' installed.Models()
installed.Models <- function() {
  .env$.jclassFunz$getModelList()
}


#' List available models from Funz GitHub repository
#'
#' @param refresh_repo should we force refreshing GitHub Funz repositories content ?
#'
#' @return array of available models
#' @export
#'
#' @examples
#' \dontrun{
#' available.Models()
#' }
available.Models <- function(refresh_repo = F) {
  if (refresh_repo | any(is.na(.github_repos)))
      .env$.github_repos <- gh::gh("/orgs/Funz/repos",.token=NA, per_page=100)

  gsub("plugin-","",
       unlist(lapply(.github_repos,
                     function(r) {if (length(grep("plugin-",r$name))>0) r$name else NULL})),
       fixed=T)
}

#' Install Funz model plugin from local zip file.
#'
#' @param model.zip zip file of plugin. Usually plugin-XYZ.zip
#' @param model model name (parsed in model.zip if not provided)
#' @param force if already installed, reinstall.
#' @param edit.script open installed script for customization.
#' @param ... optional parameters to pass to unzip()
#'
#' @export
install_file.Model <- function(model.zip, model=gsub(".zip(.*)","",gsub("(.*)plugin-","",model.zip)),force=F,edit.script=FALSE,...) {
  if (model %in% installed.Models())
    if (!force) {
      warning("Model ",model," was already installed. Skipping new installation.")
      return()
    } else
      message("Model ",model," was already installed. Forcing new installation..")

  utils::unzip(zipfile=model.zip, exdir=FUNZ_HOME,...)

  eval({
    .env$.jclassFunz$init()
    .env$.Funz.Models <- installed.Models()
    .env$.Funz.Designs <- installed.Designs()
  }) # reload plugins in Funz env
  if (!(model %in% installed.Models()))
    stop("Could not install model ",model , " from ",model.zip)
  else
    message("Installed Funz model ",model)

  # .. in the end, configure model script
  setup.Model(model=model, edit.script=edit.script)
}


#' Configure model calculation entry point
#'
#' @param model Name of model corresponding to given script
#'
#' @export
setup.Model <- function(model, edit.script=FALSE) {
  # Setup script file
  if (Sys.info()[['sysname']]=="Windows")
    script = file.path(FUNZ_HOME,"scripts",paste0(model,".bat"))
  else
    script = file.path(FUNZ_HOME,"scripts",paste0(model,".sh"))

  if (!file.exists(script))
    if (Sys.info()[['sysname']]=="Windows") {
      writeLines(paste0("@echo off\n\nREM Fill this file to launch ",model,"\nREM First argument will be the main file"),
                 file(script))
    } else {
      writeLines(paste0("#!/bin/bash\n\n# Fill this file to launch ",model,"\n# First argument will be the main file"),
                 file(script))
    }

  if (isTRUE(edit.script)) {
    message("The script used to launch ",model," is now opened in the editor.")
    utils::file.edit(script)
  }
  Sys.chmod(script,"0755")

  # Update calculator.xml
  calculator.xml = xml2::as_list(
    xml2::read_xml(
      file.path(FUNZ_HOME,"calculator.xml")))
  found = F
  for (i in 1:length(calculator.xml$CALCULATOR)) {
    node <- calculator.xml$CALCULATOR[[i]]
    if (!is.null(attr(node,"name")))
      if (attr(node,"name") == model) {
        found = T
        attr(node,"command") <- normalizePath(script)
        cplugin = file.path(FUNZ_HOME,"plugins","calc",paste0(model,".cplugin.jar"))
        if (file.exists(cplugin))
          attr(node,"cplugin") <- paste0("file:/",normalizePath(cplugin))
      }
    if (isTRUE(node == "[ comment ]")) {
      node <- NA
    }
    calculator.xml$CALCULATOR[[i]] <- node
  }
  # Add this CODE if not yet found
  if (!found) {
    node = list()
    attr(node,"name") <- model
    attr(node,"command") <- normalizePath(script)
    cplugin = file.path(FUNZ_HOME,"plugins","calc",paste0(model,".cplugin.jar"))
    if (file.exists(cplugin))
      attr(node,"cplugin") <- paste0("file:/",normalizePath(cplugin))
  }
  calculator.xml$CALCULATOR[[i+1]] <- node
  names(calculator.xml$CALCULATOR)[[i+1]] <- "CODE"

  # cleanup NA
  calculator.xml$CALCULATOR <- calculator.xml$CALCULATOR[!is.na(calculator.xml$CALCULATOR)]
  xml2::write_xml(
    xml2::as_xml_document(calculator.xml),
    file.path(FUNZ_HOME,"calculator.xml"),
    options=c("format","no_empty_tags","no_declaration","as_xml"))

}

#' Setup calculators in calculator.xml file.
#'
#' @export
#' @examples
#' \dontrun{
#' setup.Calculator()
#' }
setup.Calculator <- function() {
  message(paste0("The calculator.xml file is now opened in the editor: ",file.path(FUNZ_HOME,"calculator.xml")))
  utils::file.edit(file.path(FUNZ_HOME,"calculator.xml"))
}

#' Install Funz model plugin from central GitHub repository.
#'
#' @param model model to install.
#' @param force if already installed, reinstall.
#' @param edit.script open installed script for customization.
#'
#' @export
#' @examples
#' \dontrun{
#' install_github.Model('Modelica')
#' }
install_github.Model <- function(model,force=F, edit.script=FALSE) {
  major = gsub("-(.*)","",utils::packageDescription("Funz")$Version)
  model.zip = tempfile(paste0("plugin-",model,".zip"))
  for (minor in 10:0) {
    cat(".")
    z <- NULL
    try({z <- suppressWarnings(utils::download.file(.github_release("plugin",model,major,minor),model.zip,quiet = T))},silent=T)
    if (!is.null(z)) break;
  }
  if (is.null(z)) stop("Could not download model ",model)

  install_file.Model(model.zip=model.zip, force=force, edit.script=edit.script)
}

#' Install Funz model from local file or GitHub central repository
#'
#' @param model model to install.
#' @param force if already installed, reinstall.
#' @param edit.script open installed script for customization.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install.Model('Modelica')
#' }
install.Model <- function(model,force=F, edit.script=FALSE) {
  if (file.exists(model))
    install_file.Model(model, force, edit.script)
  else {
    if (model %in% available.Models())
      install_github.Model(model, force, edit.script)
    else
      stop("Model ",model," is not available.")
  }
}


############################ Designs #################################

#' Get available Funz Designs
#'
#' @return array of available Designs in Funz environment.
#' @export
#' @examples
#' installed.Designs()
installed.Designs <- function() {
  Funz:::.env$.jclassFunz$getDesignList()
}

#' List available designs from Funz GitHub repository
#'
#' @param refresh_repo should we force refreshing GitHub Funz repositories content ?
#'
#' @return array of available designs
#' @export
#'
#' @examples
#' \dontrun{
#' available.Designs()
#' }
available.Designs <- function(refresh_repo = F) {
  if (refresh_repo | any(is.na(.github_repos)))
    .env$.github_repos <- gh::gh("/orgs/Funz/repos",.token=NA, per_page=100)

  gsub("algorithm-","",
       unlist(lapply(.github_repos,
                     function(r) {if (length(grep("algorithm-",r$name))>0) r$name else NULL})),
       fixed=T)
}

#' Install Funz design plugin from local zip file.
#'
#' @param design.zip zip file of algorithm. Usually algorithm-XYZ.zip
#' @param design design name (parsed in design.zip if not provided)
#' @param force if already installed, reinstall.
#' @param ... optional parameters to pass to unzip()
#'
#' @export
#' @examples
#' \dontrun{
#' install_file.Design('algorithm-GradientDescent.zip')
#' }
install_file.Design <- function(design.zip, design=gsub(".zip(.*)","",gsub("(.*)algorithm-","",design.zip)),force=F,...) {
  if (design %in% installed.Designs())
    if (!force) {
      warning("Design ",design," was already installed. Skipping new installation.")
      return()
    } else
      message("Design ",design," was already installed. Forcing new installation...")

  utils::unzip(zipfile=design.zip, exdir=FUNZ_HOME,...)

  eval({
    .env$.jclassFunz$init()
    .env$.Funz.Models <- installed.Models()
    .env$.Funz.Designs <- installed.Designs()
  }) # reload plugins in Funz env
  if (!(design %in% installed.Designs()))
    stop("Could not install design ",design , " from ",design.zip)
  else
    message("Installed Funz design ",design)
}


#' Install Funz design plugin from central GitHub repository.
#'
#' @param design design to install.
#' @param force if already installed, reinstall.
#'
#' @export
#' @examples
#' \dontrun{
#' install_github.Design('GradientDescent')
#' }
install_github.Design <- function(design,force=F) {
  major = gsub("-(.*)","",utils::packageDescription("Funz")$Version)
  design.zip = tempfile(paste0("algorithm-",design,".zip"))
  for (minor in 10:0) {
    cat(".")
    z <- NULL
    try({z <- suppressWarnings(utils::download.file(.github_release("algorithm",design,major,minor),design.zip,quiet = T))},silent=T)
    if (!is.null(z)) break;
  }
  if (is.null(z)) stop("Could not download design ",design)

  install_file.Design(design.zip=design.zip, force=force)
}

#' Install Funz design from local file or GitHub central repository
#'
#' @param design design to install.
#' @param force if already installed, reinstall.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install.Design('GradientDescent')
#' }
install.Design <- function(design,force=F) {
  if (file.exists(design))
    install_file.Design(design, force)
  else {
    if (design %in% available.Designs())
      install_github.Design(design, force)
    else
      stop("Design ",design," is not available.")
  }
}
