#' Get Package Version
#'
#' Returns the installed version of \pkg{depcarbpexa}. Useful for confirming
#' which version is deployed on the PexaCloud server.
#'
#' @return A named list with elements \code{package} and \code{version}.
#' @export
version <- function() {
  list(
    package = "depcarbpexa",
    version = as.character(utils::packageVersion("depcarbpexa"))
  )
}
