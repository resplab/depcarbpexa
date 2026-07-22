#' Get Default Input for the Depression Carbon Footprint Model
#'
#' Returns a named list of default parameter values that can be passed
#' directly to \code{\link{model_run}()}.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{scc}{Social cost of carbon (USD/tonne CO2e). Default: 90.}
#'   \item{dr}{Annual discount rate. Default: 0.025.}
#'   \item{dru_duration}{Drug treatment duration (quarters). Default: 8.}
#'   \item{dru_duration_com}{Combined therapy drug duration (quarters).
#'     Default: 8 (same as \code{dru_duration}).}
#'   \item{cycles}{Model time horizon in quarters. Default: 20.}
#'   \item{seed}{Random seed. Default: 1.}
#' }
#' @export
get_default_input <- function() {
  list(
    scc             = 90,
    dr              = 0.025,
    dru_duration    = 8,
    dru_duration_com = 8,
    cycles          = 20L,
    seed            = 1L
  )
}
