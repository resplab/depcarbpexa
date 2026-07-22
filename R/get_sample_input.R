#' Get Sample Input for the Depression Carbon Footprint Model
#'
#' Returns a data frame of example model scenarios that can be passed
#' directly to \code{\link{model_run}()}.
#'
#' @param n Integer. Number of sample scenarios to return (1–5). Default: 3.
#' @return A data frame with \code{n} rows covering varied parameter choices.
#' @export
get_sample_input <- function(n = 3) {
  n <- min(max(as.integer(n), 1L), 5L)

  sample_data <- data.frame(
    # Scenario 1: base-case (matches get_default_input)
    # Scenario 2: higher social cost of carbon (policy-relevant sensitivity)
    # Scenario 3: shorter drug duration, higher discount rate
    # Scenario 4: longer time horizon, low SCC
    # Scenario 5: high SCC, shorter drug duration in combined arm
    scc              = c(90,  190,  90,   50,  300),
    dr               = c(0.025, 0.025, 0.05, 0.025, 0.025),
    dru_duration     = c(8,    8,    4,   12,    8),
    dru_duration_com = c(8,    8,    4,   12,    4),
    cycles           = c(20L, 20L,  20L,  40L,  20L),
    seed             = c(1L,   2L,   3L,   4L,   5L),
    stringsAsFactors = FALSE
  )

  sample_data[seq_len(n), ]
}
