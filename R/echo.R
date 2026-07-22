#' Echo Input Back
#'
#' Returns the input unchanged. Useful for verifying that the PexaCloud
#' connection is live, that JSON deserialisation is working correctly, and
#' that the correct data types and structure arrive at the R function.
#'
#' @param model_input Any R object (list, data frame, scalar, etc.).
#' @return The input as received, wrapped in a one-row data frame so OpenCPU
#'   can serialise it. Columns: \code{class}, \code{length}, \code{as_json}.
#' @export
echo <- function(model_input = NULL) {
  data.frame(
    class   = paste(class(model_input), collapse = ", "),
    length  = length(model_input),
    as_json = jsonlite::toJSON(model_input, auto_unbox = TRUE, force = TRUE),
    stringsAsFactors = FALSE
  )
}
