
#' @description (From package hutils) Checks an input is TRUE or FALSE,
#' signalling an error with a descriptive error message otherwise.
#' @name check_TF
#' @author Hugh Parsonage
#' @param x An unhandled input that should be TRUE or FALSE only.
#' @noRd
check_TF <- function (x) {
  if (is.logical(x) && length(x) == 1L) {
    if (anyNA(x)) {
      xc <- deparse(substitute(x))
      stop("`", xc, " = NA` but must be TRUE or FALSE. ",
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      return(NULL)
    }
  } else {
    xc <- deparse(substitute(x))
    if (length(x) != 1L) {
      stop("`", xc, "` had length ", length(x),
           " but must be length-one. ", "Change `",
           xc, "` to be TRUE or FALSE.")
    }
    else {
      stop("`", xc, "` was type ", typeof(x),
           " but must be logical. ", "Change `",
           xc, "` to be TRUE or FALSE.")
    }
  }
}
