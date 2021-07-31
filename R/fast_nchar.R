#' Fast nchar that can be used on integers and strings
#' @param x Either an integer or character vector.
#'
#' @return Same as \code{nchar(x)}
#' @export

fast_nchar <- function(x) {
  ans <- .Call("Cfast_nchar", x, PACKAGE = packageName())
  if (is.null(ans)) {
    return(nchar(ans, keepNA = FALSE))
  }
  ans
}
