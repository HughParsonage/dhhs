#' Fast nchar that can be used on integers and strings
#' @param x Either an integer or character vector.
#'
#' @return Same as \code{nchar(x)}.
#'
#' \code{max_nchar(x)} returns the \code{max(nchar(x))}
#' with the exception that length-zero inputs return zero, not \code{-Inf},
#' since negative-length strings are not possible.
#'
#' \code{const_nchar} returns the width of every string in \code{x} if and only
#' if every element's width is the same. Otherwise, \code{-1}.
#'
#' @export

fast_nchar <- function(x) {
  ans <- .Call("Cfast_nchar", x, PACKAGE = packageName())
  if (is.null(ans)) {
    return(nchar(ans, keepNA = FALSE))
  }
  ans
}

#' @rdname fast_nchar
#' @export
max_nchar <- function(x) {
  .Call("Cmax_nchar", x, PACKAGE = packageName())
}

#' @rdname fast_nchar
#' @export
const_nchar <- function(x) {
  .Call("C_const_nchar", x, PACKAGE = packageName())
}

