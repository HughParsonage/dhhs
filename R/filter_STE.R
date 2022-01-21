#' Filter by encoded state
#'
#' @param x Either an atomic vector or a \code{data.table} with column \code{State}.
#' @param y The table to lookup.
#'
#' @return Either a logical vector or the filtered \code{data.table} the elements
#' of \code{x} in \code{y}.
#'
#'
#' @export

filter_ste_in <- function(x, y) {
  do_filter_ste_in(x, y, FALSE)
}

filter_ste_notin <- function(x, y) {
  do_filter_ste_in(x, y, TRUE)
}

do_filter_ste_in <- function(x, y, .opposite = FALSE) {
  if (!is.raw(y)) {
    y <- encode_State(y, oraw = TRUE)
  }
  stopifnot(is.atomic(y),
            is.raw(y))
  if (is.data.table(x)) {
    v <- .subset2(x, "State")
    if (is.null(v)) {
      stop("`x` was a data.table but lacked a column 'State' so `filter_ste_in` could not be invoked.")
    }
    yes <- filter_ste_in(v, y)
    return(x[(yes)])
  }
  .Call("CFilter_STE_in", x, y, FALSE, PACKAGE = packageName())
}

