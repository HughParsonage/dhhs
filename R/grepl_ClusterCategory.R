#' Find ClusterCategory pattern
#' @description Find a substring in a comma-separated string
#' @param x The character vector to search in.
#' @param table A character vector of terms to search for.
#'
#' @return Equivalent to \code{grepl(table, x, fixed = TRUE)}
#' or \code{grepl(paste0(table, collapse = "|"), x)} for sufficiently
#' fixed patterns in \code{table}.
#'
#' @examples
#' x <- c("Other", "Foo, Other", "Childcare, Education")
#' grepl_ClusterCategory(x, c("Education", "Other"))
#'
#' @export

grepl_ClusterCategory <- function(x, table) {
  .Call("Cgrepl_ClusterCategory", x, table, PACKAGE = packageName())
}
