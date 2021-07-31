grepl_09 <- function(x) {
  .Call("Cgrepl_09", x, PACKAGE = packageName())
}

gsub_09 <- function(x) {
  .Call("Cgsub_09", x, PACKAGE = packageName())
}

uint2dbl <- function(x) {
  .Call("Cuint2dbl", x, PACKAGE = packageName())
}
