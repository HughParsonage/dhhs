

raw_brack <- function(x, i) {
  # x[i]
  .Call("C_rawBrack", x, i, PACKAGE = packageName())
}
