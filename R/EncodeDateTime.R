

EncodeDateTime <- function(x, y) {
  .Call("C_EncodeDateTime", x, y, PACKAGE = packageName())
}
