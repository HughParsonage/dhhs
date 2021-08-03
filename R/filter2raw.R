

filter2raw <- function(x, y, tbl_x, tbl_y = NULL, And = TRUE) {
  .Call("Cfilter_2raw", x, y, tbl_x, tbl_y, And, PACKAGE = packageName())
}
