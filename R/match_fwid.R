

match_fwid <- function(x, y, verify = 1L) {
  oy <- order(y)
  sy <- y[oy]
  ans <- .Call("Cmatch_fwid", x, sy, verify, PACKAGE = "dhhs")
}

