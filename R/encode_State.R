
encode_State <- function(x, oraw = TRUE) {
  o <- fmatch(x,
              get0("uSTE", envir = getOption("dhhs_env")),
              nomatch = 0L)
  if (oraw) {
    as.raw(.Call("Cencode_State", x, o, PACKAGE = packageName()))
  } else {
    .Call("Cencode_State", x, o, PACKAGE = packageName())
  }

}

decode_State <- function(x) {
  uSTE <- get0("uSTE", envir = getOption("dhhs_env"))
  if (is.raw(x)) {
    .Call("Cdecode_State", x, uSTE, PACKAGE = packageName())
  } else {
    x <- .Call("Ccurdle0", x, PACKAGE = packageName())
    uSTE[x]
  }
}

