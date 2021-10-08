
match_intrnl <- function(x, table_nom, ux = NULL) {
  if (is.raw(x)) {
    # idempotent
    return(x)
  }
  if (is.null(ux)) {
    ux <- get_dhhs(table_nom)
  }
  if (is.null(ux)) {
    stop("`ux` was NULL.")
  }
  m <- fmatch(x, ux)
  if (length(ux) < 255 && !anyNA(m)) {
    as.raw(m)
  } else {
    m
  }
}

decode_intrnl <- function(x, table_nom, ux = NULL) {
  if (is.null(ux)) {
    ux <- get_dhhs(table_nom)
  }
  if (!is.integer(x)) {
    ux[as.integer(x)]
  } else {
    ux[x]
  }
}


