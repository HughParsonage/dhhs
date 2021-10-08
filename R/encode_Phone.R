

encode_Phone <- function(DT, quiet = !interactive()) {
  loud <- isFALSE(quiet)
  if (!requireNamespace("dauphin", quietly = TRUE)) {
    if (loud) {
      message("dauphin not loaded so no encoding done.")
    }
    return(DT)
  }
  if (!hasName(DT, "Mobile") && !hasName(DT, "HomePhone")) {
    return(DT)
  }
  if (hasName(DT, "Mobile") && !hasName(DT, "HomePhone")) {
    v <- .subset2(DT, "Mobile")
    mv <- dauphin::dauphin_mobile(v)
    if (is.atomic(mv)) {
      set(DT, j = "Mobile", v = as.integer(mv))
    }
  }
  if (hasName(DT, "HomePhone")) {
    u <- .subset2(DT, "Mobile")
    v <- .subset2(DT, "HomePhone")
    DT[, c("Mobile", "HomePhone") := dauphin::dauphin_mobile_landline(u, v)]
  }
  DT[]
}

