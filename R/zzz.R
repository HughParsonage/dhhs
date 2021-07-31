
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("dhhs_env"))) {
    options(dhhs_env = new.env())
  }
  dhhs_env <- getOption("dhhs_env")
  hash_assign <- function(nom, value, envir = dhhs_env) {
    opt_nom <- paste0("dhhs_", nom)
    tbl <- value
    fastmatch::fmatch(value, tbl)
    assign(nom, value = value, envir = envir)
  }


  uRecordType <-
    c("Acquisition Contact",
      "COVID-19 Case Assessment",
      "COVID-19 Case Screening",
      "Casual Contact",
      "Close Contact",
      "Secondary Close Contact",
      "Unclassified Contact")

  hash_assign("uRecordType", uRecordType)

  uPermitType <- c("Green", "Orange", "Red")
  hash_assign("uPermitType", uPermitType)

  uMetroRural <- c("Metro", "Rural", "Unknown", "Interstate", "Overseas")
  hash_assign("uMetroRural", uMetroRural)

  uLabSummary <- c("No results", "Not Detected", "Detected")
  hash_assign("uLabSummary", uLabSummary)

}


.onUnload <- function (libpath) {
  library.dynam.unload(packageName(), libpath)
}


