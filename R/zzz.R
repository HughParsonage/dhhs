
.onLoad <- function(libname, pkgname) {
  dhhs_env <- getOption("dhhs_env", new.env())
  hash_assign <- function(nom, value, envir = dhhs_env) {
    opt_nom <- paste0("dhhs_", nom)
    tbl <- value
    fastmatch::fmatch(value, tbl)
    if (is.null(getOption(opt_nom))) {
      options(opt_nom = tbl)
    }
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

}


.onUnload <- function (libpath) {
  library.dynam.unload(packageName(), libpath)
}


