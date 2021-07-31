
encode_iso3c <- function(x) {
  CountryCodes <-
    fst::read_fst(system.file("extdata", "iso3166.fst", package = packageName()),
                  as.data = TRUE)
  iso3n <- .subset2(CountryCodes, "iso3n")
  iso3c <- .subset2(CountryCodes, "iso3c")
  iso3n[fmatch(x, iso3c)]
}

decode_iso3n <- function(x) {
  CountryCodes <-
    fst::read_fst(system.file("extdata", "iso3166.fst", package = packageName()),
                  as.data = TRUE)

  iso3n <- .subset2(CountryCodes, "iso3n")
  iso3c <- .subset2(CountryCodes, "iso3c")
  iso3c[fmatch(x, iso3n)]
}

