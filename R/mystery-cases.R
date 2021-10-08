#' Filter mystery cases
#'
#' A mystery case is one in which the classification is \code{"Confirmed"}
#' and which the \code{Acquired} field is either \code{"Acquired in Australia, unknown source"}
#' or \code{"Under investigation"}
#'
#' @export

filter_mystery_cases <- function(linelist) {
  stopifnot(is.data.table(linelist))
  if (!hasNames(linelist, "Classification", "Acquired")) {
    stop("linelist does not have required columns 'Classification' and ",
         "'Acquired'.")
  }
  tClass <- encode_Classification("Confirmed")
  tAcqui <- encode_Acquired(c("Acquired in Australia, unknown source",
                              "Under investigation"))

  vClass <- .subset2(linelist, "Classification")
  vAcqui <- .subset2(linelist, "Acquired")

  vClass <- encode_Classification(vClass)
  vAcqui <- encode_Acquired(vAcqui)

  linelist[filter2raw(vClass, vAcqui, tClass, tAcqui)]
}
