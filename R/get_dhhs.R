#' Get an object from package environment
#'
#' @noRd

get_dhhs <- function(name) {
  get0(name, envir = getOption("dhhs_env"))
}
