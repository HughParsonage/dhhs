

encode_LabSummary <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  uLabSummary<- get0("uLabSummary", envir = getOption("dhhs_env"))
  fmatch(x, uLabSummary)
}

decode_LabSummary <- function(x) {
  uLabSummary<- get0("uLabSummary", envir = getOption("dhhs_env"))
  uLabSummary[x]
}
