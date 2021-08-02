#' Encode character vectors as logicals
#' @param x A character vector.
#'
#' @return A logical vector with \code{TRUE} and \code{FALSE} matching Y/Yes and
#' N/No. Other values mapped to \code{NA}.
#'
#' \code{isEssentiallyLogical} Returns \code{TRUE} or \code{FALSE}
#' based on whether the input numeric vector is comprised only of
#' 0 and 1 (and NA).
#'
#' @export


encode_YN <- function(x) {
  .Call("CEncodeYN", x, PACKAGE = packageName())
}

encode_YN_startsWith <- function(x) {
  startsWith(x, "Y")
}

.yncols <- function() {
  c("Symptoms", "ContactWithRecordID", "ContactWithPrimaryContact",
    "CaseInterview", "MostRecentICUAdmission", "Healthcare14Days",
    "LostToFollowUp", "HCWFlag", "GMClusterFlag", "TravelOutsideVictoria",
    "HealthcareWorkerOps", "SymptomsARDS", "SymptomsCough", "SymptomsDiarrhoea",
    "SymptomsFever", "SymptomsOther", "SymptomsPneumonia", "SymptomsPneumonitis",
    "SymptomsShortnessOfBreath", "SymptomsSoreThroat", "InterpretorRequired",
     "ConsentFlag", "VirtualWardFlag",
    "ExcludeFromExternalCommunications",
    "CBNRequired", "IsThisVOC",
    "NewToICU", # likely constant
    "InterpretorRequired")
}

#' @rdname encode_YN
#' @export
isEssentiallyLogical <- function(x) {
  .Call("C_isLogical", x, PACKAGE = packageName())
}
