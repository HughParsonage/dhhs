#' Encode character vectors as logicals
#' @param x A character vector.
#'
#' @return A logical vector with \code{TRUE} and \code{FALSE} matching Y/Yes and
#' N/No. Other values mapped to \code{NA}.
#'
#' @export


encode_YN <- function(x) {
  .Call("CEncodeYN", x, PACKAGE = packageName())
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

    "InterpretorRequired")
}
