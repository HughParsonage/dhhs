
encode_Indig <- function(Indigenous, IndigenousStatus) {
  uIndigStatus <-
    # ignore synonyms for NA
    c("Not Aboriginal or Torres Strait Islander",
      "Torres Strait Islander but not Aboriginal origin",
      "Aboriginal but not Torres Strait Islander origin",
      "Aboriginal and Torres Strait Islander origin")

  # Minus one so positive => Indigenous
  fmatch(IndigenousStatus, uIndigStatus) - 1L
}

decode_Indig <- function(eIndigenous) {
  uIndigStatus <-
    # ignore synonyms for NA
    c("Not Aboriginal or Torres Strait Islander",
      "Torres Strait Islander but not Aboriginal origin",
      "Aboriginal but not Torres Strait Islander origin",
      "Aboriginal and Torres Strait Islander origin")
  uIndigenous <- c("Not stated", "Non-Indigenous", "Indigenous")

  list(Indigenous = fifelse(uIndigenous > 0L, "Indigenous", "Non-Indigenous", "Not stated"),
       IndigenousStatus = uIndigStatus[eIndigenous])
}


