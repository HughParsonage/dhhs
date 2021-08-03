
encode_DiedDueToNotifiableCondition <- function(x) {
  uDiedDueToNotifiableCondition <- get_dhhs("uDiedDueToNotifiableCondition")
  fmatch(x, uDiedDueToNotifiableCondition)
}

