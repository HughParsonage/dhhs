
encode_LostToFollowUpReason <- function(x) {
  uLostToFollowUpReason <- get_dhhs("uLostToFollowUpReason")
  fmatch(x, uLostToFollowUpReason)
}

decode_LostToFollowUpReason <- function(x) {
  uLostToFollowUpReason <- get_dhhs("uLostToFollowUpReason")
  uLostToFollowUpReason[x]
}


