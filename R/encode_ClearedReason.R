
encode_ClearedReason <- function(x) {
  uClearedReason <- get_dhhs("uClearedReason")
  fmatch(x, uClearedReason)
}

decode_ClearedReason <- function(x) {
  uClearedReason <- get_dhhs("uClearedReason")
  uClearedReason[x]
}

