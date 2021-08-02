
encode_PrimaryWorkplace <- function(x) {
  uPrimaryWorkplace <- get_dhhs("uPrimaryWorkplace")
  fmatch(x, uPrimaryWorkplace)
}

decode_PrimaryWorkplace <- function(x) {
  uPrimaryWorkplace <- get_dhhs("uPrimaryWorkplace")
  uPrimaryWorkplace[x]
}

