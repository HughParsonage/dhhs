
encode_CALDFlag <- function(x) {
  c(FALSE, TRUE)[fmatch(x, c("Not CALD", "CALD"))]
}

decode_CALDFlag <- function(x) {
  fifelse(x, no = "Not CALD", yes = "CALD", "Missing")
}


