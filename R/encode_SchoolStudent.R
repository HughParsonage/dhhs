

encode_SchoolStudent <- function(x) {
  c(FALSE, TRUE)[fmatch(x, c("Primary School Aged", "Secondary School Aged"))]
}

decode_SchoolStudent <- function(x) {
  fifelse(x, "Secondary School Aged", "Primary School Aged")
}
