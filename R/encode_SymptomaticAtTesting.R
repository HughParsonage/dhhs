

encode_SymptomaticAtTesting <- function(x) {
  c(FALSE, TRUE)[fmatch(x, c("Asymptomatic at Testing", "Symptomatic at Testing"))]
}

decode_SymptomaticAtTesting <- function(x) {
  c("Asymptomatic at Testing", "Symptomatic at Testing")[x]
}
