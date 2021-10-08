

dob2age <- function(dob, when = Sys.Date()) {
  dy <- year(when) - year(dob)
  dm <- month(when) - month(dob)
  dd <- mday()
}


