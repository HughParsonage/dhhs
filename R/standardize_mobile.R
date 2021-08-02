

standardize_mobile <- function(mobraw) {

  sub("^.*4([0-9][0-9]) ?([0-9]{3}) ?([0-9]{3}).*$",
      "+614\\1\\2\\3",
      mobraw,
      perl = TRUE)
}

standard_mobile_intl <- function(mobraw) {
  .Call("CStandardMobile", mobraw, TRUE, TRUE, PACKAGE = packageName())
}

standard_phone <- function(mobile, home_phone) {

}


