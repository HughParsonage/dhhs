

EncodeClassificationAcquired <- function(Classification, Acquired) {
  .Call("CEncodeClassificationAcquired", Classification, Acquired, PACKAGE = packageName())
}

DecodeClassificationAcquired <- function(x) {
  uClassifications <-
    c("Acquisition contact",
      "Casual contact",
      "Confirmed",
      "Contact - active",
      "Historical", "Not notifiable",
      "Probable",
      "Rejected",

      "Rejected - no testing",
      "Rejected after testing",
      "Rejected - contact > 14 days",
      "Secondary contact - active",
      "Secondary contact - rejected")

  uAcquired <-
    c(NA, "Travel overseas", "Contact with a confirmed case",
      "Acquired in Australia, unknown source", "Under investigation")


  list(Classification = uClassifications[bitwAnd(x, 15L)],
       Acquired = uAcquired[bitwShiftR(x, 16L)])
}
