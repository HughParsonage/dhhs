test_that("encode_ClassificationAcquired", {
  library(data.table)
  DT <-
    list(CC = c("Acquisition contact",
                "Casual contact",
                "Casual contact",
                "Casual contact",
                "Casual contact",
                "Confirmed",
                "Confirmed",
                "Confirmed",
                "Contact - active",
                "Historical",
                "Historical",
                "Not notifiable",
                "Not notifiable",
                "Not notifiable",
                "Not notifiable",
                "Probable",
                "Probable",
                "Rejected",
                "Rejected",
                "Rejected",
                "Rejected",
                "Rejected - contact > 14 days",
                "Rejected - contact > 14 days",
                "Rejected - contact > 14 days",
                "Rejected - contact > 14 days",
                "Rejected - no testing",
                "Rejected - no testing",
                "Rejected - no testing",
                "Rejected after testing",
                "Rejected after testing",
                "Rejected after testing",
                "Rejected after testing",
                "Secondary contact - active",
                "Secondary contact - rejected",
                "Secondary contact - rejected"),
         AA = c(NA,
                NA,
                "Acquired in Australia, unknown source",
                "Contact with a confirmed case",
                "Travel overseas",
                "Acquired in Australia, unknown source",
                "Contact with a confirmed case",
                "Travel overseas",
                NA,
                NA,
                "Travel overseas",
                NA,
                "Acquired in Australia, unknown source",
                "Contact with a confirmed case",
                "Travel overseas",
                "Contact with a confirmed case",
                "Under investigation",
                NA,
                "Acquired in Australia, unknown source",
                "Contact with a confirmed case",
                "Travel overseas",
                NA,
                "Acquired in Australia, unknown source",
                "Contact with a confirmed case",
                "Travel overseas",
                NA,
                "Acquired in Australia, unknown source",
                "Contact with a confirmed case",
                NA,
                "Acquired in Australia, unknown source",
                "Contact with a confirmed case",
                "Travel overseas",
                NA,
                NA,
                "Under investigation"))
  setDT(DT)
  DT[, enc := encode_ClassificationAcquired(CC, AA)]
  DT[, c("C", "A") := decode_ClassificationAcquired(enc)]
  expect_true(is.integer(DT$enc))
  expect_true(is.character(DT$C))
  expect_true(is.character(DT$A))
  expect_identical(DT$C, DT$CC)
  expect_identical(DT$A, DT$AA)




})
