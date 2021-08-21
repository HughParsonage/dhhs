test_that("encode_ID is invertible", {
  RecordID <- c("5002P00000ETdllQAD", "5002P00000JiZusQAF")
  encoded <- encode_ID(RecordID)
  expect_true(is.integer(encode_ID(RecordID)))

  decoded <- decode_ID(encode_ID(RecordID), cipher = attr(encoded, "dhhs_fwalnum_cipher"))
  expect_equal(decoded, RecordID)
})

test_that("encode_ID works with small numbers in character vectors", {
  nos <- sample.int(1e4)
  noc <- as.character(nos)
  encoded <- encode_ID(noc)
  decoded <- decode_ID(encoded)
  expect_equal(as.integer(encoded), nos)
  expect_equal(decoded, noc)


  String <- "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  Splits <- strsplit(String, split = "")[[1]]

  RecordID <- c("5002P00000ETdllQAD", "5002P00000JiZusQAF",
                sub("...$", "QAF",
                    paste0("5002P00000",
                           sapply(Splits, strrep, 18 - nchar("5002P00000"),
                                  USE.NAMES = FALSE))),
                "100", "1                 ")
  encoded <- encode_ID(RecordID)
  expect_equal(tail(encoded, 1), 1)
  decoded <- decode_ID(encoded)
  expect_equal(trimws(decoded), trimws(RecordID))
})

