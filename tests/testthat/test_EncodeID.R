test_that("EncodeID is invertible", {
  RecordID <- c("5002P00000ETdllQAD", "5002P00000JiZusQAF")
  encoded <- EncodeID(RecordID)
  expect_true(is.integer(EncodeID(RecordID)))

  decoded <- DecodeID(EncodeID(RecordID), cipher = attr(encoded, "dhhs_fwalnum_cipher"))
  expect_equal(decoded, RecordID)
})
