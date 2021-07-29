test_that("encode_ID is invertible", {
  RecordID <- c("5002P00000ETdllQAD", "5002P00000JiZusQAF")
  encoded <- encode_ID(RecordID)
  expect_true(is.integer(encode_ID(RecordID)))

  decoded <- decode_ID(encode_ID(RecordID), cipher = attr(encoded, "dhhs_fwalnum_cipher"))
  expect_equal(decoded, RecordID)
})
