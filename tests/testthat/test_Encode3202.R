test_that("encode_3202", {
  x <- c("320200000002", "320201000000")
  expect_true(Validate3202(x))
  expect_equal(decode_3202(encode_3202(x)), x)
  x <- c("320200000002", "320201000000", NA)
  expect_true(Validate3202(x))
  expect_equal(decode_3202(encode_3202(x)), x)
  y <- c("320200000002", "420201000000")
  expect_error(Validate3202(y))
  expect_error(Validate3202("12345678901"))
  expect_error(Validate3202("a"))
})
