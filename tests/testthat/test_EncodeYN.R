test_that("YN", {
  a <- c("Yes", "No", "Yes", "Not applicable", NA)
  expect_identical(encode_YN(a), c(TRUE, FALSE, TRUE, NA, NA))
})
