test_that("YN", {
  a <- c("Yes", "No", "Yes", "Not applicable", NA)
  expect_identical(EncodeYN(a), c(TRUE, FALSE, TRUE, NA, NA))
})
