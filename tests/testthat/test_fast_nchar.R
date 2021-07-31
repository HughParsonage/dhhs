test_that("fast_nchar works", {
  expect_equal(fast_nchar("12345678"), 8L)
  expect_equal(fast_nchar(12345678L), 8L)
})
