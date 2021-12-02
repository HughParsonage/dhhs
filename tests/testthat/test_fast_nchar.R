test_that("fast_nchar works", {
  expect_equal(fast_nchar("12345678"), 8L)
  expect_equal(fast_nchar(12345678L), 8L)
})

test_that("max_nchar works", {
  x <- character(0)
  expect_equal(max_nchar(x), 0L)
  x <- c("2021-05-1200:00:00 +00:00", "2021-06-1000:00:00 +00:00")
  expect_equal(max_nchar(x), max(nchar(x, type = "bytes")))
})
