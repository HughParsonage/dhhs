test_that("ddmmyyyy2Int works", {
  expect_equal(ddmmyyyy2Int("04/03/2000"),
               as.integer(as.Date("04/03/2000", format = "%d/%m/%Y")))

  dates <- seq.Date(as.Date("2019-01-01"), as.Date("2022-12-31"), by = "1 day")
  dates_ddmmyyyy <- format(dates, "%d/%m/%Y")
  expect_identical(ddmmyyyy2Int(dates_ddmmyyyy),
                   as.integer(dates))

})
