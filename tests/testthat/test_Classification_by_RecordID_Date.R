test_that("Classification_by_RecordID_Date", {

  EventLog1 <-
    data.table(RecordID = c(rep(1L, 4), rep(2L, 4), 3L, rep(4L, 5L)),
               EventDate = c(18800L + 1:4, 18800L + 1:4, 18800L, 18800L + 1:5))

  EventLog1[, EventClassification := c("At Risk", "Rejected", "At Risk", "Confirmed",
                                       "At Risk", "Confirmed", "")]

})
