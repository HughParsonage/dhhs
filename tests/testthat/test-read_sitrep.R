test_that("read_sitrep works", {
  Contacts <- read_sitrep("Contacts")
  expect_true(is.character(Contacts$TargetPHESSID))
  expect_true(is.character(Contacts$SourcePHESSID))
  expect_true(inherits(Contacts$TargetDiagnosisDate, "Date"))
  expect_true(inherits(Contacts$SourceDiagnosisDate, "Date"))

  ContactDates <-
    read_sitrep("ContactDates")

  expect_equal(sapply(ContactDates, class),
               c(PhessID = "character",
                 PartyId = "character",
                 CONTACT_WITH_NCOV = "character",
                 CONTACT_WITH_NCOV_DATE_MAX = "Date",
                 CONTACT_WITH_NCOV_DATE = "Date"))
})
