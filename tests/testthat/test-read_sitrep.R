test_that("read_sitrep works", {
  skip_if_not(dir.exists("E:/PBIX/"))
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


test_that("sitrep_file doesn't overwrite existing files", {
  skip_if_not(dir.exists("E:/PBIX/"))
  skip_if_not(getRversion() >= "4.0.0")
  temp_path <-
    r"{E:\PBIX\NCoronavirus 2020\Stata nCoV reporting\31 Azure Data Model\DART\Data snapshots\CRMSampleData\Solid name for DPC}"
  expect_false(
    identical(sitrep_file(file.path(temp_path, paste0("linelist", ".txt")), fst = TRUE),
              sitrep_file(file.path(temp_path, paste0("linelist", ".txt")), fst = FALSE))
  )
})
