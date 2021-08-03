test_that("filtering", {
  library(data.table)
  options(datatable.auto.index = FALSE)
  options(datatable.use.index = FALSE)
  setDTthreads(1)
  skip_if_not_installed("withr")
  skip_if_not_installed("hutils")
  sampl <- function(x, size = 181) hutils::samp(x, size = size, loud = FALSE)
  uClinicalStatus <- get_dhhs("uClinicalStatus")
  uSeverity <- get_dhhs("uSeverity")
  uClinical <- get_dhhs("uClinicalStatus")


  withr::with_seed(2, {
    Linelisto <- data.table(ClinicalStatus = sampl(get_dhhs("uClinicalStatus")),
                            Severity = sampl(get_dhhs("uSeverity")))
    Linelisto[, i := .I]
    Lineliste <- Encode_linelist(Linelisto)
  })
  Expect_A <- Linelisto[ClinicalStatus %chin% c("Well, isolation complete", "Admitted to ICU")]
  Actual_A <- filter_ClinicalStatus_Severity(Lineliste,
                                             ClinicalStatus = c("Well, isolation complete",
                                                                "Admitted to ICU"))
  expect_equal(Expect_A$i, Actual_A$i)

  for (size_1 in 0:length(uClinical)) {
    uClinicalCombn <-
      if (size_1) {
        combn(uClinical, m = size_1)
      }

    for (size_2 in 0:length(uSeverity)) {
      uSeverityCombn <-
        if (size_2) {
          combn(uSeverity, m = size_2)
        }
      for (ss in 12:12) {
        withr::with_seed(ss, {
          Linelisto <- data.table(ClinicalStatus = sampl(get_dhhs("uClinicalStatus")),
                                  Severity = sampl(get_dhhs("uSeverity")))
          Linelisto[, i := .I]
          Lineliste <- Encode_linelist(Linelisto)
        })
        if (size_1 <= 2) {
          Expect_A <- Linelisto[ClinicalStatus %chin% c("Well, isolation complete", "Admitted to ICU")]
          Actual_A <- filter_ClinicalStatus_Severity(Lineliste,
                                                     ClinicalStatus = c("Well, isolation complete",
                                                                        "Admitted to ICU"))
          Actual_B <- filter_ClinicalStatus_Severity(Linelisto,
                                                     ClinicalStatus = c("Well, isolation complete",
                                                                        "Admitted to ICU"))
          expect_equal(Expect_A$i, Actual_A$i)
          expect_equal(Expect_A$i, Actual_B$i)
          Expect_A <- Linelisto[`&`(ClinicalStatus %chin% c("Well, isolation complete", "Admitted to ICU"),
                                    Severity == "Asymptomatic")]
          Actual_A <- filter_ClinicalStatus_Severity(Lineliste,
                                                     ClinicalStatus = c("Well, isolation complete",
                                                                        "Admitted to ICU"),
                                                     Severity = "Asymptomatic")
          Actual_B <- filter_ClinicalStatus_Severity(Linelisto,
                                                     ClinicalStatus = c("Well, isolation complete",
                                                                        "Admitted to ICU"),
                                                     Severity = "Asymptomatic")
          expect_equal(Expect_A$i, Actual_A$i)
          expect_equal(Expect_A$i, Actual_B$i)
        }

        # use NCOL for NULL
        for (ic in 1:NCOL(uClinicalCombn)) {
          crhs <- uClinicalCombn[, ic] # NULL ok
          for (js in 1:NCOL(uSeverity)) {
            srhs <- uSeverityCombn[, js]
            if (is.null(crhs) && is.null(srhs)) {
              Ex <- (Linelisto)
            } else if (is.null(crhs)) {
              Ex <- Linelisto[Severity %in% srhs]
            } else if (is.null(srhs)) {
              Ex <- Linelisto[ClinicalStatus %in% crhs]
            } else {
              Ex <- Linelisto[ClinicalStatus %in% crhs][Severity %in% srhs]
            }
            Ac <- filter_ClinicalStatus_Severity(Lineliste, crhs, srhs)
            expect_equal(Ex$i, Ac$i, info = paste(ic, js, "|", toString(crhs), "|", toString(srhs)))
          }
        }
      }
    }
  }

})
