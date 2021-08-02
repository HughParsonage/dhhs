test_that("encode_HealthCare2", {
  o <- encode_HealthCare2("Medical practitioner")
  expect_equal(decode_HealthCare2(o, incl_broad = FALSE), "Medical practitioner")
  expect_equal(decode_HealthCare2(o, incl_broad = TRUE), "Medical practitioner")
  expect_equal(decode_HealthCare2(o, incl_broad = NA),
               list(HealthcareWorker = "Medical practitioner",
                    HealthcareWorkerBroad = "Medical practitioner"))


  a <- c("Medical practitioner", "Not a healthcare worker",
         "Not a healthcare worker", "Not stated or under investigation",
         "Not stated or under investigation", "Nurse", "Other Healthcare Worker",
         "Other Healthcare Worker", "Other Healthcare Worker", "Other Healthcare Worker",
         "Other Healthcare Worker", "Other Healthcare Worker", "Other Healthcare Worker",
         "Other Healthcare Worker", "Other Healthcare Worker")
  b <- c("Medical practitioner",
         "Non-clinical roles in HCW setting", "Not a healthcare worker",
         "Under investigation", "Unknown/not stated", "Nurse",
         "Aboriginal and Torres Strait Islander health worker",
         "Aged care or disability worker", "Allied health", "Dental professional",
         "Medical imaging professional", "Midwife", "Other healthcare worker",
         "Paramedic or patient transport officer", "Pharmacist")

  withr::with_seed(2, {
    is <- sample(seq_along(a), size = 100, replace = TRUE)
    as <- a[is]
    bs <- b[is]
    os <- encode_HealthCare2(bs)
    expect_true(is.integer(os) || is.raw(os))
    bf <- decode_HealthCare2(os, incl_broad = FALSE)
    bt <- decode_HealthCare2(os, incl_broad = TRUE)
    bn <- decode_HealthCare2(os, incl_broad = NA)
    expect_equal(bf, as)
    expect_equal(bt, bs)
    expect_equal(length(bn), 2)
  })


})
