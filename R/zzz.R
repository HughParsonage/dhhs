
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("dhhs_env"))) {
    options(dhhs_env = new.env())
  }
  dhhs_env <- getOption("dhhs_env")
  hash_assign <- function(nom, value, envir = dhhs_env) {
    opt_nom <- paste0("dhhs_", nom)
    tbl <- value <- unique(value)
    fastmatch::fmatch(value, tbl)
    assign(nom, value = value, envir = envir)
  }

  uRecordType <-
    c("Acquisition Contact",
      "COVID-19 Case Assessment",
      "COVID-19 Case Screening",
      "Casual Contact",
      "Close Contact",
      "Secondary Close Contact",
      "Unclassified Contact")

  hash_assign("uRecordType", uRecordType)

  uPermitType <- c("Green", "Orange", "Red")
  hash_assign("uPermitType", uPermitType)

  uMetroRural <- c("Metro", "Rural", "Unknown", "Interstate", "Overseas")
  hash_assign("uMetroRural", uMetroRural)

  uLabSummary <- c("No results", "Not Detected", "Detected")
  hash_assign("uLabSummary", uLabSummary)

  uSTE <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT")
  hash_assign("uSTE", uSTE)

  uPrimaryWorkplace <- c(NA, "GP clinic", "Hospital", "Other healthcare")
  hash_assign("uPrimaryWorkplace", uPrimaryWorkplace)

  uLostToFollowUpReason <-
    c(NA,
      "Unable to be interviewed within a month",
      "Case refused to be interviewed but it is unknown if they have isolated",
      "Contacted with incomplete interview and not cleared")
  hash_assign("uLostToFollowUpReason", uLostToFollowUpReason)

  uClearedReason <-
    c(NA,
      "Normal community clearance",
      "One negative swab",
      "Two negative swabs taken at least 24 hours apart")
  hash_assign("uClearedReason", uClearedReason)

  uCountryOfBirthCALD <-
    c("Australia", "Missing", "Overseas - English main", "Overseas - English non-main")
  hash_assign("uCountryOfBirthCALD", uCountryOfBirthCALD)


  HealthCareTable <-
    list(HealthcareWorkerBroad = c("Medical practitioner",
                                   "Not a healthcare worker",
                                   "Not a healthcare worker",
                                   "Not stated or under investigation",
                                   "Not stated or under investigation",
                                   "Nurse",
                                   "Other Healthcare Worker",
                                   "Other Healthcare Worker", "Other Healthcare Worker", "Other Healthcare Worker",
                                   "Other Healthcare Worker", "Other Healthcare Worker", "Other Healthcare Worker",
                                   "Other Healthcare Worker", "Other Healthcare Worker"),
         HealthcareWorker = c("Medical practitioner",
                              "Non-clinical roles in HCW setting",
                              "Not a healthcare worker",
                              "Under investigation",
                              "Unknown/not stated",
                              "Nurse",
                              "Aboriginal and Torres Strait Islander health worker",
                              "Aged care or disability worker", "Allied health", "Dental professional",
                              "Medical imaging professional", "Midwife", "Other healthcare worker",
                              "Paramedic or patient transport officer", "Pharmacist"))
  assign("HealthCareTable", HealthCareTable, envir = dhhs_env)

  uDiedDueToNotifiableCondition <-
    c(NA, "Alive", "Died due to the notifiable condition", "Died from other/unknown causes",
      "Unknown")
  hash_assign("uDiedDueToNotifiableCondition", uDiedDueToNotifiableCondition)

  uClinicalStatus <-
    c(NA,
      "Under investigation",
      "Admitted, not known to be in ICU",
      "Admitted to ICU",
      "Admitted to ICU, on ventilation",
      "Deceased",
      "Home isolation",
      "Hotel detention",
      "Hospital in the home",
      "Lost to follow up",
      "Well, isolation complete")
  hash_assign("uClinicalStatus", uClinicalStatus)

  uSeverity <-
    c(NA,
      "Asymptomatic",
      "Symptomatic, not hospitalised",
      "Admitted, not ICU",
      "ICU",
      "Died")
  hash_assign("uSeverity", uSeverity)

  uClassification <-
    c(NA,
      "Acquisition contact",
      "Casual contact",
      "Confirmed",
      "Contact - active",
      "Historical",
      "Not notifiable",
      "Probable",
      "Rejected",
      "Rejected - no testing",
      "Rejected after testing",
      "Rejected - contact > 14 days",
      "Secondary contact - active",
      "Secondary contact - rejected",
      "At Risk",
      "Suspected",
      "Notifiable")

  uAcquired <-
    c(NA, "Travel overseas", "Contact with a confirmed case",
      "Acquired in Australia, unknown source", "Under investigation")

  hash_assign("uClassification", uClassification)
  hash_assign("uAcquired", uAcquired)



}


.onUnload <- function (libpath) {
  library.dynam.unload(packageName(), libpath)
}


