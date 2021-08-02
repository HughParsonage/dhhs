
Encode_linelist <- function(DT, do_copy = TRUE) {
  if (!is.data.table(DT)) {
    stop("`DT` was class ", toString(class(DT)), " but must be a data.table.")
  }
  if (isTRUE(do_copy)) {
    DT <- copy(DT)
  }
  const_cols <- which(vapply(DT, isntConstant, 0L) == 0L)
  if (length(const_cols)) {
    setattr(DT, name = "const_cols", DT[, lapply(.SD, first), .SDcols = c(const_cols)])
    DT[, c(const_cols) := NULL]
  }

  noms <- copy(names(DT))
  set_cols_last(DT, c("Classification", "Acquired",
                      "IndigenousStatus", "Indigenous",
                      "Continent"))
  for (j in noms) {
    nomj <- j
    if (!hasName(DT, nomj)) {
      # may happen if cols dropped
      next
    }
    v <- .subset2(DT, nomj)
    if (inherits(v, "Date") && !inherits(v, "IDate")) {
      set(DT, j = j, value = as.IDate(v))
      next
    }

    if (nomj %in% c("Classification", "Acquired")) {
      DT[, "ClassAcqEnc" := encode_ClassificationAcquired(Classification, Acquired)]
      DT[, c("Classification", "Acquired") := NULL]
      next
    }
    if (nomj %in% c("IndigenousStatus", "Indigenous")) {
      DT[, "eIndig" := encode_Indig(IndigenousStatus, Indigenous)]
      DT[,  c("IndigenousStatus", "Indigenous") := NULL]
      next
    }

    if (nomj %chin% .yncols()) {
      set(DT, j = j, value = encode_YN(v))
      next
    }
    if (isEssentiallyLogical(v)) {
      set(DT, j = j, value = as.logical(v))
      next
    }
    if (nomj == "Sex") {
      set(DT, j = "Sex", value = startsWith(v, "M"))
      setnames(DT, "Sex", "isMale")
      next
    }

    oj <-
      switch(nomj,
             "RecordID" = encode_ID(v),
             "AccountID" = encode_ID(v),
             "Contact" = encode_ID(v),
             "HealthServiceManaging" = encode_ID(v),
             "PHESSID" = encode_3202(v),
             "LegacyPHESSID" = encode_3202(v),
             "PermitType" = encode_PermitType(v),
             "ActiveFlag" = v == "Active",
             "MetroRural" = encode_MetroRural(v),
             "LabSummary" = encode_LabSummary(v),
             "CovidSafe" = startsWith(v, "Y"),
             "SymptomaticAtTesting" = encode_SymptomaticAtTesting(v),
             "CALDFlag" = encode_CALDFlag(v),
             "PrimaryExposure" = v == "Primary",
             "SchoolStudent" = encode_SchoolStudent(v),
             "CountryOfBirth" = encode_iso3c(v),
             "State" = encode_State(v),
             "TravelWithinAustraliaState" = encode_State(v),
             "PrimaryWorkplace" = encode_PrimaryWorkplace(v),
             "LostToFollowUpReason" = encode_LostToFollowUpReason(v),
             "ClearedReason" = encode_ClearedReason(v),
             NULL)
    if (!is.null(oj)) {
      set(DT, j = j, value = oj)
    }
  }

  # Unnecessary columns (or columns that may be inferred from others)


  DT[, c("AgeAtOnset", "AgeGroupTenYr", "BroadAgeGroup", "Continent") := NULL]
}





