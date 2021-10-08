#' Encode/Decode using memory-light objects
#' @param DT The \code{data.table} to encode/decode.
#' @param do_copy Whether \code{DT} should first be copied.
#' @export

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


  if (hasName(DT, "HealthcareWorker")) {
    if (hasName(DT, "HealthcareWorkerBroad")) {
      DT[, "HealthcareWorkerBroad" := NULL]
    }
    DT[, "HealthcareWorker" := encode_HealthCare2(HealthcareWorker)]
  }
  if (hasName(DT, "Mobile") && hasName(DT, "HomePhone")) {
    DT <- encode_Phone(DT)
  }

  noms <- copy(names(DT))
  set_cols_last(DT, c("Classification", "Acquired",
                      "IndigenousStatus", "Indigenous",
                      "Continent",
                      "HealthcareWorkerBroad"))





  RawAbleLineList <- sys_qs("RawAble")

  for (j in noms) {
    nomj <- j
    if (!hasName(DT, nomj)) {
      # may happen if cols dropped
      next
    }
    v <- .subset2(DT, nomj)
    if (isEssentiallyLogical(v)) {
      set(DT, j = j, value = as.logical(v))
      next
    }
    if (is.numeric(v)) {
      next
    }

    if (inherits(v, "Date") && !inherits(v, "IDate")) {
      set(DT, j = j, value = as.IDate(v))
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
             "CountryOfBirthCALD" = encode_CountryOfBirthCALD(v),
             NULL)
    if (!is.null(oj)) {
      set(DT, j = j, value = oj)
      next
    }
    if (!is.null(uJ <- RawAbleLineList[[j]])) {
      tryCatch(match_intrnl(v, ux = uJ),
               warning = function(e) {
                 cat(e$m, "\n")
                 cat(hutilscpp::range_rcpp(uJ), "n")
               })
      set(DT, j = j, value = match_intrnl(v, ux = uJ))

    }



  }

  # Unnecessary columns (or columns that may be inferred from others)
  hutils::drop_cols(DT, c("AgeAtOnset", "AgeGroupTenYr", "BroadAgeGroup", "Continent"))
}

#' @rdname Encode_linelist
#' @export
Decode_linelist <- function(DT, do_copy = TRUE) {
  if (isTRUE(do_copy)) {
    DT <- copy(DT)
  }
  for (nom in c("RecordID", "AccountID", "Contact", "HealthServiceManaging")) {
    if (hasName(DT, nom)) {
      set(DT, j = nom, value = decode_ID(.subset2(DT, nom)))
    }
  }
  for (nom in c("PHESSID", "LegacyPHESSID")) {
    if (hasName(DT, nom)) {
      set(DT, j = nom, value = decode_3202(.subset2(DT, nom)))
    }
  }
  if (hasName(DT, "PermitType")) {
    set(DT, j = "PermitType", value = decode_PermitType(.subset2(DT, "PermitType")))
  }
  for (nom in c("TravelWithinAustraliaState", "State")) {
    if (hasName(DT, nom)) {
      set(DT, j = j, value = decode_State(.subset2(DT, nom)))
    }
  }
  if (hasName(DT, "CountryOfBirth") &&
      is.integer(v <- .subset2(DT, "CountryOfBirth"))) {
    set(DT, j = "CountryOfBirth", value = decode_iso3n(v))
    rm(v)
  }
  if (hasName(DT, "MetroRural")) {
    set(DT, j = "MetroRural", value = decode_MetroRural(.subset2(DT, "MetroRural")))
  }
  if (hasName(DT, "LabSummary")) {
    set(DT, j = "LabSummary", value = decode_LabSummary(.subset2(DT, "LabSummary")))
  }
  if (hasName(DT, "SchoolStudent")) {
    set(DT, j = "SchoolStudent", value = decode_SchoolStudent(.subset2(DT, "SchoolStudent")))
  }
  if (hasName(DT, "LostToFollowUpReason")) {
    set(DT, j = "LostToFollowUpReason", value = decode_LostToFollowUpReason(.subset2(DT, "LostToFollowUpReason")))
  }
  if (hasName(DT, "ClearedReason")) {
    set(DT, j = "ClearedReason", value = decode_ClearedReason(.subset2(DT, "ClearedReason")))
  }
  if (hasName(DT, "CountryOfBirthCALD")) {
    set(DT, j = "CountryOfBirthCALD", value = decode_CountryOfBirthCALD(.subset2(DT, "CountryOfBirthCALD")))
  }



  Decode_RawAbleLineList(DT)

}


Decode_RawAbleLineList <- function(DT) {
  RawAbleLineList <- sys_qs("RawAbleLineList")
  for (nom in names(RawAbleLineList)) {
    if (!is.null(ux <- .subset2(RawAbleLineList, nom)) && hasName(DT, nom)) {
      tryCatch(set(DT, j = nom, value = decode_intrnl(.subset2(DT, nom), ux = ux)),
               error = function(e) {

                 cat(e$m, "\n",
                     nom, "\n",
                     sep = "")
               })
    }
  }
  DT
}







