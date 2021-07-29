
Encode_linelist <- function(DT, do_copy = TRUE) {
  if (isTRUE(do_copy)) {
    DT <- copy(DT)
  }
  noms <- copy(names(DT))
  set_cols_last(DT, c("Classification", "Acquired"))
  for (j in noms) {
    nomj <- j
    if (!hasName(DT, nomj)) {
      # may happen if cols dropped
      next
    }
    v <- .subset2(DT, nomj)
    if (nomj %in% c("Classification", "Acquired")) {
      DT[, "ClassAcqEnc" := encode_ClassificationAcquired(Classification, Acquired)]
      DT[, c("Classification", "Acquired") := NULL]
      next
    }

    if (nomj %chin% .yncols()) {
      set(DT, j = j, value = encode_YN(v))
      next
    }
    oj <-
      switch(nomj,
             "RecordID" = encode_ID(v),
             "AccountID" = encode_ID(v),
             "Contact" = encode_ID(v),
             "PHESSID" = encode_3202(v),
             "LegacyPHESSID" = encode_3202(v),
             NULL)
    if (!is.null(oj)) {
      set(DT, j = j, value = oj)
    }
  }

  # Unnecessary columns (or columns that may be inferred from others)


  DT[, c("AgeAtOnset", "AgeGroupTenYr", "BroadAgeGroup") := NULL]
}





