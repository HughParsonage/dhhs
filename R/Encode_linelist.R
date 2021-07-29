
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
      DT[, "ClassAcqEnc" := EncodeClassificationAcquired(Classification, Acquired)]
      DT[, c("Classification", "Acquired") := NULL]
      next
    }

    if (nomj %chin% .yncols()) {
      set(DT, j = j, value = EncodeYN(v))
      next
    }
    oj <-
      switch(nomj,
             "RecordID" = EncodeID(v),
             "AccountID" = EncodeID(v),
             "Contact" = EncodeID(v),
             "PHESSID" = Encode3202(v),
             "LegacyPHESSID" = Encode3202(v),
             "Sex" = startsWith(v, "M"),
             "Symptoms" = c(FALSE, TRUE)[chmatch(v, c("No", "Yes"))],
             NULL)
    if (!is.null(oj)) {
      set(DT, j = j, value = oj)
    }
  }
  DT[]
}





