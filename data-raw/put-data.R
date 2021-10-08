# iso3166 <- fst::read_fst("inst/extdata/iso3166.fst", as.data.table = TRUE)
# country_codes <-
#   fread("https://github.com/datasets/country-codes/raw/master/data/country-codes.csv", encoding = "UTF-8") %>%
#   setnames("ISO3166-1-Alpha-3", "iso3c") %>%
#   .[]
#
# iso3166[country_codes, Dial := i.Dial, on = .(iso3c)]
# iso3166[, i := .I]
# fst::write_fst(iso3166, "inst/extdata/iso3166.fst")
library(data.table)
library(magrittr)
library(fst)
cat(hh_ss(), "\n")
LineList <- dhhs:::read_linelist_fst("linelist")
devtools::load_all()

redundant.cols <-  c("AgeAtOnset", "AgeGroupTenYr", "BroadAgeGroup", "Continent")
otherwise.encoded <-
  c("RecordID", "AccountID", "Contact", "HealthServiceManaging",
    "PHESSID", "LegacyPHESSID", "PermitType", "ActiveFlag", "MetroRural",
    "LabSummary", "CovidSafe", "SymptomaticAtTesting", "CALDFlag",
    "PrimaryExposure", "SchoolStudent", "CountryOfBirth", "State",
    "TravelWithinAustraliaState", "PrimaryWorkplace", "LostToFollowUpReason",
    "ClearedReason", "CountryOfBirthCALD")

do_Rawable <- function(DT) {
  file.qs <- paste0("inst/extdata/RawAble.qs")
  if (file.exists(file.qs)) {
    origList <- qs::qread(file.qs)
  } else {
    origList <- NULL
  }

  List <-
    lapply(names(DT), function(nom) {
      orig_uv <- origList[[nom]]  # may be NULL[[]]
      if (is.null(orig_uv)) {
        if (nom %in% c(redundant.cols, otherwise.encoded)) {
          return(NULL)
        }
        cat(formatC(nom, width = max(nchar(names(DT))) + 1), "\r")
        if (grepl("RecordID|PHESSID|AccountID|Date", nom)) {
          return(NULL)
        }
        if (nom %in% c("TravelWithinAustraliaState", "State")) {
          return(NULL)
        }
        v <- .subset2(DT, nom)
        if (isEssentiallyLogical(v) || nom %in% .yncols() || !is.character(v)) {
          return(NULL)
        }

        if (uniqueN(head(v, 1e3)) > 255) {
          return(NULL)
        }
        cat(crayon::red(formatC(nom, width = max(nchar(names(DT))) + 1)), "\r")
      }
      v <- .subset2(DT, nom)
      uv <- union(v, orig_uv)
      if (length(uv) <= 3 || length(uv) > 255) {
        return(NULL)
      }
      uv
    })
  names(List) <- names(DT)
  # Now make sure that we don't omit any possible values
  # (e.g. Person$Status)



  qs::qsave(List, file.qs)
}

Files <- dir(Sys.getenv("R_DHHS_SITREP_FST_TRUNK"),
             pattern = "\\.fst$",
             full.names = TRUE)
Files <- Files[!grepl("Prson|e100", Files)]
cat(hh_ss(), "\t", "fst_columns\n", sep = "")
AllNames <- unique(unlist(lapply(Files, dhhs:::fst_columns)))

List <- sapply(AllNames, function(x) NULL)
qs::qsave(List, "inst/extdata/RawAble.qs")

for (j in seq_along(Files)) {

  filej <- hutils::trim_common_affixes(Files)[j]
  cat(hh_ss(), "\t", filej, "\n")
  assign(filej, read_fst(Files[[j]], as.data.table = TRUE))
}

List <- lapply(AllNames, function(nom) {
  if (grepl("ID$", nom, ignore.case = TRUE) || grepl("Date", nom) ||
      nom %in% c(redundant.cols, otherwise.encoded))  {
    return(NULL)
  }

  cat(hh_ss(), "\t", nom, "\n", sep = "")
  uv <- NULL
  for (j in seq_along(Files)) {
    filej <- hutils::trim_common_affixes(Files)[j]
    if (length(uv) > 255) {
      break
    }
    DT <- get(filej)
    stopifnot(is.data.table(DT))
    if (hasName(DT, nom) && is.character(v <- .subset2(DT, nom))) {
      uv <- union(uv, v)
    }
  }
  if (length(uv) > 255) {
    return(NULL)
  }
  unique(uv)
})
names(List) <- AllNames
qs::qsave(List, "inst/extdata/RawAble.qs")





