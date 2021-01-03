#' @title Sitrep file
#' @name sitrep_file
#' @description Convenience function to point to the correct location of
#' sitrep files on the E drive.
#' @return A string, the path to the file requested.
#' @author Hugh Parsonage
#' @param view One of the (given) strings, which file to read. The argument is
#' called 'view' reflecting the SQL notion of a View.
#'
#' \code{view} may also be a literal filename, in which case it is returned immediately
#' and other arguments will be ignored, silently.
#'
#' Partial matches are accepted, and if multiple matches are plausible then the best
#' is chosen, with a message.
#'
#' @param previous,latest (TRUE or FALSE, default: \code{FALSE}) Should the previous/latest
#' day's file be returned?
#' @param fst (TRUE or FALSE, default: \code{FALSE}) Should the \code{.fst}
#' version be returned. If \code{FALSE}, the default, the path returned is the
#' standard \code{.txt} delimited/flat file.
#' @param must_exist (TRUE or FALSE, default: negation of \code{fst}) If \code{TRUE},
#' but the requested file does not exist, an error is raised. Otherwise, the putative
#' file location is returned.
#' @param exact \code{FALSE | TRUE} If \code{TRUE}, then \code{view} must match exactly,
#' (up to file extensions).
#'
#' @return A \code{data.table}, the file requested.
#'
#' @details
#'
#' The following columns (if present) are modified in the following way:
#' \describe{
#' \item{\code{*Date}}{Any columns with 'Date' in their name will be coerced to \code{Date} class.}
#' \item{\code{Classification}}{Missing values are coalesced to \code{dhhs:::CLASSIFICIATION_MISSING_VALUE_ALIAS()}.}
#' \item{\code{AgeGroupTenYr}}{Missing values are coalesced to \code{dhhs:::AGEGROUP_MISSING_VALUE_ALIAS()}
#'   and "00-09" is mapped to "0-9".}
#' \item{\code{Latitude,Longitude}}{Any rows where \code{Latitude > 90} are assumed to be rows where the
#' latitude and longitude are confused/entered the wrong way and are switched back. Values of -999
#' are set to \code{NA_real_}.}
#' }
#'
#'
#'
#' @export
sitrep_file <- function(view = c("linelist",
                                 "AdmitTimeline",
                                 "AllClusters",
                                 "CaseLinks",
                                 "CasesCluster",
                                 "ClusterSites",
                                 "Comorbidities",
                                 "ContactDates",
                                 "Contacts",
                                 "Daneetable",
                                 "EventLog",
                                 "EventStatus",
                                 "labresults",
                                 "MonitoringLogging",
                                 "MonitoringLoggingComplete",
                                 "Persons",
                                 "Presentations",
                                 "SITREPTable",
                                 "status"),
                        previous = FALSE,
                        latest = FALSE,
                        fst = FALSE,
                        must_exist = !fst,
                        exact = FALSE) {
  if (!missing(view) &&
      length(view) == 1L &&
      is.character(view) &&
      !is.na(view) &&
      file.exists(view)) {
    return(view)
  }
  .views_avbl <- get_choices(view)

  FileDateToday <-
    if (hour(Sys.time()) < 6) {
      warning("Early hours of the morning - 'current day' will be interpreted as yesterday.")
      format(Sys.Date() - 1L, "%Y%m%d")
    } else {
      format(Sys.Date(), "%Y%m%d")
    }
  FileDateYesterday <- format(Sys.Date() - 1L, "%Y%m%d")
  hhss <- format(Sys.time(), "%H%S")

  check_TF(previous)
  check_TF(fst)
  check_TF(must_exist)
  if (fst) {
    fst_dir <- Sys.getenv("R_DHHS_SITREP_FST_TRUNK")
    if (fst_dir == "") {
      if ("D:" %notin% windows_disks()) {
        fst_dir <- tempfile()
      } else {
        fst_dir <- file.path("D:/sitrep-fst-trunk", "fst")
      }
    }
    provide.dir(fst_dir)
    if (previous) {
      outd <- g("{fst_dir}/{FileDateYesterday}/")
    } else if (latest) {
      outd <- g("{fst_dir}/{FileDateToday}/{hhss}/")
    } else {
      outd <- g("{fst_dir}/{FileDateToday}/")
    }
  } else {

    if (previous) {
      outd <- "E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/DART/Data snapshots/sitrep-previous/"
    } else if (latest) {
      outd <- "E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/DART/Data snapshots/latest/"
    } else {
      outd <- "E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/DART/Data snapshots/sitrep/"
    }
  }

  if (missing(view)) {
    view <- "linelist"
  } else {
    if (length(view) != 1) {
      stop("length(view) = ", length(view), ", but must be length-one.")
    }
    if (view %notin% .views_avbl) {

      orig.view <- copy(view)
      # Check for simply the wrong case
      if (VIEW_MATCH <- match(toupper(view), toupper(.views_avbl), nomatch = 0L)) {
        view <- .views_avbl[VIEW_MATCH]
        if (exact) {
          stop(g("`exact = TRUE`, and `view = {orig.view}`. So stopping, as requested."), "\n",
               g("(Did you mean '{view}'?)"))
        }
        message(g("Changing `view = {orig.view}` to `view`."))
      } else {
        # don't need full names
        files.outd <- dir(path = outd)
        adist.view <- adist(view, .views_avbl, ignore.case = TRUE)
        agrep.view <- agrep(view, .views_avbl, ignore.case = TRUE, value = TRUE)

        view <- .views_avbl[which.min(adist.view)]
        err_msg <-
          paste0(g("Using partial match for `view = {orig.view}`. "),
                 g("Using `view = {view}`."), "\n",
                 if (length(agrep.view)) {
                   g("(Did you mean {toString(agrep.view)}?)")
                 })

        (if (exact) stop else warning)(err_msg)
      }
    }
  }

  ext <- if (fst) "fst" else "txt"
  out <- g("{outd}/{view}.{ext}")
  if (must_exist && !file.exists(out)) {
    stop(g("File does not exist `view = {view}`, `previous = {previous}`, `fst = {fst}`\n\n{out}"))
  }
  normalizePath(out, winslash = "/", mustWork = FALSE)
}



#' @title Read COVID Sitrep file
#' @name read_sitrep
#' @description Used to read in sitrep files. Avoids
#' DRY violations throughout; hopefully with performance.
#' @return A \code{data.table} sourced from the requested sitrep file.
#' @param view,prev,latest As in `sitrep_file()` above.
#' @param excl_diag_today (TRUE or FALSE, default: \code{TRUE}) Should
#' rows where \code{DiagnosisDate >= today()} be dropped? Has no effect
#' if the requested file has no such column.
#' @param use_fst Should \code{.fst} files be used if available. If \code{TRUE},
#' but the corresponding file does not exist, it will be created and read in
#' on future reads.
#' @param columns Only useful when reading \code{.fst} files.
#' A character vector of column names to read in, dropping others. If \code{NULL},
#' the default, all columns are read.
#' @param decode If \code{FALSE}, \code{fst} files will be read without being decoded from
#' integer to character. Set to \code{FALSE} if you will decode the items
#' at a later stage and the performance penalty is too large.
#' @param verbose Be chatty and report the output from \code{fread(..., verbose = TRUE)}?
#' @param reset_cache If \code{TRUE}, saved \code{fst} files will be deleted. Useful
#' if the original files have been updated, or if the \code{fst} file had problems.
#' @export
read_sitrep <- function(view,
                        prev = FALSE,
                        latest = FALSE,
                        excl_diag_today = TRUE,
                        use_fst = getOption("dhhs.use_fst", TRUE),
                        columns = NULL,
                        decode = TRUE,
                        verbose = getOption("verbose", FALSE),
                        reset_cache = getOption("dhhs.reset_cache", FALSE),
                        exact = getOption("dhhs.sitrep_file_exact", FALSE)) {
  check_TF(use_fst)
  check_TF(excl_diag_today)
  check_TF(reset_cache)

  view.fst <-
    sitrep_file(view,
                previous = prev,
                latest = latest,
                fst = TRUE,
                exact = exact)

  if (reset_cache && file.exists(view.fst)) {
    file.remove(view.fst)
    file.exists(ext2ext(view.fst, "u.rds")) &&
      file.remove(ext2ext(view.fst, "u.rds"))
  }



  # If user requests fst file, has fst installed,
  # and there's one already prepared, we
  # can return it immediately
  if (use_fst &&
      requireNamespace("fst", quietly = TRUE) &&
      file.exists(view.fst) &&
      endsWith(view.fst, ".fst")) {
    return(read_sitrep_fst(view.fst, columns = columns, decode = decode))
  }
  view.txt <-
    sitrep_file(view,
                previous = prev,
                latest = latest,
                fst = FALSE,
                exact = exact)


  known_char_cols <- c("DateOfDeath",
                       "DischargedDeceased",
                       "LostToFollowUpReason",
                       "AcquiredCountry",
                       "RiskTravelOverseasCountry",
                       "LatestHospital",
                       "HealthServiceManaging",
                       "ResultValue",
                       "LocalHealthServiceClinicalCatchment",
                       "ExcludeFromExternalCommunicationsReason")

  # just get names so colClasses can be used as appopriate
  file_col_names <- names(fread(file = view.txt, nrows = 101))

  known_char_cols <- intersect(file_col_names, known_char_cols)



  out <- fread(file = view.txt,

               # Avoid mistaking missing values for (valid) empty strings
               na.strings = c("", "NA"),

               # PHESSID known to be integer64 -- but we read in
               # as character since we have plenty of RAM
               # and int64 can cause problems for the unwary
               integer64 = "character",

               # Known character columns to avoid bump from bool8
               colClasses = if (length(known_char_cols)) list("character" = c(known_char_cols)),
               showProgress = FALSE,
               # for debug: passed to data.table::fread only
               verbose = verbose)

  # Make date columns Date class (if not already -- see newer versions of data.table)
  for (j in grep("Date|DATE", copy(names(out)), value = TRUE)) {
    # j is the column name, v is the column itself
    v <- .subset2(out, j)
    if (!is.character(v)) {
      next
    }
    # idea to update the column to have class 'Date':
    #  take our current column (e.g. 'DiagDate')
    #  create a new column 'DiagDate_tmp' in which has class 'Date'
    #  delete the original column
    #  rename it to the original name
    # This may seem circuitous but performance is important (there
    # are a lot of date columns, though few distinct dates) so we should
    # exploit this by coercing each date only once (per column) rather
    # than 1.2M times.  The catch is that if we try to coerce distinct
    # dates in a column, part of the column will be date but the rest
    # will be character.  This is not (or at least, may not be) a well-
    # defined operation; hence, we allocate a new column that is class
    # 'Date' from the start.

    k <- g("{j}_tmp")
    out[, (k) := as.Date(.BY[[1]], format = "%d/%m/%Y"), by = c(j)]
    out[, (j) := NULL]
    setnames(out, k, j)
  }
  # Verify we don't have any integer64 columns that have snuck through
  # (Some versions of data.table did not respect the integer64 argument
  # to fread so while this is defensive it is not overly paranoid.)
  sapply(seq_along(out), function(j) {
    if (inherits(v <- .subset2(out, j), "integer64")) {
      stop(names(out)[j], " has type integer64. Unsupported.")
    }
  })

  # Known missing value changes
  # [2020-10-16: 16:10:00] Kara Martin (DHHS)
  #
  # July and August negs, all still be QAd by PHESS. We're all good
  if (hasName(out, "Classification")) {
    Classification <- NULL # CRAN NOTE avoidance
    out[, Classification := fcoalesce(Classification, CLASSIFICIATION_MISSING_VALUE_ALIAS())]
  }
  if (hasName(out, "AgeGroupTenYr")) {
    AgeGroupTenYr <- NULL
    # fix 00-09 to 0-9
    out[, AgeGroupTenYr := fcoalesce(AgeGroupTenYr, AGEGROUP_MISSING_VALUE_ALIAS())]
    out[AgeGroupTenYr == "00-09", AgeGroupTenYr := "0-9"]
  }


  if (excl_diag_today && hasName(out, "DiagnosisDate")) {
    DiagnosisDate <- NULL
    # today means today only if after 6 am
    today <- Sys.Date() - (hour(Sys.time()) < 6L)

    # Exclude cases diagnosed today, assume missing values should be kept
    out <- out[fcoalesce(DiagnosisDate < today, TRUE)]
  }

  if (hasName(out, "Latitude") && hasName(out, "Longitude")) {
    Longitude <- Latitude <- NULL
    # We don't discard overseas latlons, but Latitudes of > 90 are impossible,
    # and there are some clear cases of lat/lon confusion
    out[Latitude > 90, c("Latitude", "Longitude") := list(Longitude, Latitude)]
    # Latitude of -999 is sentinel for missing value
    out[Latitude == -999, Latitude := NA_real_]
    out[Longitude == -999, Longitude := NA_real_]
  }

  # Finally if the user has requested the use of fst (i.e. me!)
  # so long as he has fst installed write the data.table we've
  # prepared for future reads
  if (use_fst && requireNamespace("fst", quietly = TRUE)) {
    # Need to provide the directory (hutils package not avbl yet)
    if (!dir.exists(dirname(view.fst))) {
      dir.create(dirname(view.fst), recursive = TRUE)
    }
    # Need to copy since the write file updates by ref
    write_sitrep_fst(copy(out), view.fst)
  }
  if (!is.null(columns)) {
    return(hutils::selector(out, cols = intersect(columns, names(out)), shallow = TRUE))
  }
  out
}

#' @description Write out a prepared data.table to an
#' \code{\link[fst]{fst}} file, encoding some variables
#' as logicals, integers, for performance.
#' @param DT The data.table so written.
#' @param file.fst The location of the file to be written. RDS files
#' will be written alongside.
#' @param ucx_threshold The threshold value of \code{uniqueN(x)}
#' beyond which character columns will be left as-is. Default
#' of 1000 is somewhat arbitrary -- seemed good enough after a
#' few minutes of testing.
#' @noRd
write_sitrep_fst <- function(DT, file.fst, ucx_threshold = 1000L, compress = 50) {
  stopifnot(is.character(file.fst),
            length(file.fst) == 1L,
            is.data.table(DT))

  uniques <-
    lapply(DT, function(x) {
      if (is.character(x) && length(ux <- unique(x)) <= ucx_threshold) {
        ux
      } else {
        0L
      }
    }) %>%
    # important to name the list so subsets of data
    # can also be decoded
    setNames(names(DT))

  saveRDS(uniques, ext2ext(file.fst, "u.rds"))

  for (j in names(DT)) {
    if (is.character(x <- .subset2(DT, j)) && is.character(ux <- uniques[[j]])) {
      set(DT, j = j, value = chmatch(x, ux))
    }
  }

  fst::write_fst(DT, file.fst, compress = compress)
}

decode_sitrep <- function(DT, uds = NULL, file.u.rds = NULL) {
  if (is.null(uds) && is.null(file.u.rds)) {
    stop("`uds` and `file.u.rds` were both NULL. At least one must be provided.")
  }
  if (is.null(uds)) {
    assert_string(file.u.rds)
    if (!file.exists(file.u.rds)) {
      np_file.u.rds <- normalizePath(file.u.rds, winslash = "/")
      warning("`file.u.rds = {np_file.u.rds}` does not exist so decoding will not occur.")
      return(DT)
    }
    uds <- readRDS(file.u.rds)
  }
  for (j in names(DT)) {
    if (is.integer(v <- .subset2(DT, j)) &&
        hasName(uds, j) &&
        is.character(ux <- .subset2(uds, j))) {
      set(DT, j = j, value = ux[v])
    }
  }
  DT[]
}

read_sitrep_fst <- function(file.fst, columns = NULL, decode = TRUE) {
  check_TF(decode)
  out <- fst::read_fst(file.fst, columns = columns, as.data.table = TRUE)
  if (decode) {
    return(decode_sitrep(out, file.u.rds = ext2ext(file.fst, "u.rds")))
  }
  out
}



