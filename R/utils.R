
g <- glue::glue

ext2ext <- function(file.ext, new.ext) {
  paste0(tools::file_path_sans_ext(file.ext), new.ext)
}

is_windows <- function() .Platform$OS.type == "windows"

windows_disks <- function(option = 0L) {
  if (option && !is_windows()) {
    switch(option,
           warning("Not on Windows, returning character(0)."),
           stop("Not on Windows, returning character(0)"))
    return(invisible(character(0)))
  }
  wmic_output <- trimws(shell("wmic logicaldisk get caption", intern = TRUE))
  wmic_output[-1] # first entry is 'caption'
}

"%notin%" <- function(x, y) {
  # need to divert based on length since logical(0) not a valid output
  if (length(y)) {
    is.na(match(x, y))
  } else {
    # no length => all true
    rep.int(TRUE, length(x))
  }
}

provide.dir <- function (path, ...) {
  if (dir.exists(path) ||
      dir.create(path, recursive = TRUE, ...)) {
    return(path)
  }
  ""
}

distinct_dt <- function(x) {
  # dplyr::distinct is faster
  if (requireNamespace("dplyr", quietly = TRUE)) {
    return(dplyr::distinct(x))
  }
  # generally one thread is faster
  threads <- getDTthreads()
  setDTthreads(1)
  out <- unique(x)
  setDTthreads(threads)
  out
}

vname <- function(x) {
  # from checkmate
  paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L),
         collapse = "\n")
}

assert_string <- function(x) {
  if (!is.character(x)) {
    stop(g("{vname(x)} was class {toString(x)}, type {typeof(x)}, but must be a string."))
  }
  if (length(x) != 1L) {
    stop(g("{vname(x)} was length {length(x)}, but must be length-one."))
  }
}

fst_columns <- function(file.fst) {
  # Columns available from an fst
  fst::metadata_fst(file.fst)[["columnNames"]]
}

# Returns the default argument for arg
get_choices <- function(arg) {

  formal.args <- formals(sys.function(sysP <- sys.parent()))
  choices <- eval(formal.args[[as.character(substitute(arg))]],
                  envir = sys.frame(sysP))
  choices
}


