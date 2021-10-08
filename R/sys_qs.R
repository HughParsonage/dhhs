

sys_qs <- function(NAME) {
  file.qs <- sprintf("./inst/extdata/%s.qs", NAME)
  if (!file.exists(file.qs)) {
    file.qs <- system.file("extdata",
                           sprintf("%s.qs", NAME),
                           package = packageName())
  }
  qs::qread(file.qs)
}

