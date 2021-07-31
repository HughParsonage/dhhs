iso3166 <- fst::read_fst("inst/extdata/iso3166.fst", as.data.table = TRUE)
country_codes <-
  fread("https://github.com/datasets/country-codes/raw/master/data/country-codes.csv", encoding = "UTF-8") %>%
  setnames("ISO3166-1-Alpha-3", "iso3c") %>%
  .[]

iso3166[country_codes, Dial := i.Dial, on = .(iso3c)]

