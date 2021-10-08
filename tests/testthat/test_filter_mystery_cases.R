library(tinytest)
library(dhhs)
library(data.table)

filter_mystery_casez <- function(DT) {
  DT[Classification == "Confirmed"][Acquired %in% c("Acquired in Australia, unknown source", "Under investigation")]
}
DT <- CJ(Classification = dhhs:::get_dhhs("uClassification"),
         Acquired = dhhs:::get_dhhs("uAcquired"))
DT[, i := .I]
DTz <- DT[rep((7 ^ (seq_len(.N) / 15)) %% .N, rep_len(as.integer(.leap.seconds) %/% 86400L, .N))]
DTz[, ii := .I]
expect_true(hasName(DT, "i"))
expect_true(hasName(DTz, "ii"))
expect_equal(.subset2(filter_mystery_cases(DT), "i"),
             .subset2(filter_mystery_casez(DT), "i"))
expect_equal(.subset2(filter_mystery_cases(DTz), "ii"),
             .subset2(filter_mystery_casez(DTz), "ii"))

