# dhhs 0.5.0 (2020-12-07)

* `columns` in `read_sitrep` now takes the intersection of columns and is 
  used even when not reading in an `fst` file.
* `view` is now more reluctant to accept input that technically partially matches,
  and accepts argument `exact` where partial matching is not tolerated.


## dhhs 0.4.0 (2020-12-01)
 
* Added a `NEWS.md` file to track changes to the package.

### `read_sitrep`
* `columns` now accepts absent columns (the intersection is used)
* `Latitude` and `Longitude` are fixed internally in `read_sitrep`
* `view` is now more lenient, accepting wrong case and slight misspellings.
