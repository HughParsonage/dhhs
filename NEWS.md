## dhhs 0.17.0

* New functions:
  - `yyyymmdd_HHMMSS_UTC`
  - `const_nchar`

## dhhs 0.14.0

* EncodeID now guarantees round trip for numeric (non-fixed with inputs)

## dhhs 0.10.0 (2021-07-29)

### Enhancements

* When reading sitrep, dates are mapped to `IDate` via a new function `ddmmYYYY2Int`
  which offers considerably improved performance.

### New functions

* `ddmmYYYY2Int` for converting an dd/mm/YYYY character vector to integer
* Various encoding functions

## dhhs 0.8.0 (2021-01-28)

* Fixed bug where `fst = TRUE` could overwrite `.txt` files.

## dhhs 0.7.0 (2021-01-22)
* New environment variable `R_DHHS_SITREP_TXT_TRUNK` to specify the location of text files. Currently
  defaults to 2020 path.
  


## dhhs 0.5.0 (2020-12-07)

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
