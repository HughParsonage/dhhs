
encode_CountryOfBirthCALD <- function(x) {
  uCountryOfBirthCALD <- get_dhhs("uCountryOfBirthCALD")
  fmatch(x, uCountryOfBirthCALD)
}

decode_CountryOfBirthCALD <- function(x) {
  uCountryOfBirthCALD <- get_dhhs("uCountryOfBirthCALD")
  uCountryOfBirthCALD[x]
}
