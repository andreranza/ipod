replace_bslash <- function(chr) {
  stringr::str_replace_all(chr, stringr::fixed("/"), "-")
}
