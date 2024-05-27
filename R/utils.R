#' @importFrom stringr str_replace_all fixed
replace_bslash <- function(chr) {
  str_replace_all(chr, fixed("/"), "-")
}
