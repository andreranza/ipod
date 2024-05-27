#' @export
get_meta_data <- function(path) {
  out <- system(sprintf("mdls %s", path), intern = TRUE)
  paste0(out, collapse = " ")
}

#' @export
#' @importFrom stringr str_extract str_replace_all fixed
get_author <- function(chr) {

  out <- str_extract(
    chr,
    "(?<=kMDItemAuthors).+(?=kMDItemComposer)"
  )

  if (is.na(out)) {
    out <- str_extract(
      chr,
      "(?<=kMDItemAuthors).+(?=kMDItemContentCreationDate\\b)"
    )
  }

  out <- str_extract(out, "(?<=\\().+(?=\\))")
  out <- clean_str(out)

  str_replace_all( # TODO: more general
    out,
    fixed("\\U0301"),
    fixed("\u0301")
  )

}

#' @export
#' @importFrom stringr str_extract
get_album <- function(chr) {

  out <- str_extract(
    chr,
    "(?<=kMDItemAlbum).+(?=kMDItemAlternateNames)"
  )

  out <- clean_str(out)

  out

}

#' @export
#' @importFrom stringr str_extract
get_track_title <- function(chr) {

  out <- str_extract(
    chr,
    "(?<=kMDItemDisplayName).+(?=kMDItemDocumentIdentifier)"
  )

  clean_str(out)
}

#' @export
#' @importFrom stringr str_extract
get_track_no <- function(chr) {

  out <- str_extract(
    chr,
    "(?<=kMDItemAudioTrackNumber).+(?=kMDItemAuthors)"
  )

  if (is.na(out)) {

    out <- "1"

  }

  clean_str(out)

}

#' @export
#' @importFrom stringr str_extract
get_genre <- function(chr) {

  out <- str_extract(
    chr,
    "(?<=kMDItemMusicalGenre).+(?=kMDItemPhysicalSize)"
  )

  clean_str(out)

}

#' @export
#' @importFrom stringr str_extract str_remove_all str_squish
get_id <- function(chr) {

  out <- str_extract(
    chr,
    "(?<=kMDItemAlternateNames).+(?=kMDItemAudioBitRate)"
  )

  out <- str_remove_all(out, "\\(|\\)")
  out <- str_remove_all(out, "\\\"")
  out <- str_remove_all(out, "=")
  out <- str_squish(out)

  out

}

#' @export
#' @importFrom stringr str_extract
get_composer <- function(chr) {

  out <- str_extract(
    chr,
    "(?<=kMDItemComposer).+(?=kMDItemContentCreationDate\\b)"
  )

  clean_str(out)

}

#' @importFrom stringr str_remove_all str_squish
clean_str <- function(chr) {

  out <- str_remove_all(chr, "\\\"")
  out <- str_remove_all(out, "=")
  # out <- stringr::str_remove_all(out, "\\(|\\)")
  out <- str_squish(out)

  out

}


