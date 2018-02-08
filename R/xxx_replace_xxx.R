#' replace patterns (vector) with replacements (vector) in string or string vector.
#' uses gsub and perl=TRUE
#' taken from \url{https://stackoverflow.com/questions/26676045}
#
#' @export
#' @param string string or vector of strings to replace in
#' @param patterns pattern or vector of patterns to replace
#' @param replacements replacements string, needs to have same length as replacment
xxx_replace_xxx <- function(string, patterns, replacements) {
  for (i in seq_along(patterns))
    string <- gsub(patterns[i], replacements[i], string, perl=TRUE)
  string
}
