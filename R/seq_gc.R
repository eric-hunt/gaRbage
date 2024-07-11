#' Get the GC content of a nucleic acid sequence
#'
#' @param seq A string - the nucleic acid sequence formatted as a single string
#' @param digits An integer - rounding precision for the result (see [base::round()])
#'
#' @return A double, the GC content of *seq*, with precision defined by *digits*
#' @export
seq_gc <- function(seq, digits = 4) {
  seq <- stringr::str_remove_all(seq, stringr::regex("\\s|\\W"))

  seq_gc <- stringr::str_count(seq, stringr::regex("G|C", ignore_case = TRUE))

  seq_len <- stringr::str_length(seq)

  base::round(seq_gc / seq_len, digits = digits)
}
