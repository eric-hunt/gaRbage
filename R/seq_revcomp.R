#' Get the reverse complement of a nucleic acid sequence
#'
#' @param seq A string - the nucleic acid sequence formatted as a single string
#'
#' @return A string - the reverse complement of *seq*
#' @export
seq_revcomp <- function(seq) {
  seq <- stringr::str_remove_all(seq, stringr::regex("\\s|\\W"))

  seq_split <- lapply(
    strsplit(toupper(seq), ""),
    rev
  )

  paste(
    unlist(
      lapply(
        seq_split,
        function(x) chartr("ATUGC", "TAACG", x)
      )
    ),
    collapse = ""
  )
}
