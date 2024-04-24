#' Convert DNA sequence to RNA sequence
#'
#' @param dna_sequence A character string representing a DNA sequence.
#' @return A character string representing the corresponding RNA sequence.
dna_to_rna <- function(dna_sequence) {
  rna_sequence <- gsub("T", "U", dna_sequence)
  return(rna_sequence)
}
