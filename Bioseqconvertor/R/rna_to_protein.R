#' Convert RNA sequence to protein sequence
#'
#' @param rna_sequence A character string representing an RNA sequence.
#' @return A character string representing the corresponding protein sequence.
rna_to_protein <- function(rna_sequence) {
  codon_table <- list(
    UUU = "F", UUC = "F", UUA = "L", UUG = "L",
    CUU = "L", CUC = "L", CUA = "L", CUG = "L",
    AUU = "I", AUC = "I", AUA = "I", AUG = "M",
    GUU = "V", GUC = "V", GUA = "V", GUG = "V",
    UCU = "S", UCC = "S", UCA = "S", UCG = "S",
    CCU = "P", CCC = "P", CCA = "P", CCG = "P",
    ACU = "T", ACC = "T", ACA = "T", ACG = "T",
    GCU = "A", GCC = "A", GCA = "A", GCG = "A",
    UAU = "Y", UAC = "Y", UAA = "*", UAG = "*",
    CAU = "H", CAC = "H", CAA = "Q", CAG = "Q",
    AAU = "N", AAC = "N", AAA = "K", AAG = "K",
    GAU = "D", GAC = "D", GAA = "E", GAG = "E",
    UGU = "C", UGC = "C", UGA = "*", UGG = "W",
    CGU = "R", CGC = "R", CGA = "R", CGG = "R",
    AGU = "S", AGC = "S", AGA = "R", AGG = "R",
    GGU = "G", GGC = "G", GGA = "G", GGG = "G"
  )

  # To convert RNA sequence to uppercase
  rna_sequence <- toupper(rna_sequence)

  # To check if the length of the RNA sequence is divisible by 3
  if (nchar(rna_sequence) %% 3 != 0) {
    stop("RNA sequence length must be divisible by 3.")
  }

  # Vector to store the protein sequence
  protein_sequence <- character()

  # Iterate over the RNA sequence by codons
  for (i in seq(1, nchar(rna_sequence), by = 3)) {
    codon <- substr(rna_sequence, i, i + 2)
    if (codon %in% names(codon_table)) {
      protein_sequence <- c(protein_sequence, codon_table[[codon]])
    } else {
      protein_sequence <- c(protein_sequence, "X")  # Unknown codon represented by 'X'
    }
  }

  # Combine the elements of the protein sequence vector into a single string
  protein_sequence <- paste(protein_sequence, collapse = "")

  return(protein_sequence)
}
