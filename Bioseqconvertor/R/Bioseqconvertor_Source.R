#' Define S3 class for DNA sequences
#'
#' @param sequence A character string representing a DNA sequence.
#' @export
DNA <- function(sequence) {
  structure(sequence, class = "DNA")
}

#' Convert DNA to RNA
#'
#' @param dna_sequence A character string representing a DNA sequence.
#' @return A character string representing the corresponding RNA sequence.
#' @export
dna_to_rna <- function(dna_sequence) {
  rna_sequence <- gsub("T", "U", dna_sequence)
  return(rna_sequence)
}

#' Convert RNA to Protein
#'
#' @param rna_sequence A character string representing an RNA sequence.
#' @return A character string representing the corresponding protein sequence.
#' @export
rna_to_protein <- function(rna_sequence) {
  # Define codon table
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

  # Convert RNA sequence to uppercase
  rna_sequence <- toupper(rna_sequence)

  # Check if the length of the RNA sequence is divisible by 3
  if (nchar(rna_sequence) %% 3 != 0) {
    stop("RNA sequence length must be divisible by 3.")
  }

  # Initialize an empty vector to store the protein sequence
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

#' Convert Protein to cDNA
#'
#' @param protein_seq A character string representing a protein sequence.
#' @return A character string representing the corresponding cDNA sequence.
#' @export
protein_to_cdna <- function(protein_seq) {
  # Define the standard genetic code
  genetic_code <- list(
    'A' = c('GCT', 'GCC', 'GCA', 'GCG'),
    'R' = c('CGT', 'CGC', 'CGA', 'CGG', 'AGA', 'AGG'),
    'N' = c('AAT', 'AAC'),
    'D' = c('GAT', 'GAC'),
    'C' = c('TGT', 'TGC'),
    'Q' = c('CAA', 'CAG'),
    'E' = c('GAA', 'GAG'),
    'G' = c('GGT', 'GGC', 'GGA', 'GGG'),
    'H' = c('CAT', 'CAC'),
    'I' = c('ATT', 'ATC', 'ATA'),
    'L' = c('TTA', 'TTG', 'CTT', 'CTC', 'CTA', 'CTG'),
    'K' = c('AAA', 'AAG'),
    'M' = c('ATG'),
    'F' = c('TTT', 'TTC'),
    'P' = c('CCT', 'CCC', 'CCA', 'CCG'),
    'S' = c('TCT', 'TCC', 'TCA', 'TCG', 'AGT', 'AGC'),
    'T' = c('ACT', 'ACC', 'ACA', 'ACG'),
    'W' = c('TGG'),
    'Y' = c('TAT', 'TAC'),
    'V' = c('GTT', 'GTC', 'GTA', 'GTG'),
    '*' = c('TAA', 'TAG', 'TGA')
  )

  # Initialize an empty vector to store codons
  codon_seq <- character()

  # Iterate through each amino acid in the protein sequence
  for (aa in strsplit(protein_seq, '')[[1]]) {
    # Find the corresponding codons for the amino acid
    codons <- genetic_code[[aa]]

    # Choose a random codon (if multiple exist for the same amino acid)
    selected_codon <- sample(codons, 1)

    # Append the selected codon to the codon sequence
    codon_seq <- c(codon_seq, selected_codon)
  }

  # Concatenate codons to form cDNA sequence
  cdna_seq <- paste(codon_seq, collapse = '')

  return(cdna_seq)
}
