#' Convert protein sequence to cDNA sequence
#'
#' @param protein_seq A character string representing a protein sequence.
#' @return A character string representing the corresponding cDNA sequence.
protein_to_cdna <- function(protein_seq) {
  #The standard genetic code table/Codons
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
