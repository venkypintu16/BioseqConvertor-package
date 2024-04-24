# Test DNA sequence
test_dna_sequence <- "ATCGATCGATCGATCGTC"

# Expected RNA sequence
expected_rna_sequence <- "AUCGAUCGAUCGAUCGUC"

# Expected protein sequence
expected_protein_sequence <- "IDRSIV"

# Expected cDNA sequence
expected_cdna_sequence <- "ATCGATCGATCGATCGTC"

# Store test data in a list
test_data <- list(
  test_dna_sequence = test_dna_sequence,
  expected_rna_sequence = expected_rna_sequence,
  expected_protein_sequence = expected_protein_sequence,
  expected_cdna_sequence = expected_cdna_sequence
)

# Validation steps.

library(Bioseqconvertor)

RNA = dna_to_rna(test_dna_sequence)

Protein = rna_to_protein(expected_rna_sequence)

CDNA = protein_to_cdna(Protein) # The output CDNA doesn't match with the test_DNA because,
# every amino acid has different combination of codons that code for them.

