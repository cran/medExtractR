## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(R.options = list(width = 90))

## ---- eval = FALSE----------------------------------------------------------------------
#  note <- paste(scan(filename, '', sep = '\n', quiet = TRUE), collapse = '\n')
#  medExtractR(note, drug_names, unit, window_length, max_dist, ...)

## ---------------------------------------------------------------------------------------
library(medExtractR)
# note file names
fn <- c(
  system.file("examples", "tacpid1_2008-06-26_note1_1.txt", package = "medExtractR"),
  system.file("examples", "tacpid1_2008-06-26_note2_1.txt", package = "medExtractR"),
  system.file("examples", "tacpid1_2008-12-16_note3_1.txt", package = "medExtractR"),
  system.file("examples", "lampid1_2016-02-05_note4_1.txt", package = "medExtractR"),
  system.file("examples", "lampid1_2016-02-05_note5_1.txt", package = "medExtractR"),
  system.file("examples", "lampid2_2008-07-20_note6_1.txt", package = "medExtractR"),
  system.file("examples", "lampid2_2012-04-15_note7_1.txt", package = "medExtractR")
)
getNote <- function(x) paste(scan(x, '', sep = '\n', quiet = TRUE), collapse = '\n')
notes <- vapply(fn, getNote, character(1))

## ---------------------------------------------------------------------------------------
medExtractR(note = notes[7], drug_names = c("lamotrigine", "lamictal"),
  window_length = 130, unit = "mg", drug_list = "rxnorm")

## ---------------------------------------------------------------------------------------
data(rxnorm_druglist, package = 'medExtractR')
length(rxnorm_druglist)
head(rxnorm_druglist)

## ---------------------------------------------------------------------------------------
medExtractR(note = notes[7], drug_names = c("lamotrigine", "lamictal"),
  window_length = 130, unit = "mg", drug_list = rxnorm_druglist)

## ---------------------------------------------------------------------------------------
medExtractR(note = notes[7], drug_names = c("lamotrigine", "lamictal"),
  window_length = 130, unit = "mg", drug_list = NULL)

## ---------------------------------------------------------------------------------------
medExtractR(note = notes[7], drug_names = c("lamotrigine", "lamictal"),
  window_length = 130, unit = "mg", drug_list = 'lorazepam')

## ---------------------------------------------------------------------------------------
parallel::makeCluster(2, setup_strategy = "sequential")
drug_check <- string_occurs(rxnorm_druglist, notes)
names(drug_check)
lengths(drug_check)
fnd_drugs <- drug_check[['TRUE']] # or, drug_check[[1]]
fnd_drugs
medExtractR(note = notes[7], drug_names = c("lamotrigine", "lamictal"),
  window_length = 130, unit = "mg", drug_list = fnd_drugs)

## ---------------------------------------------------------------------------------------
sug_drugs <- string_suggestions(fnd_drugs, notes)
sug_drugs

## ---------------------------------------------------------------------------------------
all_drugs <- c(fnd_drugs, sug_drugs[,'suggestion'])
medExtractR(note = notes[7], drug_names = c("lamotrigine", "lamictal"),
  window_length = 130, unit = "mg", drug_list = all_drugs)

