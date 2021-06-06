## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----mxr_taper----------------------------------------------------------------
library(medExtractR)

# prednisone tapering note file name
pred_fn <- c(
  system.file("examples", "predpid1_2013-04-27_note1_1.txt", package = "medExtractR"),
  system.file("examples", "predpid2_2015-12-11_note1_1.txt", package = "medExtractR"),
  system.file("examples", "predpid3_2017-01-01_note1_1.txt", package = "medExtractR")
)

# execute medExtractR_tapering
pred_mxr <- do.call(rbind, lapply(pred_fn, function(filename){
  pred_note <- paste(scan(filename, '', sep = '\n', quiet = TRUE), collapse = '\n')
  fn <- sub(".+/", "", filename)
  cbind("filename" = fn,
        medExtractR_tapering(note = pred_note,
                             drug_names = c("prednisone", "pred"),
                             unit = "mg"))
}))

## -----------------------------------------------------------------------------
# Taper 1 schedule
subset(pred_mxr, filename == sub(".+/", "", pred_fn[1]))

## -----------------------------------------------------------------------------
# Taper 2 schedule
subset(pred_mxr, filename == sub(".+/", "", pred_fn[2]))

## -----------------------------------------------------------------------------
# Alternating schedule
subset(pred_mxr, filename == sub(".+/", "", pred_fn[3]))

## ---- eval = FALSE------------------------------------------------------------
#  note <- paste(scan(filename, '', sep = '\n', quiet = TRUE), collapse = '\n')
#  medExtractR_tapering(note, drug_names, unit, max_dist, ...)

