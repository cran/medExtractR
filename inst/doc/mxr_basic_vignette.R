## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)

## ---- eval = FALSE------------------------------------------------------------
#  note <- paste(scan(filename, '', sep = '\n', quiet = TRUE), collapse = '\n')
#  medExtractR(note, drug_names, unit, window_length, max_dist, ...)

## ----mxr, message = FALSE-----------------------------------------------------
library(medExtractR)

# tacrolimus note file names
tac_fn <- list(
  system.file("examples", "tacpid1_2008-06-26_note1_1.txt", package = "medExtractR"),
  system.file("examples", "tacpid1_2008-06-26_note2_1.txt", package = "medExtractR"),
  system.file("examples", "tacpid1_2008-12-16_note3_1.txt", package = "medExtractR")
)

# execute medExtractR
tac_mxr <- do.call(rbind, lapply(tac_fn, function(filename){
  tac_note <- paste(scan(filename, '', sep = '\n', quiet = TRUE), collapse = '\n')
  fn <- sub(".+/", "", filename)
  cbind("filename" = fn,
        medExtractR(note = tac_note,
             drug_names = c("tacrolimus", "prograf", "tac", "tacro", "fk", "fk506"),
             unit = "mg",
             window_length = 60,
             max_dist = 2,
             lastdose=TRUE))
}))

# lamotrigine note file name
lam_fn <- c(
  system.file("examples", "lampid1_2016-02-05_note4_1.txt", package = "medExtractR"),
  system.file("examples", "lampid1_2016-02-05_note5_1.txt", package = "medExtractR"),
  system.file("examples", "lampid2_2008-07-20_note6_1.txt", package = "medExtractR"),
  system.file("examples", "lampid2_2012-04-15_note7_1.txt", package = "medExtractR")
)

# execute medExtractR
lam_mxr <- do.call(rbind, lapply(lam_fn, function(filename){
  lam_note <- paste(scan(filename, '', sep = '\n', quiet = TRUE), collapse = '\n')
  fn <- sub(".+/", "", filename)
  cbind("filename" = fn,
        medExtractR(note = lam_note,
              drug_names = c("lamotrigine", "lamotrigine XR", 
                            "lamictal", "lamictal XR", 
                            "LTG", "LTG XR"),
              unit = "mg",
              window_length = 130,
              max_dist = 1,
              strength_sep="-"))
}))

## ---- echo = FALSE------------------------------------------------------------
# Print output
message("tacrolimus `medExtractR` output:\n")
tac_mxr
message("lamotrigine `medExtractR` output:\n")
lam_mxr

## ---- echo = FALSE------------------------------------------------------------
ann <- read.delim(system.file("mxr_tune", "ann_example.ann", package = "medExtractR"), 
                            header = FALSE, sep = "\t", stringsAsFactors = FALSE, 
                            col.names = c("id", "entity", "annotation"))
head(ann)

## -----------------------------------------------------------------------------
# Read in the annotations - might be specific to annotation method/software
ann_filenames <- list(system.file("mxr_tune", "tune_note1.ann", package = "medExtractR"),
                      system.file("mxr_tune", "tune_note2.ann", package = "medExtractR"),
                      system.file("mxr_tune", "tune_note3.ann", package = "medExtractR"))

tune_ann <- do.call(rbind, lapply(ann_filenames, function(fn){
  annotations <- read.delim(fn, 
                            header = FALSE, sep = "\t", stringsAsFactors = FALSE, 
                            col.names = c("id", "entity", "annotation"))
  
  # Label with file name
  annotations$filename <- sub(".ann", ".txt", sub(".+/", "", fn), fixed=TRUE)
  
  # Separate entity information into entity label and start:stop position
  # Format is "entity start stop"
  ent_info <- strsplit(as.character(annotations$entity), split="\\s")
  annotations$entity <- unlist(lapply(ent_info, '[[', 1))
  annotations$pos <- paste(lapply(ent_info, '[[', 2), 
                           lapply(ent_info, '[[', 3), sep=":")
  
  annotations <- annotations[,c("filename", "entity", "annotation", "pos")]
  
  return(annotations)
}))
head(tune_ann)

## ----run_mxr, cache = TRUE----------------------------------------------------
wind_len <- seq(30, 120, 30)
max_edit <- seq(0, 2, 1)
tune_pick <- expand.grid("window_length" = wind_len, 
                         "max_edit_distance" = max_edit)
# Run the Extract-Med module on the tuning notes
note_filenames <- list(system.file("mxr_tune", "tune_note1.txt", package = "medExtractR"),
                       system.file("mxr_tune", "tune_note2.txt", package = "medExtractR"),
                       system.file("mxr_tune", "tune_note3.txt", package = "medExtractR"))

# List to store output for each parameter combination
mxr_tune <- vector(mode="list", length=nrow(tune_pick))
for(i in 1:nrow(tune_pick)){
  
  mxr_tune[[i]] <- do.call(rbind, lapply(note_filenames, function(filename){
    tune_note <- paste(scan(filename, '', sep = '\n', quiet = TRUE), collapse = '\n')
    fn <- sub(".+/", "", filename)
    cbind("filename" = fn,
          medExtractR(note = tune_note,
                      drug_names = c("tacrolimus", "prograf", "tac", "tacro", "fk", "fk506"),
                      unit = "mg",
                      window_length = tune_pick$window_length[i],
                      max_dist = tune_pick$max_edit_distance[i]))
  }))

}


## ---- echo=FALSE--------------------------------------------------------------
# Functions to compute true positive, false positive, and false negatives
# number of true positives - how many annotations were correctly identified by medExtractR
Tpos <- function(df){
  sum(df$annotation == df$expr, na.rm=TRUE)
}
# number of false positive (identified by medExtractR but not annotated)
Fpos <- function(df){
  sum(is.na(df$annotation))
}
# number of false negatives (annotated but not identified by medExtractR)
Fneg <- function(df){
  # keep only rows with annotation
  df_ann <- subset(df, !is.na(annotation))
  sum(is.na(df$expr))
}
prf <- function(df){
  tp <- Tpos(df)
  fp <- Fpos(df)
  fn <- Fneg(df)
  
  precision <- tp/(tp + fp)
  recall <- tp/(tp + fn) 
  f1 <- (2*precision*recall)/(precision + recall)
  
  return(f1)
}

tune_pick$F1 <- sapply(mxr_tune, function(x) {
  y <- merge(x, tune_ann, by = c("filename", "entity", "pos"), all = TRUE)
  compare <- y[order(as.numeric(gsub(":.+", "", y[,'pos']))),]
  prf(compare)
})
ggplot(tune_pick) + geom_point(aes(max_edit_distance, window_length, size = F1)) + 
  scale_y_continuous(breaks=seq(30,120,30)) + 
  annotate("text", x = tune_pick$max_edit_distance+.2, y = tune_pick$window_length,
           label = round(tune_pick$F1, 2)) + 
  ggtitle("F1 for tuning parameter values")

