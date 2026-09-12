#!/usr/bin/env Rscript
# ============================================================
# niche_model_generator.R (Williams & Martinez 2000,
# Allesina et al. 2008) via ATNr::create_niche_model(S, C),
# drawing S and C from an existing WebMetrics table.
#
# Usage:
#   Rscript scripts/niche_model_generator.R \
#     --in-dir e.g., SLNs/rhynie_unlumped_complete/ts \
#     --n-reps default 1000 --seed e.g., 20260906
#
# Reads  <in-dir>/WebMetrics_*.csv
# Writes <out-dir>/matrix_<rep>.csv
#        <out-dir>/speciesinfo_<rep>.csv
#        <out-dir>/niche_params.csv     one row recording this run
# Default out-dir: SLNs/niche_<resolution>/raw, resolution taken from
# the source dataset id.
# ===============================================

#### ---- argument parsing --------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
opt_val <- function(flag, default = NA_character_) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}

in_dir <- opt_val("--in-dir")
if (is.na(in_dir))  stop("--in-dir is required", call. = FALSE)
in_dir <- sub("/+$", "", in_dir)       # remove trailing /'s  
if (!dir.exists(in_dir)) stop("--in-dir is not a directory: ", in_dir, call. = FALSE)

# locate the source WebMetrics table
wm <- list.files(in_dir, pattern = "^WebMetrics_.*\\.csv$", full.names = TRUE)
if (length(wm) != 1L) {
  stop("Expected exactly one WebMetrics_*.csv in ", in_dir,
       "; found ", length(wm), call. = FALSE)
}

stage <- basename(in_dir)                       # "ts" or "raw"
dsid  <- basename(dirname(in_dir))       # e.g., "rhynie_unlumped_complete"

# resolution for the default output name
resolution <- if (grepl("_unlumped", dsid)) "unlumped" else
              if (grepl("_lumped",   dsid)) "lumped"   else
              stop("Cannot infer resolution from '", dsid,
                   "'. Pass --out-dir explicitly.", call. = FALSE)

out_dir   <- opt_val("--out-dir")
defaulted <- is.na(out_dir)
if (defaulted) out_dir <- file.path("SLNs", paste0("niche_", resolution), "raw")

if (!dir.exists(out_dir)) {
  message("Creating output folder: ", out_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

n_reps   <- as.integer(opt_val("--n-reps", "1000"))
seed_str <- opt_val("--seed")
if (!is.na(seed_str)) set.seed(as.integer(seed_str))

suppressPackageStartupMessages(library(ATNr))


#### ---- sample rhynie webs + generate niche webs ------------------------------------------------------

# create n niche models with C and S value drawn from a Rhynie web 
# and output them as csv's along w sp. info files

df <- read.csv(wm, stringsAsFactors = FALSE)
for (col in c("S", "C")) {
  if (!col %in% names(df)) stop("No '", col, "' column in ", wm, call. = FALSE)
}

message("Source: ", wm)
message("  ", nrow(df), " webs;  S ", min(df$S), "-", max(df$S),
        ",  C ", signif(min(df$C), 3), "-", signif(max(df$C), 3))
message("Writing ", n_reps, " niche webs to ", out_dir)

# paired sampling: draw a row, take that web's S and C together.
rows <- sample(nrow(df), n_reps, replace = TRUE)

# loop to generate n niche webs
for (i in seq_len(n_reps)) {
  S <- as.integer(df$S[rows[i]])
  C <- df$C[rows[i]]

  speciesinfo <- data.frame(sp_name = seq_len(S), guild = seq_len(S))
  
  # transpose so rows = consumers, matching the SLN convention
  nichewebmatrix <- t(as.matrix(create_niche_model(S, C)))
  
  write.csv(speciesinfo, file.path(out_dir, paste0("speciesinfo_", i, ".csv")),
            row.names = FALSE)
  write.table(nichewebmatrix, file.path(out_dir, paste0("matrix_", i, ".csv")),
              sep = ",", row.names = FALSE, col.names = FALSE)
  
  if (i %% 100 == 0) message("  wrote ", i, " / ", n_reps)
}

#### ---- record the run ----------------------------------------------------

write.csv(data.frame(
  source_file = wm,
  source_dsid = dsid,
  source_stage = stage,
  n_reps      = n_reps,
  seed        = if (is.na(seed_str)) NA_character_ else seed_str,
  S_min = min(df$S), S_max = max(df$S),
  C_min = min(df$C), C_max = max(df$C),
  sampling    = "paired rows"
), file.path(out_dir, "niche_params.csv"), row.names = FALSE)

message("Done: ", n_reps, " niche webs written to ", out_dir)