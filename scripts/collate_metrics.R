#!/usr/bin/env Rscript
# ============================================================
# collate_metrics.R
#
# Reads WebMetrics files listed in sources.csv, attaches network info (covariates)
# from webinfo.xlsx, and writes one combined table.
#
# Usage:
#   Rscript scripts/collate_metrics.R \
#     --sources sources.csv \
#     --webinfo webinfo.xlsx \
#     --out MetricsComparisons/CompleteMetrics.csv
#
# Options:
#   --sheet NAME        input webinfo.xlsx sheet name    (default Sheet1)
#   --in-paper-only     skip sources with in_paper = FALSE
#   --strict            missing files become errors, not warnings
#
# ============================================================

suppressPackageStartupMessages(library(readxl))

## ---- argument parsing --------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
opt_val <- function(flag, default = NA_character_) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
opt_flag <- function(flag) flag %in% args

f_sources <- opt_val("--sources", "sources.csv")
f_webinfo <- opt_val("--webinfo", "webinfo.xlsx")
sheet     <- opt_val("--sheet",   "Sheet1")
f_out     <- opt_val("--out",     "MetricsComparisons/CompleteMetrics.csv")
in_paper_only <- opt_flag("--in-paper-only")
strict        <- opt_flag("--strict")

## ---- helpers -----------------------------------------------------------
# Treat "NA", "" and whitespace as missing. Excel writes all three.
blank_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "?")] <- NA
  x
}
say <- function(...) cat(..., "\n", sep = "")

## ---- read sources ------------------------------------------------------
if (!file.exists(f_sources)) stop("sources file not found: ", f_sources, call. = FALSE)
sources <- read.csv(f_sources, stringsAsFactors = FALSE, comment.char = "#")

need <- c("source_id", "metrics_file", "row_type", "webinfo_key", "in_paper")
missing_cols <- setdiff(need, names(sources))
if (length(missing_cols)) {
  stop("sources.csv missing column(s): ", paste(missing_cols, collapse = ", "),
       call. = FALSE)
}

# Columns carried from sources onto every row of that source's metrics.
carry <- setdiff(names(sources), c("metrics_file", "notes"))

sources$webinfo_key <- blank_to_na(sources$webinfo_key)
sources$in_paper    <- toupper(trimws(as.character(sources$in_paper))) %in% c("TRUE", "T", "YES", "1")

if (anyDuplicated(sources$source_id)) {
  stop("Duplicate source_id in ", f_sources, ": ",
       paste(unique(sources$source_id[duplicated(sources$source_id)]), collapse = ", "),
       call. = FALSE)
}

bad_type <- setdiff(unique(sources$row_type), c("replicate", "web"))
if (length(bad_type)) {
  stop("Invalid row_type value(s): ", paste(bad_type, collapse = ", "),
       " (expected 'replicate' or 'web')", call. = FALSE)
}

if (in_paper_only) {
  n0 <- nrow(sources)
  sources <- sources[sources$in_paper, , drop = FALSE]
  say("Filtered to in_paper sources: ", nrow(sources), " of ", n0)
}

## ---- read webinfo ------------------------------------------------------
if (!file.exists(f_webinfo)) stop("webinfo file not found: ", f_webinfo, call. = FALSE)
webinfo <- as.data.frame(read_excel(f_webinfo, sheet = sheet), stringsAsFactors = FALSE)

if (!"sln_id" %in% names(webinfo)) {
  stop("webinfo sheet '", sheet, "' has no `sln_id` column.", call. = FALSE)
}
webinfo$sln_id <- blank_to_na(webinfo$sln_id)

if (anyNA(webinfo$sln_id)) {
  warning(sum(is.na(webinfo$sln_id)), " webinfo row(s) have a blank sln_id; dropped.")
  webinfo <- webinfo[!is.na(webinfo$sln_id), ]
}
if (anyDuplicated(webinfo$sln_id)) {
  stop("Duplicate sln_id in webinfo: ",
       paste(unique(webinfo$sln_id[duplicated(webinfo$sln_id)]), collapse = ", "),
       call. = FALSE)
}
say("webinfo: ", nrow(webinfo), " rows, ", ncol(webinfo), " columns")

# Check every non-NA webinfo_key resolves, before reading any metrics files.
keys <- unique(na.omit(sources$webinfo_key))
unresolved <- setdiff(keys, webinfo$sln_id)
if (length(unresolved)) {
  stop("webinfo_key value(s) in sources.csv not found in webinfo sln_id:\n  ",
       paste(unresolved, collapse = "\n  "), call. = FALSE)
}

## ---- read each metrics file -------------------------------------------
frames  <- list()
absent  <- character(0)
metric_cols_ref <- NULL

for (i in seq_len(nrow(sources))) {
  src    <- sources[i, ]
  path <- src$metrics_file

  if (!file.exists(path)) {
    absent <- c(absent, sprintf("%s  ->  %s%s", src$source_id, path,
                                if (src$in_paper) "  [in_paper]" else ""))
    next
  }

  d <- read.csv(path, stringsAsFactors = FALSE)
  if (!nrow(d)) { warning("Empty metrics file, skipped: ", path); next }
  if (!"SLN_ID" %in% names(d)) {
    stop("No SLN_ID column in ", path, call. = FALSE)
  }

  # Track metric column names to catch files built by different versions of WebMetrics script.
  mc <- sort(setdiff(names(d), "SLN_ID"))
  if (is.null(metric_cols_ref)) {
    metric_cols_ref <- mc
  } else if (!identical(mc, metric_cols_ref)) {
    warning(sprintf("Metric columns differ in %s | missing: %s | extra: %s",
                    path,
                    paste(setdiff(metric_cols_ref, mc), collapse = ","),
                    paste(setdiff(mc, metric_cols_ref), collapse = ",")))
  }

  # SLN_ID means different things by row_type, so give it two clear names.
  d$replicate <- if (src$row_type == "replicate") d$SLN_ID else NA
  d$web_id    <- if (src$row_type == "web") as.character(d$SLN_ID) else src$webinfo_key

  # Carry the source-level columns.
  for (cn in carry) d[[cn]] <- src[[cn]]

  frames[[length(frames) + 1]] <- d
}

if (!length(frames)) stop("No metrics files could be read.", call. = FALSE)

## ---- report missing ----------------------------------------------------
if (length(absent)) {
  say("\n", strrep("-", 60))
  say(length(absent), " metrics file(s) not found:")
  say(paste0("  ", absent, collapse = "\n"))
  say(strrep("-", 60), "\n")
  n_paper <- sum(grepl("\\[in_paper\\]", absent))
  if (strict) {
    stop(length(absent), " metrics file(s) missing (--strict).", call. = FALSE)
  }
  if (n_paper) {
    warning(n_paper, " missing file(s) are flagged in_paper = TRUE. ",
            "The output is incomplete until they are generated.")
  }
}

## ---- stack -------------------------------------------------------------
all_cols <- unique(unlist(lapply(frames, names)))
frames <- lapply(frames, function(d) {
  for (cn in setdiff(all_cols, names(d))) d[[cn]] <- NA
  d[, all_cols, drop = FALSE]
})
combined <- do.call(rbind, frames)
say("Stacked ", nrow(combined), " rows from ", length(frames), " file(s)")

## ---- join covariates ---------------------------------------------------
# One join for both row types: web_id already holds the right key
# (the per-row SLN_ID for web files, the source's webinfo_key otherwise).
combined$web_id <- blank_to_na(combined$web_id)

matched   <- combined$web_id %in% webinfo$sln_id
unmatched <- sort(unique(combined$web_id[!matched & !is.na(combined$web_id)]))

if (length(unmatched)) {
  say("\nweb_id value(s) with no webinfo row (covariates will be NA):")
  say(paste0("  ", unmatched, collapse = "\n"))
  say("")
}

clash <- setdiff(intersect(names(webinfo), names(combined)), "sln_id")
if (length(clash)) {
  say("Note: webinfo column(s) also present in metrics, suffixed .webinfo: ",
      paste(clash, collapse = ", "))
  names(webinfo)[match(clash, names(webinfo))] <- paste0(clash, ".webinfo")
}

combined <- merge(combined, webinfo, by.x = "web_id", by.y = "sln_id",
                  all.x = TRUE, sort = FALSE)

unused <- setdiff(webinfo$sln_id, combined$web_id)
if (length(unused)) {
  say("webinfo row(s) matching no data (expected if those files are missing): ",
      length(unused))
}

## ---- uniqueness check --------------------------------------------------
# Every row must be identifiable by dataset_id + replicate. A failure here
# means two sources produced colliding IDs.
kdup <- duplicated(combined[, c("source_id", "web_id", "replicate")])
if (any(kdup)) {
  offenders <- unique(paste(combined$source_id[kdup], combined$web_id[kdup]))
  stop(sum(kdup), " duplicate source_id/web_id/replicate row(s): ",
       paste(head(offenders, 10), collapse = ", "), call. = FALSE)
}
## ---- tidy and write ----------------------------------------------------

lead <- intersect(c("source_id", "web_id", "replicate", "row_type",
                    "rhynie_habitat", "rhynie_lumping", "TS_lumped",
                    "in_paper", "dataset"), names(combined))
combined <- combined[, c(lead, setdiff(names(combined), lead)), drop = FALSE]
GROUP_ORDER <- c("small", "ecoweb", "digel", "rhynie", "niche")

if ("sort_group" %in% names(combined)) {
  unknown <- setdiff(unique(na.omit(combined$sort_group)), GROUP_ORDER)
  if (length(unknown)) {
    warning("sort_group value(s) not in GROUP_ORDER, sorted last: ",
            paste(unknown, collapse = ", "))
  }
  grp <- factor(combined$sort_group, levels = GROUP_ORDER)
} else {
  grp <- factor(rep("x", nrow(combined)))
}

combined <- combined[order(grp, combined$source_id, combined$web_id,
                           combined$replicate, na.last = TRUE), ]

if (dirname(f_out) != ".") {
  dir.create(dirname(f_out), recursive = TRUE, showWarnings = FALSE)
}
write.csv(combined, f_out, row.names = FALSE, na = "NA")

say("\nWrote ", f_out, ": ", nrow(combined), " rows x ", ncol(combined), " columns")
say("Analysis units: ", nrow(unique(combined[, c("source_id", "web_id")])))
print(table(combined$row_type, useNA = "ifany"))
