#!/usr/bin/env Rscript
# ============================================================
# metaweb_builder_rhynie.R
#
# Builds guild-level metawebs from RhynieGuildStructure.xlsx, one
# folder per resolution x habitat variant:
#   <out>/rhynie_unlumped_complete/{guilds.csv, links.csv, guild_matrix.csv}
#   <out>/rhynie_unlumped_terr/...
#   <out>/rhynie_lumped_aqu/...      etc.     
#
# Usage:
#   Rscript scripts/metaweb_builder_rhynie.R \
#     --workbook default: data/rhynie/RhynieGuildStructure.xlsx \
#     --out      default: data/rhynie \
#     --resolutions unlumped,lumped \
#     --habitats complete,terr,aqu
# ============================================================

# ---- argument parsing --------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
opt_val <- function(flag, default = NA_character_) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
opt_flag <- function(flag) flag %in% args

workbook  <- opt_val("--workbook", "data/rhynie/RhynieGuildStructure.xlsx")
output_path  <- opt_val("--out",  "data/rhynie")
resolutions <- strsplit(opt_val("--resolutions", "unlumped,lumped"), ",")[[1]]
habitats <- strsplit(opt_val("--habitats", "complete,terr,aqu"), ",")[[1]]

stopifnot(all(resolutions %in% c("unlumped", "lumped")))
stopifnot(all(habitats %in% c("complete", "terr", "aqu")))
if (!file.exists(workbook)) stop("Workbook not found: ", workbook, call. = FALSE)

suppressPackageStartupMessages(library(readxl))
guilds <- as.data.frame(read_excel(workbook, sheet = "Guilds"))
lp     <- as.data.frame(read_excel(workbook, sheet = "Lumped priority"))

# Parse one comma-separated ID string. Blank, "-", "NA" --> "none".
parse_ids <- function(s) {
  s <- trimws(as.character(s))
  if (is.na(s) || s %in% c("", "-", "NA")) return(integer(0))
  out <- suppressWarnings(as.integer(strsplit(s, ",")[[1]]))
  if (anyNA(out)) stop("Non-integer in resource list: '", s, "'", call. = FALSE)
  out
}

# The schema every variant writes. og_guild_ids is appended for lumped and subset versions
STD_COLS <- c("guild_no", "guild_name", "major_taxa", "G",
              "priority_resources", "general_resources", "terr", "aqu", "animal")

# ---- make the full links table --------------------------------------------------

P_COL <- "priority_resources"
G_COL <- "general_resources"

# Wide -> long. One row per (consumer, resource, priority).
links <- do.call(rbind, lapply(seq_len(nrow(guilds)), function(i) {
  cons <- guilds$guild_no[i]
  p    <- parse_ids(guilds[[P_COL]][i])
  g    <- parse_ids(guilds[[G_COL]][i])
  both <- intersect(p, g)
  if (length(both)) stop("Guild ", cons, " lists resource(s) in both columns: ",
                         paste(both, collapse = ","), call. = FALSE)
  rbind(
    if (length(p)) data.frame(consumer = cons, resource = p, priority.level = 2L),
    if (length(g)) data.frame(consumer = cons, resource = g, priority.level = 1L)
  )
}))

#### ---- integrity checks --------------------------------------------------

problems <- character(0)
notes    <- character(0)

#    Every link ID must name a real guild. Catches NA, 0, negatives, and
#    numbers above the guild count
known <- guilds$guild_no
bad_c <- which(!(links$consumer %in% known))
bad_r <- which(!(links$resource %in% known))
if (length(bad_c)) problems <- c(problems, paste0(
  length(bad_c), " link(s) whose consumer is not a guild (rows ",
  paste(head(bad_c, 10), collapse = ", "), ")"))
if (length(bad_r)) problems <- c(problems, paste0(
  length(bad_r), " link(s) whose resource is not a guild (rows ",
  paste(head(bad_r, 10), collapse = ", "), ")"))

#    Priority values: only 1 or 2 are accepted
bad_p <- which(!(links$priority.level %in% c(1, 2)))
if (length(bad_p)) problems <- c(problems, paste0(
  length(bad_p), " link(s) with priority not in {1, 2} (rows ",
  paste(head(bad_p, 10), collapse = ", "), ")"))

#    Duplicate consumer/resource pairs. The matrix assignment algorithm retains
#    the last one, so a duplicate with two different priorities is a coin-flip.
key <- paste(links$consumer, links$resource)
if (anyDuplicated(key)) {
  dupes <- unique(key[duplicated(key)])
  problems <- c(problems, paste0(
    length(dupes), " duplicated consumer/resource pair(s): ",
    paste(head(dupes, 10), collapse = "; ")))
}

if (length(problems)) {
  stop("links.csv failed integrity checks:\n  ",
       paste(problems, collapse = "\n  "), call. = FALSE)
}

#    Guild(s) absent from links produces a warning: basal
#    resources legitimately have no prey, but a guild that is neither eaten
#    nor eats is probably an error.
present_links   <- unique(c(links$consumer, links$resource))
unlinked   <- setdiff(known, present_links)
if (length(unlinked)) {
  warning("Guild(s) not appearing in any links: ",
          paste(guilds$guild_name[guilds$guild_no %in% unlinked],
                collapse = "; "), call. = FALSE)
}

# ---- lumping -----------------------------------------------------------

# makes a taxonomically-lumped version of the guilds and links dataframes
# g is the guilds sheet, l is the links table, lp contains priority overrides
# lumped A -> B exists if any member pair had a link. Priority
# defaults to 1 for resources of lumped guilds; priority-2 pairs are 
# listed explicitly in the "Lumped priority" sheet, with a note giving reasoning.
lump_metaweb <- function(g, l, lp) {
  
  lg <- unique(g[, c("lumped_id", "lumped_name", "lumped_G")])
  if (anyDuplicated(lg$lumped_id))
    stop("lumped_name or lumped_G disagree within a lumped_id", call. = FALSE)
  lg <- lg[order(lg$lumped_id), ]
  
  # Habitat and animal flags: 1 if any member is
  # og_guild_ids: original guild IDs making up each lumped guild.
  hab     <- aggregate(cbind(terr, aqu, animal) ~ lumped_id, data = g, FUN = max)
  members <- aggregate(guild_no ~ lumped_id, data = g,
                       FUN = function(x) paste(sort(x), collapse = ","))
  names(members)[2] <- "og_guild_ids"
  
  lg <- merge(lg, hab,     by = "lumped_id", all.x = TRUE, sort = FALSE)
  lg <- merge(lg, members, by = "lumped_id", all.x = TRUE, sort = FALSE)
  lg <- lg[order(lg$lumped_id), ]
  
  to_lumped <- setNames(g$lumped_id, g$guild_no)   # original -> lumped
  
  # Effective priority before aggregation. A lumped consumer loses its
  # specialized links; resource-side lumping keeps priority, so the highest 
  # surviving link wins.
  n_members <- table(g$lumped_id) # table of how many guilds in each lumped guild
  lc  <- unname(to_lumped[as.character(l$consumer)]) # converted consumer IDs
  lr  <- unname(to_lumped[as.character(l$resource)]) # converted resource IDs
  eff <- ifelse(n_members[as.character(lc)] > 1, 1L, l$priority.level) # for lumped consumers, reset default link priority to 1  
  
  agg <- aggregate(list(priority.level = eff),
                   by = list(consumer = lc, resource = lr), FUN = max)
  lumped_links <- agg[order(agg$consumer, agg$resource), ]
  
  # Overrides reference lumped IDs from the complete web. In a habitat
  # subset some of those guilds are absent, so their overrides are inapplicable. 
  # Drop those first; anything left must still match a real link, 
  # which is what the orphan check below is for.
  lp <- lp[lp$consumer_lumped_id %in% lg$lumped_id &
             lp$resource_lumped_id %in% lg$lumped_id, , drop = FALSE]

  if (nrow(lp)) {
    key_all <- paste(lumped_links$consumer, lumped_links$resource)
    key_ovr <- paste(lp$consumer_lumped_id, lp$resource_lumped_id)
    
    orphan <- setdiff(key_ovr, key_all)
    if (length(orphan))
      stop("Override given for pair(s) with no preexisting link: ",
           paste(orphan, collapse = "; "), call. = FALSE)
    if (!all(lp$priority %in% c(1, 2)))
      stop("Override priority must be 1 or 2", call. = FALSE)
    
    m       <- match(key_all, key_ovr)
    changed <- !is.na(m) & lumped_links$priority.level != lp$priority[m]
    lumped_links$priority.level[!is.na(m)] <- as.integer(lp$priority[m[!is.na(m)]])
    
    message(sum(changed), " of ", nrow(lp), " override(s) changed a derived value")
  }
  
  # Self-links created by lumping ---------------------------------------
  # Kept only if a member guild ate itself in the unlumped web AND that
  # guild had G == 1, i.e. the link was already a species-level self-loop.
  # An original self-link on a G > 1 guild meant predation among distinct
  # species, so collapsing it onto one node manufactures a new self-loop.
  
  g_of      <- setNames(lg$lumped_G, lg$lumped_id)      # lumped id -> richness
  g1        <- g$guild_no[g$G == 1]
  self_orig <- unique(unname(to_lumped[
    as.character(intersect(l$consumer[l$consumer == l$resource], g1))]))
  
  cid       <- lumped_links$consumer
  drop_rows <- (cid == lumped_links$resource) &
    (unname(g_of[as.character(cid)]) == 1) &
    !(cid %in% self_orig)
  
  if (any(drop_rows)) {
    message("  removed ", sum(drop_rows),
            " self-link(s) created by lumping: ",
            paste(lg$lumped_name[match(cid[drop_rows], lg$lumped_id)],
                  collapse = "; "))
    lumped_links <- lumped_links[!drop_rows, , drop = FALSE]
  }
  
  # standardize names for the writer function
  lg$guild_no   <- lg$lumped_id
  lg$guild_name <- lg$lumped_name
  lg$G          <- lg$lumped_G
  lg$major_taxa <- NA_character_
  
  return(list(guilds = lg, links = lumped_links))
}

# ---- Habitat subset ---------------------------------------------------------
# Keeps original IDs; renumbering happens later
subset_habitat <- function(g, l, habitat) {
  if (habitat == "complete") return(list(guilds = g, links = l))
  keep <- g$guild_no[g[[habitat]] == 1]
  list(
    guilds = g[g$guild_no %in% keep, , drop = FALSE],
    links  = l[l$consumer %in% keep & l$resource %in% keep, , drop = FALSE]
  )
}
# ---- Renumber IDs --------------------------------------------------------
# Close gaps in guild IDs so they run 1..n.
renumber_ids <- function(g, l) {
  old <- sort(unique(g$guild_no))
  map <- setNames(seq_along(old), old)
  g$guild_no  <- unname(map[as.character(g$guild_no)])
  l$consumer  <- unname(map[as.character(l$consumer)])
  l$resource  <- unname(map[as.character(l$resource)])
  g <- g[order(g$guild_no), , drop = FALSE]
  l <- l[order(l$consumer, l$resource), , drop = FALSE]
  list(guilds = g, links = l)
}

# Build adjacency matrix ----------------------------------------------------
build_matrix <- function(l, n) {
  m <- matrix(0L, n, n)
  m[cbind(l$consumer, l$resource)] <- as.integer(l$priority.level)
  m
}

#---- Refresh resource lists in guilds sheet -----------------------------------
# Rebuild the two resource-list columns from links, so guilds.csv is
# internally consistent after subsetting/renumbering
refresh_resource_cols <- function(g, l) {
  fmt <- function(gid, pr) {
    ids <- sort(l$resource[l$consumer == gid & l$priority.level == pr])
    if (!length(ids)) NA_character_ else paste(ids, collapse = ",")
  }
  g$priority_resources <- vapply(g$guild_no, fmt, character(1), pr = 2L)
  g$general_resources  <- vapply(g$guild_no, fmt, character(1), pr = 1L)
  g
}

# ---- writer function to output files ----------------------------------------
write_metaweb <- function(g_full, l_full, lp, resolution, habitat) {
  
  tag <- sprintf("rhynie_%s_%s", resolution, habitat) #for folder naming later
  message("Building ", tag)
  
  # habitat subset, on original guilds before lumping
  s <- subset_habitat(g_full, l_full, habitat)
  
  # lump if asked
  if (resolution == "lumped") {
    s <- lump_metaweb(s$guilds, s$links, lp)
  } else {
    s$guilds$og_guild_ids <- NA_character_
  }
  
  # renumber to 1..n
  s <- renumber_ids(s$guilds, s$links)
  g <- s$guilds; l <- s$links
  
  #    isolated-node check, per variant. Subsetting can strand a guild
  #    whose only partners were in the other habitat.
  isolated <- setdiff(g$guild_no, unique(c(l$consumer, l$resource)))
  if (length(isolated)) {
    warning(tag, ": guild(s) with no links after subsetting: ",
            paste(g$guild_name[g$guild_no %in% isolated], collapse = "; "),
            call. = FALSE)
  }
  
  #   write
  g   <- refresh_resource_cols(g, l)
  dir <- file.path(output_path, tag)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  
  keep_cols <- c(STD_COLS, if (resolution == "lumped") "og_guild_ids")
  write.csv(g[, keep_cols], file.path(dir, "guilds.csv"), row.names = FALSE)
  write.csv(l, file.path(dir, "links.csv"), row.names = FALSE)
  write.table(build_matrix(l, nrow(g)), file.path(dir, "guild_matrix.csv"),
              sep = ",", row.names = FALSE, col.names = FALSE)
  
  message(sprintf("  %d guilds, %d species, %d links",
                  nrow(g), sum(g$G), nrow(l)))
  invisible(NULL)
}

#### ---- run every requested combination -----------------------------------

for (res in resolutions) {
  for (hab in habitats) {
    write_metaweb(guilds, links, lp, res, hab)
  }
}