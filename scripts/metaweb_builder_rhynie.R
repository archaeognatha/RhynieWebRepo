#!/usr/bin/env Rscript
# ============================================================
# metaweb_builder_rhynie.R
#
# This script takes a single Excel spreadsheet defining Rhynie
# network structure as input and builds the following csv outputs:
#      guilds.csv                     
#      links.csv                      
#      guild_matrix.csv               
#      guilds_lumped.csv              
#      links_lumped.csv   (for transparency)
#      guild_matrix_lumped.csv        
#
# Usage:
#   Rscript scripts/metaweb_builder_rhynie.R \
#     --workbook data/rhynie/RhynieGuildStructure.xlsx \
#     --out data/rhynie \
#
# ============================================================

#### ---- argument parsing --------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
opt_val <- function(flag, default = NA_character_) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
opt_flag <- function(flag) flag %in% args

workbook  <- opt_val("--workbook", "data/rhynie/RhynieGuildStructure.xlsx")
output_path  <- opt_val("--out",  "data/Rhynie")

if (!file.exists(workbook)) stop("Workbook not found: ", workbook, call. = FALSE)
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

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

#### ---- output guilds csv --------------------------------------------------

export_cols <- c("guild_no", "guild_name", "major_taxa", "G", "priority_resources",
                 "general_resources", "terr", "aqu")
write.csv(guilds[, export_cols], file.path(output_path,"guilds.csv"), row.names = FALSE)

#### ---- make links table --------------------------------------------------

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

write.csv(links, file.path(output_path,"links.csv"), row.names = FALSE)

# Build adjacency matrix ----

# Find the highest guild ID to define the size of the square matrix.
num_guilds <- max(c(links$consumer, links$resource, guilds$guild_no), na.rm = TRUE)

if (max(guilds$guild_no) != nrow(guilds)) {
  stop("Guild IDs are not contiguous 1..n: ", nrow(guilds), " guilds, max ID ",
       max(guilds$guild_no), call. = FALSE)
}

# Initialize a square matrix with rows & columns equal to the number of guilds, populated with zeros
adj_matrix <- matrix(0, nrow = num_guilds, ncol = num_guilds)

# The 'links.csv' file has a 'priority.level' column.
# We can loop through the links and assign the value from that column.

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

#### ---- fill matrix -------------------------------------------------------
adj_matrix <- matrix(0L, nrow = num_guilds, ncol = num_guilds)
adj_matrix[cbind(links$consumer, links$resource)] <- links$priority.level

# Write the adjacency matrix to a new CSV file.
output_filename <- file.path(output_path, "guild_matrix.csv")
write.table(adj_matrix, output_filename, sep = ",", row.names = FALSE, col.names = FALSE)

# Build lumped adjacency matrix ----

# Membership and richness come from the guilds sheet. Link existence is
# derived: lumped A -> B exists if any member pair had a link. Priority
# defaults to 1 for resources of lumped guilds; priority-2 pairs are 
# listed explicitly in the "Lumped priority" sheet, with a note giving reasoning.

lg <- unique(guilds[, c("lumped_id", "lumped_name", "lumped_G")])
if (anyDuplicated(lg$lumped_id))
  stop("lumped_name or lumped_G disagree within a lumped_id", call. = FALSE)
lg <- lg[order(lg$lumped_id), ]
if (!identical(as.integer(lg$lumped_id), seq_len(nrow(lg))))
  stop("lumped_id must be contiguous 1..n", call. = FALSE)

# Habitat membership: present in a habitat if any member is
# og_guild_ids: original guild IDs making up each lumped guild.
hab     <- aggregate(cbind(terr, aqu) ~ lumped_id, data = guilds, FUN = max)
members <- aggregate(guild_no ~ lumped_id, data = guilds,
                     FUN = function(x) paste(sort(x), collapse = ","))
names(members)[2] <- "og_guild_ids"

lg <- merge(lg, hab,     by = "lumped_id", all.x = TRUE, sort = FALSE)
lg <- merge(lg, members, by = "lumped_id", all.x = TRUE, sort = FALSE)
lg <- lg[order(lg$lumped_id), ]

mixed <- lg$lumped_id[lg$terr == 1 & lg$aqu == 1]
if (length(mixed)) {
  message("Lumped guild(s) spanning both habitats: ",
          paste(lg$lumped_name[lg$lumped_id %in% mixed], collapse = "; "))
}

to_lumped <- setNames(guilds$lumped_id, guilds$guild_no)   # original -> lumped

# Effective priority before aggregation. A lumped consumer loses its
# specialized links; resource-side lumping keeps priority, so the highest 
# surviving link wins.
n_members <- table(guilds$lumped_id) # table of how many guilds in each lumped guild
lc  <- unname(to_lumped[as.character(links$consumer)]) # converted consumer IDs
lr  <- unname(to_lumped[as.character(links$resource)]) # converted resource IDs
eff <- ifelse(n_members[as.character(lc)] > 1, 1L, links$priority.level) # for lumped consumers, reset default link priority to 1  

agg <- aggregate(list(priority.level = eff),
                 by = list(consumer = lc, resource = lr), FUN = max)
lumped_links <- agg[order(agg$consumer, agg$resource), ]

# Manual overrides. Can promote or demote any existing link.
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

self <- lumped_links$consumer == lumped_links$resource
if (any(self))
  warning("Lumping created self-link(s) in lumped guild(s): ",
          paste(unique(lumped_links$consumer[self]), collapse = ", "), call. = FALSE)

n_l <- nrow(lg)
lumped_matrix <- matrix(0L, n_l, n_l)
lumped_matrix[cbind(lumped_links$consumer, lumped_links$resource)] <- lumped_links$priority.level

## ---- lumped outputs ---------------------------
write.csv(lg, file.path(output_path,"guilds_lumped.csv"), row.names = FALSE)

write.csv(lumped_links, file.path(output_path,"links_lumped.csv"), row.names = FALSE)

output_filename_lumped <- file.path(output_path, "guild_matrix_lumped.csv")
write.table(lumped_matrix, output_filename_lumped, sep = ",", row.names = FALSE, col.names = FALSE)
