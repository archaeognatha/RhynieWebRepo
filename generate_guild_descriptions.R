# generate_guild_descriptions.R
# ------------------------------------------------------------------------------
# Generates a markdown document describing each guild in the Rhynie food web
# metaweb. For each guild it lists: resource guilds (what it eats), consumer
# guilds (what eats it), with priority annotations; plus habitat, species
# richness, and major taxa pulled from guilds.csv.
#
# Inputs:  guilds.csv, links.csv  (both in working directory)
# Output:  RhynieGuild_Descriptions_generated.md
#
# Convert to Word with:  pandoc RhynieGuild_Descriptions_generated.md \
#                          -o RhynieGuild_Descriptions.docx
# ------------------------------------------------------------------------------

library(readr)
library(dplyr)

# -------- Read source files ---------------------------------------------------
guilds <- read_csv("guilds.csv", show_col_types = FALSE)
links  <- read_csv("links.csv",  show_col_types = FALSE)

# Tidy column names so they're easier to reference
names(guilds) <- c("guild_no", "guild_name", "major_taxa", "G", "sp_tp",
                   "prio_resources", "gen_resources", "terr", "aqu")
names(links)  <- c("consumer", "resource", "priority")

# Lookup: guild_no -> guild_name
name_for <- setNames(guilds$guild_name, guilds$guild_no)

# -------- Helper functions ----------------------------------------------------
habitat_label <- function(terr, aqu) {
  if (terr == 1 && aqu == 1)  return("terrestrial + aquatic")
  if (terr == 1)              return("terrestrial")
  if (aqu  == 1)              return("aquatic")
  "unspecified habitat"
}

# Format a numeric vector of guild IDs as a markdown bulleted list, with
# "(priority resource)" tagged on the IDs that appear in `priority_ids`
format_guild_list <- function(ids, priority_ids) {
  ids <- sort(unique(ids))
  if (length(ids) == 0) return("- *(none listed)*")
  vapply(ids, function(g) {
    tag <- if (g %in% priority_ids) " (priority resource)" else ""
    sprintf("- %d\\. %s%s", g, name_for[as.character(g)], tag)
  }, character(1)) |> paste(collapse = "\n")
}

# -------- Build the document --------------------------------------------------
out <- c(
  "# Rhynie Chert Food Web — Guild Descriptions",
  "",
  sprintf("*Auto-generated from `guilds.csv` and `links.csv` on %s*",
          format(Sys.Date(), "%B %d, %Y")),
  "",
  "Each guild is listed with its **resource guilds** (what it eats) and",
  "**consumer guilds** (what eats it). The \"(priority resource)\" tag means at",
  "least one interaction with that guild is always assigned during",
  "species-level network (SLN) generation; non-priority interactions", 
  "may or may not be assigned in any given replicate.",
  "",
  "Pending edits to be discussed are marked in red or with Tracked Changes.",
  "",
  "---",
  ""
)

for (i in seq_len(nrow(guilds))) {
  g   <- guilds[i, ]
  gid <- g$guild_no

  # Resources for this guild = links where this guild is the consumer
  res_rows     <- links %>% filter(consumer == gid)
  res_priority <- res_rows %>% filter(priority == 2) %>% pull(resource)
  res_all      <- res_rows %>% pull(resource)

  # Consumers of this guild = links where this guild is the resource
  cons_rows     <- links %>% filter(resource == gid)
  cons_priority <- cons_rows %>% filter(priority == 2) %>% pull(consumer)
  cons_all      <- cons_rows %>% pull(consumer)

  # ---- Header and metadata ----
  out <- c(out, sprintf("## Guild %d: %s", gid, g$guild_name), "")

  type_label <- if (is.na(g$sp_tp) || g$sp_tp == "") {
    "primary resource (detritus)"
  } else g$sp_tp

  meta_bits <- c(
    habitat_label(g$terr, g$aqu),
    type_label,
    sprintf("S = %d", g$G)
  )
  if (!is.na(g$major_taxa) && g$major_taxa != "") {
    meta_bits <- c(meta_bits, paste0("major taxa: ", g$major_taxa))
  }
  out <- c(out, paste0("*", paste(meta_bits, collapse = " · "), "*"), "")

  # ---- Resource guilds ----
  out <- c(out, "**Resource guilds:**", "")
  is_producer <- !is.na(g$sp_tp) && g$sp_tp == "producer"
  is_basal    <- (is.na(g$sp_tp) || g$sp_tp == "")
  if (is_producer) {
    out <- c(out, "- *(primary producer — no resource guilds)*", "")
  } else if (is_basal) {
    out <- c(out, "- *(primary resource node — no resource guilds)*", "")
  } else {
    out <- c(out, format_guild_list(res_all, res_priority), "")
  }

  # ---- Consumer guilds ----
  out <- c(out, "**Consumer guilds:**", "")
  if (length(cons_all) == 0) {
    out <- c(out, "- *(no consumers listed; top of food chain in this metaweb)*", "")
  } else {
    out <- c(out, format_guild_list(cons_all, cons_priority), "")
  }

  # ---- Member taxa placeholder ----
  out <- c(out, "**Member taxa:** *[fill from Guild composition sheet]*", "")

  out <- c(out, "---", "")
}

writeLines(out, "RhynieGuild_Descriptions_generated.md")
cat("Wrote RhynieGuild_Descriptions_generated.md\n")
cat("Convert with: pandoc RhynieGuild_Descriptions_generated.md ",
    "-o RhynieGuild_Descriptions.docx\n", sep = "")
