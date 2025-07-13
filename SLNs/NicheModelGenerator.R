### Generate null webs with niche model 

library(ATNr)
library(igraph)

# import Rhynie web metrics (trophic species lumped)
df <- read_csv("TrophospeciesMetrics.csv")
# filter so it's just the rhynie full web
df_rhynie_full <- df %>%
  filter(Analysis == "Rhynie_all_TS")

sp_richnesses <- df_rhynie_full$S
connectances <- df_rhynie_full$C 

# create n niche models with C and S values drawn from Rhynie webs and output them as csv's along w sp. info files
n = 1000
pathname = "Niche_TS/"
dir.create(pathname, showWarnings = FALSE)
  
for (i in 1:n){
  # sample values for S and C
  S <- sample(sp_richnesses,1)
  C <- sample(connectances,1)
  
  # create species info dataframe
  speciesinfo <- data.frame(sp_name = 1:S, guild = 1:S)
  
  # create and transpose matrix so it's in the correct format
  nichewebmatrix <- t(as.matrix(create_niche_model(S,C)))
  
  # output files
  write.csv(speciesinfo, paste0(pathname, "speciesinfo_", i, ".csv"))
  write.table(nichewebmatrix, paste0(pathname, "matrix_", i, ".csv"),
              sep = ",", row.names = FALSE, col.names = FALSE)
}

