

rpkg <- c("dplyr ggplot2 readr boot divvy terra divDyn paleobioDB conflicted piggyback CoordinateCleaner fossilbrush rgplates icosa tidyr tibble readr purrr downloadthis ggpubr")

import_pkg <- function(x)
  x |> trimws() |> strsplit("\\s+")  |> unlist() |> 
  lapply(function(x) library(x, character.only = T)) |> 
  invisible()

rpkg |> import_pkg()

# Resolve conflicted functions.
conflicted::conflict_prefer(name = "filter", winner = "dplyr",losers = "stats")

library(openxlsx)

# Shark occurrence dataset.
Data <- read.xlsx(xlsxFile = "Data.xlsx")

maastrichtian_occurences <- Data |> filter(Age == "Maastrichtian")

# Get paleocoordinates.
paleo_coords <- reconstruct(x = maastrichtian_occurences[,21:22] |> relocate(Latitude,.after = "Longitude"),age = 69)




# Identify Invalid Coordinates.
cl <- cc_val(pbdb, value = "flagged", lat="paleolat", lon  ="paleolng") #flags incorrect coordinates
cl_rec <- pbdb[!cl,] #extract and check them

pbdb <- pbdb |> 
  cc_val(lat = "paleolat", lon="paleolng") #remove them

# Use fossilbrush to clean taxonomic errors
b_ranks <- c("phylum", "class", "order", "family", "accepted_name") #accepted_name is genus name

# Define a list of suffixes to be used at each taxonomic level when scanning for synonyms
b_suff = list(NULL, NULL, NULL, NULL, c("ina", "ella", "etta"))

pbdb2 <- check_taxonomy(pbdb, suff_set = b_suff, ranks = b_ranks, verbose = FALSE,clean_name = TRUE, resolve_duplicates = TRUE, jump = 5)
# resolves homonyms, and jump refers to which taxonomic rank is the highest we resolve to. jump = 5 will stop before phylum since phylum level usually has little error.

# Extract PBDB data from obdb2 so we have the corrected taxa:
pbdb <- pbdb2$data[1:nrow(pbdb),]

pbdb_fulldata <- pbdb # keep a record of all pertinent information, just in case