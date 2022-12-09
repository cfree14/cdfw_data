

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/public/cdfw_keys/raw"
outdir <- "data/public/cdfw_keys/processed"

# Read species key
spp_key_orig <- readRDS(file=file.path(outdir, "CDFW_species_key.Rds"))

# Read helper info
spp_key_add_on <- readxl::read_excel(file.path(indir, "taxa_info_for_species_without_taxa_info.xlsx"))

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2018_fao_landings_data.Rds")

# Build FAO species key
fao_spp_key <- fao_orig %>% 
  select(isscaap, family) %>% 
  unique() %>% 
  filter(isscaap!="") %>% 
  group_by(family) %>% 
  summarize(isscaap=paste(isscaap, collapse=" / "))


# Get taxa
################################################################################

# Species
spp <- spp_key_orig %>% 
  filter(level=="species") %>% 
  pull(sci_name) %>% unique()

# Get taxa info
spp_taxa <- freeR::taxa(species=spp)

# Which species do not have taxa info?
spp[!spp %in% spp_taxa$sciname]

# Add on
spp_taxa_plus <- bind_rows(spp_taxa, spp_key_add_on)

# Still any missing?
spp[!spp %in% spp_taxa_plus$sciname]

# Add ISSCAAP group
spp_taxa_plus_isscaap <- spp_taxa_plus %>% 
  # Add ISSCAAP
  left_join(fao_spp_key, by="family") %>% 
  # Fill ISSCAAP gaps
  mutate(isscaap=case_when(genus=="Sebastes" ~ "Miscellaneous demersal fishes", 
                           T ~ isscaap))


# Add and export
################################################################################

# Add taxa info
spp_key_final <- spp_key_orig %>% 
  left_join(spp_taxa_plus, by=c("sci_name"="sciname"))

# Export
saveRDS(spp_key_final, file=file.path(outdir, "CDFW_species_key_taxa.Rds"))






