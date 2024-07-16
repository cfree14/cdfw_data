

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/squid_logbooks/raw"
outdir <- "data/confidential/squid_logbooks/processed"

# Read data
data_orig <- readRDS(file.path(outdir, "CDFW_2000_2022_squid_lightboat_logbook_data.Rds"))

# Read landings data
landings_orig <- readRDS("data/confidential/landing_receipts_2023/processed/1980_2022_landings_receipts.Rds")

# Read block key
block_key <- readxl::read_excel(file.path(indir, "FishingBlockLocationNames_Reference.xlsx"), sheet=2, na="0") %>% 
  janitor::clean_names("snake") %>% 
  mutate(code=gsub("_", "-", code)) %>% 
  mutate(code=make.unique(code)) # "CO-PR" was duplicated
freeR::which_duplicated(block_key$code) 
freeR::which_duplicated(block_key$location)




# Simplify landing receipts
################################################################################

# Format receipts
landings <- landings_orig %>% 
  select(receipt_id, vessel_id, port, block_id) %>% 
  unique() %>% 
  mutate(vessel_id=as.numeric(vessel_id))
freeR::which_duplicated(landings$receipt_id)


# Build receipt key
################################################################################

# Receipt key
receipt_key <- data_orig %>% 
  # Filter and simplify
  filter(!is.na(receipt_ids)) %>% 
  select(logbook_id, vessel_id, receipt_ids) %>% 
  unique() %>% 
  mutate(vessel_id=as.numeric(vessel_id))

# Add meta-data
receipt_key2 <- receipt_key %>% 
  # Add data
  left_join(landings, by=c("receipt_ids"="receipt_id", "vessel_id"))


# Add to data
################################################################################

# Add to data
data <- data_orig %>% 
  # Rename
  rename(block_id_rep=block_id) %>% 
  # Convert vessel id to numeric for merge
  mutate(vessel_id=as.numeric(vessel_id)) %>% 
  # Add port and block id from receipt
  left_join(receipt_key2) %>% 
  rename(block_id_receipt=block_id) %>% 
  # Add block id from location key
  left_join(block_key %>% select(location, block_id1), by=c("location_long"="location")) %>% 
  rename(block_id_loc=block_id1) %>% 
  # Arrange
  relocate(port, .before=date) %>% 
  relocate(block_id_receipt, .after=block_id_rep) %>% 
  relocate(block_id_loc, .after=block_id_rep)


# Build location key
################################################################################

# Location key
location_key <- data %>% 
  # Location to use
  mutate(block_id_use=ifelse(!is.na(block_id_rep), block_id_rep,
                             ifelse(!is.na(block_id_loc), block_id_loc, block_id_receipt))) %>% 
  # Simplify
  count(location_long, location_code, block_id_use) %>% 
  filter(!is.na(location_long))




