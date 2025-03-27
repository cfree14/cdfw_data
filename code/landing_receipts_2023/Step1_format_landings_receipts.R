

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/landing_receipts_2023/raw/MLDS"
outdir <- "data/confidential/landing_receipts_2023/processed"

# Read species key
spp_key_orig <- readRDS("data/public/cdfw_keys/processed/CDFW_species_key.Rds")

# Read gear key
gear_key_orig <- readRDS("data/public/cdfw_keys/processed/CDFW_gear_key.Rds")


# Merge data
################################################################################

# Files 2 merge
files2merge <- list.files(indir)

# Merge data
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- read.csv(file.path(indir, x), na.strings = "") %>% 
    mutate_all(.funs = as.character)
  
})


# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(receipt_id=landing_receipt_num,
         date=landing_date,      
         permit_state=state_permit_number,
         permit_gf=gf_permit_num,  
         port=port_name, 
         block_id=cdfw_block_id,   
         primary_gear_orig=primary_gear_name,
         comm_name_orig=species_name,    
         landings_lbs=pounds,           
         price_usd=unit_price,         
         value_usd=total_price,        
         gear_orig=gear_name,        
         condition_id=fish_condition_id,
         condition=fish_condition_name,
         use=use_name) %>% 
  # Convert to numeric
  mutate(across(.cols=c(landings_lbs, value_usd, price_usd), .fns=as.numeric)) %>% 
  mutate(across(.cols=c(vessel_id, port_id, block_id, business_id, species_id, 
                        primary_gear_id, gear_id, condition_id, use_id), .fns=as.numeric)) %>% 
  # Convert date
  mutate(date=lubridate::mdy(date)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Format port
  mutate(port=stringr::str_to_title(port)) %>% 
  mutate(port_id=ifelse(port_id %in% c(0, -1), NA, port_id)) %>% 
  mutate(port=case_when(is.na(port_id) ~ NA,
                        port_id==452 ~ "Princeton-Half Moon Bay",
                        T ~ port)) %>% 
  # Format vessel id
  mutate(vessel_id=ifelse(vessel_id %in% c(0, -1), NA, vessel_id)) %>% 
  # Format fisher id
  mutate(fisher_id=ifelse(fisher_id=="L00000", NA, fisher_id)) %>% 
  # Format business id
  mutate(business_id=ifelse(business_id %in% c(-1), NA, business_id)) %>% 
  # Format block id
  mutate(block_id=ifelse(block_id %in% c(-1,0), NA, block_id)) %>% 
  # Format species
  mutate(comm_name_orig=stringr::str_squish(comm_name_orig)) %>% 
  # Add species
  left_join(spp_key_orig %>% select(spp_code_num, comm_name), by=c("species_id"="spp_code_num")) %>% 
  # Format primary gear
  mutate(primary_gear_orig=stringr::str_to_sentence(primary_gear_orig) %>% stringr::str_squish(.),
         primary_gear_id=ifelse(primary_gear_id==0, NA, primary_gear_id),
         primary_gear_orig=ifelse(is.na(primary_gear_id), NA, primary_gear_orig)) %>% 
  # Add primary gear meta-data
  left_join(gear_key_orig %>% select(gear_code, gear, gear_type), by=c("primary_gear_id"="gear_code")) %>% 
  rename(primary_gear=gear,
         primary_gear_type=gear_type) %>% 
  # Format gear
  mutate(gear_orig=stringr::str_to_sentence(gear_orig) %>% stringr::str_squish(.),
         gear_id=ifelse(gear_id %in% c(-1, 0), NA, gear_id),
         gear_orig=ifelse(is.na(gear_id), NA, gear_orig)) %>% 
  # Add gear meta-data
  left_join(gear_key_orig %>% select(gear_code, gear, gear_type), by=c("gear_id"="gear_code")) %>% 
  # Format use
  mutate(use=stringr::str_to_sentence(use),
         use_id=ifelse(use_id %in% c(0, -1), NA, use_id),
         use=ifelse(is.na(use_id), NA, use)) %>% 
  # Format condition
  mutate(condition_id=ifelse(condition_id %in% c(0, -1), NA, condition_id),
         condition=stringr::str_to_sentence(condition),
         condition=ifelse(is.na(condition_id), NA, condition)) %>% 
  # Arrange
  select(year, date, receipt_id, 
         vessel_id, fisher_id, permit_state, permit_gf,
         port_id, port, business_id, block_id,
         primary_gear_id, primary_gear_orig, primary_gear, primary_gear_type,
         gear_id, gear_orig, gear, gear_type,
         species_id, comm_name_orig, comm_name,
         condition_id, condition, 
         use_id, use,
         landings_lbs, price_usd, value_usd,
         everything()) %>% 
  arrange(date, receipt_id)

# Inspect
str(data1)
freeR::complete(data1)


# Inspect keys
################################################################################

# Date
range(data1$date)

# Vessel key
vessel_key <- data1 %>% 
  count(vessel_id)

# Port key
port_key <- data1 %>% 
  count(port_id, port) %>% 
  arrange(port_id)
freeR::which_duplicated(port_key$port_id)

# Species key
spp_key <- data1 %>% 
  count(species_id, comm_name_orig, comm_name) %>% 
  arrange(species_id)
freeR::which_duplicated(spp_key$species_id)

# Use key
use_key <- data1 %>% 
  count(use_id, use) %>% 
  arrange(use_id)
freeR::which_duplicated(use_key$use_id)

# Condition key
condition_key <- data1 %>% 
  count(condition_id, condition) %>% 
  arrange(condition_id)
freeR::which_duplicated(condition_key$condition_id)

# Primary gear key (75 is unknown)
primary_gear_key <- data1 %>% 
  count(primary_gear_id, primary_gear_orig, primary_gear, primary_gear_type) %>% 
  arrange(primary_gear_id)
freeR::which_duplicated(primary_gear_key$primary_gear_id)

# Gear key
gear_key <- data1 %>% 
  count(gear_id, gear_orig, gear, gear_type) %>% 
  arrange(gear_id)
freeR::which_duplicated(gear_key$gear_id)


# Export data
################################################################################

# Prep for export
data2 <- data1 %>% 
  select(-c(primary_gear_orig, gear_orig))

# Export data
saveRDS(data2, file=file.path(outdir, "1980_2022_landings_receipts.Rds"))

