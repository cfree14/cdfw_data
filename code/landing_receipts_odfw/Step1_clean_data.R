

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/landing_receipts_odfw/raw"
outdir <- "data/confidential/landing_receipts_odfw/processed"

# Read keys
spp_key_orig <- readxl::read_excel(file.path(indir, "ODFW_Fishery_Logbook_Codes.xls"), sheet="Species", na="N/A")
gear_key_orig <- readxl::read_excel(file.path(indir, "ODFW_Fishery_Logbook_Codes.xls"), sheet="Gear")
port_key_orig <- readxl::read_excel(file.path(indir, "ODFW_Fishery_Logbook_Codes.xls"), sheet="Ports")
target_key_orig <- readxl::read_excel(file.path(indir, "ODFW_Fishery_Logbook_Codes.xls"), sheet="Trawl Strategy Target Code")


# Format keys
################################################################################

# Species
spp_key <- spp_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(species_code=odfw_code, 
         comm_name=common_name,
         sci_name=scientific_name)  %>% 
  # Format
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         sci_name=stringr::str_to_sentence(sci_name))

# Check sci names
freeR::check_names(spp_key$sci_name)

# Gear
gear_key <- gear_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(gear=gear_description) %>% 
  mutate(gear_code=as.numeric(gear_code))

# Ports
port_key <- port_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(port=port_name) %>% 
  mutate(port_code=as.numeric(port_code))



# Merge data
################################################################################

# List files
files2merge <- list.files(file.path(indir, "fish_tickets"), pattern = ".xlsx")[2:8]
data_orig <- purrr::map_df(files2merge, function(x){
  
  df <- readxl::read_excel(file.path(indir, "fish_tickets", x), skip=1)
    
}) 


# Format data
################################################################################

# 705/295 missing common names

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(revenues_usd=landed_value,
         use_code=use,
         condition_code=cond,
         landings_n=number_of_fish,
         landings_lbs1=ticket_lbs,
         landings_lbs2=round_lbs,
         receipt_id=ticket_no, 
         date=landed,
         date_modified=modified,
         area_code=area,
         dealer_id=dlr,
         gear_code=gear, 
         port_code=port,
         price_usd_lb=price_per_pound, 
         receipt_status=ticket_status) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         date_modified=lubridate::ymd(date_modified)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Add species
  left_join(spp_key %>% select(species_code, comm_name)) %>% 
  # Add gear
  left_join(gear_key) %>% 
  # Add port
  mutate(port_code=as.numeric(port_code)) %>% 
  left_join(port_key) %>% 
  # Arrange
  select(receipt_id, receipt_status, 
         year, date, date_modified,
         dealer_id, license, permit, doc_no, 
         port_code, port, area_code, 
         grd, legal_status, 
         gear_code, gear, days_fished, 
         species_code, comm_name, 
         condition_code, use_code, 
         landings_n, landings_lbs1, landings_lbs2, 
         price_usd_lb, revenues_usd,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$receipt_status)

# Areas include multiple areas
table(data$area_code)

# What do these codes mean?
table(data$condition_code)
table(data$use_code)

# What do these variables and their codes mean?
table(data$grd)
table(data$legal_status)



# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "ODFW_2005_2023_landing_receipts.Rds"))


