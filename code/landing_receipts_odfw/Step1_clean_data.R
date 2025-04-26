

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
grade_key_orig <- readxl::read_excel(file.path(indir, "codes_from_mark_freeman.xlsx"), sheet="Grades")
use_key_orig <- readxl::read_excel(file.path(indir, "codes_from_mark_freeman.xlsx"), sheet="Use")
condition_key_orig <- readxl::read_excel(file.path(indir, "codes_from_mark_freeman.xlsx"), sheet="Condition")
legality_key_orig <- readxl::read_excel(file.path(indir, "codes_from_mark_freeman.xlsx"), sheet="Legal status")

# To-do list
# Understand vessel identifiers
  
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

# Uses
use_key <- use_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(use_code=code) %>% 
  mutate(use_code=as.numeric(use_code))

# Grades
grade_key <- grade_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(grade=size) %>% 
  mutate(grade_code=as.numeric(grade_code))

# Conditions
condition_key <- condition_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(condition_code=code) %>% 
  mutate(condition_code=as.numeric(condition_code))

# Conditions
legality_key <- legality_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(legality_code=legal_status_code,
         legality=legal_status_description) %>% 
  mutate(legality_code=as.numeric(legality_code))



# Merge data
################################################################################

# List files
files2merge <- list.files(file.path(indir, "fish_tickets"), pattern = ".xlsx")
data_orig <- purrr::map_df(files2merge, function(x){
  
  df <- readxl::read_excel(file.path(indir, "fish_tickets", x), skip=1)
    
}) 


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(revenues_usd=landed_value,
         use_code=use,
         condition_code=cond,
         grade_code=grd,
         landings_n=number_of_fish,
         landings_lbs=ticket_lbs,
         landings_lbs_round=round_lbs,
         receipt_id=ticket_no, 
         date=landed,
         date_modified=modified,
         area_code=area,
         dealer_id=dlr,
         gear_code=gear, 
         port_code=port,
         legality_code=legal_status,
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
  # Add use
  mutate(use_code=as.numeric(use_code)) %>% 
  left_join(use_key) %>% 
  # Add grade
  mutate(grade_code=as.numeric(grade_code)) %>% 
  left_join(grade_key %>% select(-typical_species)) %>%
  # Add condition
  mutate(condition_code=as.numeric(condition_code)) %>% 
  left_join(condition_key %>% select(-typical_species)) %>%
  # Add legality
  mutate(legality_code=as.numeric(legality_code)) %>% 
  left_join(legality_key %>% select(-legacy_code)) %>%
  # Arrange
  select(receipt_id, receipt_status, 
         year, date, date_modified,
         dealer_id, license, permit, doc_no, 
         port_code, port, area_code, 
         gear_code, gear, days_fished, 
         species_code, comm_name, 
         legality_code, legality,
         condition_code, condition,
         use_code, use,
         grade_code, grade,
         landings_n, landings_lbs, landings_lbs_round, 
         price_usd_lb, revenues_usd,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect more
table(data$receipt_status)

# Areas include multiple areas
table(data$area_code)

# Time
range(data$year)
range(data$date)

# Landings
ggplot(data %>% sample_frac(0.3), aes(x=landings_lbs, y= landings_lbs_round)) +
  geom_point() +
  geom_abline(slope=1)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "ODFW_2005_2023_landing_receipts.Rds"))


