

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/licenses/raw"
outdir <- "data/confidential/licenses/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))

# Read data
data1_orig <- read.csv(file.path(indir, "LicensesPermits_2000-2010", "VesselPermitExtract.CSV"), 
                       as.is=T)
data2_orig <- read.csv(file.path(indir, "LicensesPermits_2011-2020", "CommercialExtract_Vessel_HistoricalOwner_2011-2020.csv"), 
                       as.is=T, na.strings="")


# Format data
################################################################################

# Format data 1
data1 <- data1_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(vessel=vessel_name,
         length_ft=length,
         beam_ft=beam,
         horsepower=horse_power)

# Inspect
head(data1)

# Format data 2
data2 <- data2_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  rename(vessel_id=vessel_id1,
         vessel=vessel_name1,
         year_built=year_built1,
         length_ft=length_in_feet1,
         breadth_ft=breadth_in_feet1,
         horsepower=horsepower1,
         tonnage=gross_tonnage1,
         home_port=home_port1,
         major_port=major_port1,
         valid_from=valid_from1,
         valid_to=valid_to1, 
         permit=item_name,
         permit_year=item_year) %>% 
  # Format dates
  mutate(valid_from=lubridate::mdy(valid_from),
         valid_to=lubridate::mdy(valid_to))

# Inspect
head(data2)
str(data2)

table(data2$valid_to)






