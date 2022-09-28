

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
data1_orig <- read.csv(file.path(indir, "LicensesPermits_2000-2010", "FishBusinessExtract.CSV"), 
                       as.is=T, na.strings=c("<NA>", ""))
data2_orig <- read.csv(file.path(indir, "LicensesPermits_2011-2020", "CommercialExtract_Business_2011-2020.csv"), as.is=T, na.strings="")


# Format data
################################################################################

# Format data 1
data1 <- data1_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(business_id=fish_business_id, 
         business_name=dba,
         business_type=fbus_lic, 
         county_code=county,
         zipcode=street_zip,
         zipcode_plant=plant_street_zip,
         issue_date=reg_issuedate, 
         issue_office=reg_issue_office, 
         year=fbus_year) %>% 
  # Arrange
  select(business_id, business_name, business_type,
         county_code, zipcode, zipcode_plant, plant_number, 
         issue_date, issue_office, everything())

# Inspect
head(data1)


# Format data 2
data2 <- data2_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  rename(business_id=fish_business_id1,
         business_name=dba1,
         zipcode=address_zip_code1, 
         county=county1,
         year=item_year,
         license=item_name)

# Inspect
head(data2)




