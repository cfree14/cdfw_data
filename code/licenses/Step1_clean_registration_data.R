

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
data1_orig <- read.csv(file.path(indir, "LicensesPermits_2000-2010", "CFLPermitExtract.CSV"), as.is=T, na.strings="")
data2_orig <- read.csv(file.path(indir, "LicensesPermits_2011-2020", "CommercialExtract_Individual_2011-2020.csv"), as.is=T, na.strings="")


# Format data
################################################################################

# Format data 1
data1 <- data1_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(zipcode=street_zip,
         county_code=county, 
         license=cfl,
         license_date=cfl_issue_date,
         permit=permit_name,
         permit_date=permit_issue_date,
         permit_office=permit_issue_office,
         license_year=cfl_year) %>% 
  # Format date
  mutate(license_date=lubridate::mdy(license_date),
         permit_date=lubridate::mdy(permit_date))

# Inspect
head(data1)

# Inspect
freeR::uniq(data1$zipcode)
freeR::uniq(data1$county_code)
freeR::uniq(data1$permit)
freeR::uniq(data1$license_status)


# Format data 2
data2 <- data2_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  rename(zipcode=address_zip_code1,
         county=county1, 
         license=cfl_number1,
         permit=item_name,
         permit_year=item_year) %>% 
  # Format county
  mutate(county=stringr::str_to_title(county))

# Inspect
head(data2)




