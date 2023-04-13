

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

# Get zipcodes
################################################################################

# Dowload database
# zipcodeR::download_zip_data()
zip_code_db <- zipcodeR::zip_code_db

# Get CA zipcodes
ca_zipcodes <- zip_code_db %>% 
  filter(state=="CA") %>% 
  select(zipcode, county) %>% 
  mutate(county=gsub(" County", "", county))

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
         permit_date=lubridate::mdy(permit_date)) %>% 
  # Format status
  mutate(license_status=stringr::str_to_title(license_status)) %>% 
  # Format permit
  mutate(permit=permit %>% stringr::str_to_title() %>% stringr::str_squish())

# Inspect
str(data1)
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
  mutate(county=stringr::str_to_title(county)) %>% 
  # Format zipcode
  mutate(zipcode=as.character(zipcode),
         license=as.character(license))

# Inspect
str(data2)
head(data2)

# Figure out county code
county_key1 <- data1 %>% 
  count(county_code, zipcode) %>% select(-n)
county_key2 <- data2 %>% 
  count(county, zipcode)  %>% select(-n)
county_key <- county_key1 %>% 
  left_join(county_key2) %>% 
  select(county, county_code) %>% 
  unique()


# Merge data
################################################################################

# Merge data
data <- bind_rows(data1, data2) %>% 
  # Arrange
  select(county, county_code, zipcode, 
         license, license_year, license_date, license_status,
         permit, permit_date, permit_year, permit_office, everything())

# Inspect
str(data)

# Inspect
table(data$permit)
