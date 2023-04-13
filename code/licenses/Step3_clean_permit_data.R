

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
                       as.is=T, na.strings="")
data2_orig <- read.csv(file.path(indir, "LicensesPermits_2011-2020", "CommercialExtract_Vessel_HistoricalOwner_2011-2020.csv"), 
                       as.is=T, na.strings="")

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.rds"))

# There are still some columns I don't fully understand
# Check this again carefully


# Format data
################################################################################

# Format data 1
#######################################

# Format data 1
data1 <- data1_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(vessel=vessel_name,
         length_ft=length,
         beam_ft=beam,
         horsepower=horse_power,
         home_port_code=home_port,
         permit=vessel_permit)  %>% 
  # Format dates
  mutate(reg_issue_date=lubridate::mdy(reg_issue_date),
         permit_issue_date=lubridate::mdy(permit_issue_date)) %>% 
  # Format permit
  mutate(permit=stringr::str_to_title(permit),
         permit=recode(permit,
                       "Market Squid Vessel(Nt)"="Market Squid Vessel (NT)", 
                       "Market Squid Vessel(T)"="Market Squid Vessel (T)",
                       "Market Squid Brail Permit(T)"="Market Squid Brail Permit (T)",
                       "Dungeness Crab Vessel Resident"="Dungeness Crab Vessel (R)",
                       "Squid Light Boat (Nt)"="Squid Light Boat (NT)")) %>% 
  # Add doc type
  mutate(doc_type="Unknown") %>% 
  # Add home port
  left_join(port_key %>% select(port_code, port), by=c("home_port_code"="port_code")) %>% 
  rename(home_port=port) %>% 
  # Arrange
  select(vessel_id, doc_type, cf_doc, vessel,
         passengers, year_built, length_ft, beam_ft, horsepower, tonnage,
         home_port_code, home_port,
         permit, permit_issue_date, permit_issue_office,
         reg_issue_date, reg_issue_office, vessel_year, license_status, everything())
  
# Inspect
str(data1)

# Inspect more
freeR::uniq(data1$permit)


# Format data 2
#######################################

# Format data 2
data2 <- data2_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  rename(vessel_id=vessel_id1,
         cf_doc_orig=vessel_documentation1,
         vessel=vessel_name1,
         passengers=passengers1,
         year_built=year_built1,
         length_ft=length_in_feet1,
         beam_ft=breadth_in_feet1,
         horsepower=horsepower1,
         tonnage=gross_tonnage1,
         home_port=home_port1,
         major_port=major_port1,
         valid_from=valid_from1,
         valid_to=valid_to1, 
         permit=item_name,
         permit_year=item_year,
         permit_id=le_permit_number1) %>% 
  # Format doc
  mutate(cf_doc_orig=gsub("State Registration: ", "", cf_doc_orig),
         cf_doc_orig=gsub("Coast Guard:", "Coast Guard -", cf_doc_orig)) %>% 
  separate(cf_doc_orig, into=c("doc_type", "cf_doc"), sep=" - ") %>% 
  # Format permit
  mutate(permit=gsub(", Tier", " - Tier", permit)) %>% 
  # Format dates
  mutate(valid_from=lubridate::mdy(valid_from),
         valid_to=lubridate::mdy(valid_to)) %>% 
  # Format vessel id
  mutate(vessel_id=vessel_id %>% gsub("FG", "", .) %>% as.numeric()) %>% 
  # Add home port code
  mutate(home_port=stringr::str_to_title(home_port)) %>% 
  left_join(port_key %>% select(port_code, port), by=c("home_port"="port")) %>% 
  rename(home_port_code=port_code) %>% 
  # Format
  mutate(major_port=stringr::str_to_title(major_port)) %>% 
  # Arrange
  select(vessel_id, doc_type, cf_doc, vessel,
         passengers, year_built, length_ft, beam_ft, horsepower, tonnage,
         major_port, home_port_code, home_port,
         permit_id, permit, permit_year, valid_from, valid_to, 
         license_status, everything())

# Inspect
str(data2)

# Permit
freeR::uniq(data2$permit)

# Port
freeR::uniq(data2$home_port)
freeR::uniq(data2$major_port)
port_key2 <- data2 %>%
  count(major_port, home_port)


# Merge data
################################################################################

# Merge data
data <- bind_rows(data1, data2) %>% 
  # Format stats
  mutate(license_status=stringr::str_to_title(license_status)) %>% 
  # Format vessel name
  mutate(vessel=vessel %>% stringr::str_to_upper() %>% stringr::str_squish()) %>% 
  # Format permit
  mutate(permit=gsub(" Permit", "", permit),
         permit=gsub("Nt", "NT", permit),
         permit=gsub("\\(Vessel)", "Vessel", permit),
         permit=gsub("\\(Tier 1)", "- Tier 1", permit),
         permit=gsub("\\(Tier 2)", "- Tier 2", permit),
         permit=gsub("\\(Tier 3)", "- Tier 3", permit),
         permit=gsub("\\(See Comments)", "", permit)) %>% 
  # Arrange
  select(vessel_id, doc_type, cf_doc, vessel, 
         passengers, year_built, length_ft, beam_ft, horsepower, tonnage,
         major_port, home_port, home_port_code, 
         permit, permit_issue_date, permit_issue_office, permit_year,
         reg_issue_date, reg_issue_office, vessel_year, license_status, everything()) %>% 
  # Remove useless
  # 1) You can add major port more authoritatively
  select(-major_port)
  
# Inspect
str(data)

# Inspect
freeR::uniq(data$home_port)
freeR::uniq(data$home_port_code)

# Permits
freeR::uniq(data$permit)
table(data$license_status)

# Offices
table(data$permit_issue_office)
table(data$reg_issue_office)


# Vessel key
################################################################################

# I did not get as far as standardizing the documenation number

# Vessel key
vessel_key <- data %>% 
  group_by(vessel_id, passengers, year_built, length_ft, beam_ft) %>% 
  summarize(nnames=n_distinct(vessel[!is.na(vessel)]),
            vessel=paste(unique(vessel[!is.na(vessel)]), collapse = ", ")) %>% 
  ungroup()
sum(vessel_key$nnames>1)
freeR::which_duplicated(vessel_key$vessel_id)

# Export
saveRDS(vessel_key, file=file.path(outdir, "CDFW_vessel_key.Rds"))


# Merge data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_2000_2020_permit_data.Rds"))



