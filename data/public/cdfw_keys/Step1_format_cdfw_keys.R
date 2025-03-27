

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/public/cdfw_keys/raw"
outdir <- "data/public/cdfw_keys/processed"


# Format port key
################################################################################

# Read port key
port_key_orig <- read.csv(file.path(indir, "PortCodesExtract.csv"), as.is=T)

# Format port key
port_key <- port_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(port=port_name,
         port_complex=major_port_name,
         port_complex_code=major_port_code) %>%
  # Format port
  mutate(port=stringr::str_to_title(port),
         port_complex=stringr::str_to_title(port_complex)) %>%
  # Format date
  mutate(discontinued_date=lubridate::mdy(discontinued_date)) %>%
  # Arrange
  select(port_complex, port_complex_code, port, port_code, everything()) %>%
  arrange(port_complex, port)

# Inspect
str(port_key)

# Export
saveRDS(port_key, file=file.path(outdir, "CDFW_port_key.Rds"))
write.csv(port_key, file=file.path(outdir, "CDFW_port_key.csv"), row.names=F)


# Format gear key
################################################################################

# Read key
gear_key_orig <- read.csv(file.path(indir, "GearCodesExtract.csv"), as.is=T)

# Format key
gear_key <- gear_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(gear=gear_name_chris,
         discontinued_date=discontinue_date,
         gear_type=gear_type_chris) %>%
  # Format date
  mutate(discontinued_date=lubridate::mdy(discontinued_date)) %>% 
  # Simplify
  select(gear_code, gear, gear_type, discontinued_date)

# Inspect
str(gear_key)

# Export
saveRDS(gear_key, file=file.path(outdir, "CDFW_gear_key.Rds"))
write.csv(gear_key, file=file.path(outdir, "CDFW_gear_key.csv"), row.names=F)



