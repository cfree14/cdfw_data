

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/lobster_logbooks/raw"
outdir <- "data/confidential/lobster_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
species_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))

# Read data
data_orig <- read.csv(file.path(indir, "Lobster Logs Extract.csv"), as.is=T, na.strings="")

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(date=log_date_string,
         logbook_id=serial_number,
         vessel=vessel_name,
         fisher_id=fisher_license_id,
         fisher_last=fisher_last_name,
         fisher_first=fisher_first_initial,
         block_id_orig=block,
         depth_ft=depth,
         multi_day_yn=multi_day_receivered,
         n_kept=legals_retained,
         n_released=shorts_released,
         n_traps_pulled=traps_pulled,
         n_traps_set=num_traps_deployed,
         receipt_id=landing_receipt, 
         crew_id=crew_member_id,
         location_orig=trap_location,
         lat_dd=latitude_dec,
         long_dd=longitude_dec,
         n_nights=nights_in_water) %>% 
  # Format date
  mutate(date=lubridate::mdy(date),
         month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  # Format block id
  # Review these decisions carefully
  mutate(block_id=stringr::str_squish(block_id_orig),
         block_id=ifelse(grepl("°", block_id), "", block_id),
         block_id=recode(block_id,
                         "O306"="0306",
                         "O326"="0326",
                         "O345"="0345",
                         ".842"="842",
                         "'822"="822",
                         "719+"="719",
                         "756'"="756",
                         "075+"="",
                         "68/8"="",
                         "68*9"="",
                         "RR22"="",
                         "A0708"="",
                         "68/0"="",
                         "86+1"="") %>% as.numeric()) %>%
  # Format location
  mutate(location_orig=stringr::str_trim(location_orig)) %>% 
  # Format longitude
  mutate(long_dd=long_dd*-1) %>% 
  # Arrange
  select(logbook_id, year, month, date, 
         vessel_id, vessel, 
         fisher_id, fisher_last, fisher_first, crew_id,
         depth_ft, block_id_orig, block_id, location_orig, lat_dd, long_dd,
         multi_day_yn, n_traps_set, n_traps_pulled, n_nights,
         n_kept, n_released,
         everything())

# Inspect
str(data)

# Date
range(data$date)
range(data$year)
range(data$month)

# Inspect
table(data$multi_day_yn)

# Fisher key
fisher_key <- data %>% 
  select(fisher_id, fisher_last, fisher_first) %>% 
  unique() %>% 
  arrange(fisher_id)

# Vessel key
vessel_key <- data %>% 
  select(vessel_id, vessel) %>% 
  unique() %>% 
  arrange(vessel_id)
sort(unique(data$vessel))
sort(unique(data$vessel_id))

# Block key
block_key <- data %>% 
  select(block_id_orig, block_id) %>% 
  unique()

# Location key
location_key <- data %>% 
  # Unique locations
  select(location_orig) %>% 
  unique() %>% 
  arrange(location_orig) %>% 
  # Add location type
  mutate(location_type=ifelse(grepl("°", location_orig), "lat/long", "place")) %>% 
  mutate(location=ifelse(location_type=="place", stringr::str_to_title(location_orig), location_orig))

# Species key




# Location
###################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot 
g <- ggplot() +
  # Labels
  labs(x="", y="") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Plot points
  geom_point(data, mapping=aes(x=long_dd, y=lat_dd), pch=1, color="grey30", alpha=0.5) +
  # Crop
  coord_sf(x=c(-130, -114), y=c(30,50)) +
  # coord_sf(x=range(data$long_dd, na.rm=T), y=range(data$lat_dd, na.rm=T)) +
  # Theme
  theme_bw()
g



