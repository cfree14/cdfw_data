

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
         block_id=block,
         depth_ft=depth,
         multi_day_yn=multi_day_receivered,
         n_kept=legals_retained,
         n_released=shorts_released,
         n_traps_pulled=traps_pulled,
         n_traps_set=num_traps_deployed,
         receipt_id=landing_receipt, 
         crew_id=crew_member_id,
         location_type=trap_location,
         lat_dd=latitude_dec,
         long_dd=longitude_dec,
         n_nights=nights_in_water) %>% 
  # Format date
  mutate(date=lubridate::mdy(date),
         month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  # Format longitude
  mutate(long_dd=long_dd*-1) %>% 
  # Arrange
  select(logbook_id, year, month, date, 
         vessel_id, vessel, 
         fisher_id, fisher_last, fisher_first, crew_id,
         depth_ft, block_id, location_type, lat_dd, long_dd,
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

# Location
table(data$location_type) # -- this is crap
table(data$block_id)


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
  coord_sf(x=range(data$long_dd, na.rm=T), y=range(data$lat_dd, na.rm=T)) +
  # Theme
  theme_bw()
g



