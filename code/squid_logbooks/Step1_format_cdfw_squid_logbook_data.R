

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/squid_logbooks/raw"
outdir <- "data/confidential/squid_logbooks/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "MarketSquidLogs_ChrisFree_UCSB_DSA_240222.xlsx"), sheet="SquidVesselLogs")

# You're probably not reading all data in (true for comments)
# Haven't formatted times
# Haven't verified durations
# Haven't confirmed sets 
# Haven't worked on bycatch etc

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  janitor::clean_names("snake") %>% 
  rename(logbook_id=log_serial_number,
         vessel=vessel_name,
         captain=captain_name,
         date=log_date_string,
         time_min=elapsed_time,
         position=set_position,
         lat_dd=set_latitude,
         long_dd=set_longitude,
         sst_f=temperature,
         depth_fa=bottom_depth,
         catch_t=catch_estimate, 
         limited_yn=ltd_by_market_order,
         lightboat_id=light_brail_set_upon,
         bycatch=by_catch, 
         receipt_ids=landing_receipts) %>% 
  # Format date
  mutate(date=lubridate::ymd(date)) %>% 
  # Format latitude
  mutate(lat_dd=ifelse(lat_dd==0, NA, lat_dd)) %>% 
  # Format longitude
  mutate(long_dd=ifelse(long_dd==0, NA, long_dd),
         long_dd=abs(long_dd) *-1)

# Inspect
str(data)
head(data)
freeR::complete(data)

# Dates
range(data$date)

# SST
boxplot(data$sst_f)

# Depth
boxplot(data$depth_fa)

# Duration
boxplot(data$time_min)

# Vessels
vessel_key <- data %>% 
  count(vessel_id, vessel)

# Captains
captain_key <- data %>% 
  count(captain_id, captain)

# Comments
sort(unique(data$comments))


# Plot coords
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
ggplot(data, aes(x=long_dd, y=lat_dd)) +
  geom_sf(data=usa, color="white", fill="grey85", inherit.aes = F) +
  geom_point(shape=1) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim=c(range(data$long_dd, na.rm=T)),
           ylim=c(range(data$lat_dd, na.rm=T))) +
  # Theme
  theme_bw()


# Export data
################################################################################

