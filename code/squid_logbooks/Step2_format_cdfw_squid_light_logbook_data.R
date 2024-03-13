

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
data_orig <- readxl::read_excel(file.path(indir, "MarketSquidLogs_ChrisFree_UCSB_DSA_240222.xlsx"), 
                                sheet="SquidLightLogs", col_types = "text")

# High priority
# Date
# Location cleaning

# Low priority
# Could clean up captain names
# Could fill in missing vessel names


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(logbook_id=serial_number,
         vessel=vessel_name,
         vessel_permit=permit_number,
         seiner_id=seiner,
         captain=captain_name,
         date=log_date_string,
         location1=location,
         location2=general_location, 
         location3=location_description,
         time_start=start_time,
         time_end=end_time,
         birds_yn=birds_present,
         mammals_yn=mammals_present,
         duration_min=elapsed_time,
         depth_fa=bottom_depth,
         bycatch_lbs=by_catch, 
         remaining_t=est_tonnage_remaining,
         sold_t=amount_sold,
         bait_t=amt_for_live_bait,
         receipt_ids=landing_receipt) %>% 
  # Format date
  # mutate(date=lubridate::ymd(date)) %>% 
  # Convert numeric
  mutate(across(.cols=c(lat_dd, long_dd, depth_fa, duration_min), .fns=as.numeric)) %>% 
  # Recode location2 before steps below
  mutate(location2=recode(location2, "643, 656"="643/656")) %>% 
  # Add block ids fron location2 column
  mutate(block_id=ifelse(grepl("CDFW Block Code", location2), 
                         gsub("CDFW Block Code ", "", location2), NA)) %>% 
  # Add more block ids from location2 colum
  mutate(block_id=ifelse(grepl("/", location2), location2, block_id)) %>% 
  # Arrange
  select(logbook_id,
         vessel_id, vessel, vessel_permit, seiner_id,
         captain_id, captain,
         date,
         location1, location2, location3, lat_dd, long_dd, depth_fa,
         hours_searching, hours_lighting,
         time_start, time_end, duration_min,
         birds_yn, mammals_yn,
         remaining_t, sold_t, bait_t, bycatch_lbs, receipt_ids,
         comments, everything())

# Inspect
str(data)
head(data)
freeR::complete(data)

# Dates
range(data$date)

# Location key
loc_key <- data %>% 
  count(location2, location3, block_id)

# Depth
boxplot(data$depth_fa)

# Duration
boxplot(data$duration_min/60)

# Vessels
vessel_key <- data %>% 
  count(vessel_id, vessel)
freeR::which_duplicated(vessel_key$vessel_id)

# Captains
captain_key <- data %>% 
  count(captain_id, captain)
freeR::which_duplicated(captain_key$captain_id)

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

# Export data
saveRDS(data, file.path(outdir, "CDFW_1994_2023_squid_logbook_data.Rds"))




