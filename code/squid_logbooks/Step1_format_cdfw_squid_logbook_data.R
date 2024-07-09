

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
                                sheet="SquidVesselLogs", col_types = "text")

# TO-DO
# Add block id from landings receipts
# Mark reliable GPS points (fall inside block id)
# VERY LOW PRIORITY: Fill in missing vessel names based on other datasets

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(logbook_id=log_serial_number,
         vessel=vessel_name,
         vessel_permit=vessel_permit_number,
         captain=captain_name,
         date=log_date_string,
         time_start=start_time,
         time_end=end_time,
         duration_min=elapsed_time,
         gps_position=set_position,
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
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Convert numeric
  mutate(across(.cols=c(lat_dd, long_dd, sst_f, depth_fa, set_number, duration_min, catch_t), .fns=as.numeric)) %>% 
  # Format duration
  # Assume 0 is NA
  mutate(duration_min=ifelse(duration_min==0, NA, duration_min)) %>% 
  # Add missing longitudes that are available in POSITION column
  mutate(long_dd=case_when(gps_position=="33° 41.53' 111° 27."~ -(111+27/60),
                           gps_position=="34° 02.00' 118° 58."~ -(118+58/60),
                           T ~ long_dd)) %>% 
  # Format latitude
  mutate(lat_dd=ifelse(lat_dd==0, NA, lat_dd)) %>% 
  # Format longitude
  mutate(long_dd=ifelse(long_dd==0, NA, long_dd),
         long_dd=abs(long_dd) *-1) %>% 
  # Format captaid id (uppercase some lowercase L's)
  mutate(captain_id=toupper(captain_id)) %>% 
  # Format permit number (uppercase some lowercase SVT's)
  mutate(vessel_permit=toupper(vessel_permit)) %>% 
  # Arrange
  select(logbook_id,
         vessel_id, vessel, vessel_permit,
         captain_id, captain,
         date, set_number,
         limited_yn, lightboat_id,
         time_start, time_end, duration_min,
         gps_position, lat_dd, long_dd, 
         depth_fa, sst_f, catch_t, bycatch, receipt_ids,
         comments, everything()) %>% 
  mutate(nchar=nchar(bycatch))

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
boxplot(data$duration_min/60)

# Vessels
vessel_key <- data %>% 
  count(vessel_id, vessel)
freeR::which_duplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel)

# Vessel permits
table(data$vessel_permit)

# Lightboats
sort(unique(data$lightboat_id))

# Captains
captain_key <- data %>% 
  count(captain_id, captain)
freeR::which_duplicated(captain_key$captain_id)
freeR::which_duplicated(captain_key$captain)

# Other variables
table(data$set_number) # 0, 20, 99 should not be possible
table(data$limited_yn)
table(data$bycatch) # super complicated - could break out into flat table


# GPS key
gps_key <- data %>% 
  count(gps_position, lat_dd, long_dd)

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
saveRDS(data, file.path(outdir, "CDFW_1999_2022_squid_logbook_data.Rds"))




