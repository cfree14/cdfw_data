

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
# Harmonize location codes/names/blocks

# Low priority
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
         captain_orig=captain_name,
         date=log_date_string,
         gps_position=location,
         location_code=general_location, 
         location_long=location_description,
         time_start=start_time,
         time_end=end_time,
         birds_yn=birds_present,
         mammals_yn=mammals_present,
         duration_min=elapsed_time,
         depth_fa=bottom_depth,
         bycatch_lbs=by_catch, 
         remaining_t=est_tonnage_remaining,
         sold_t=amount_sold,
         bait_lbs=amt_for_live_bait,
         receipt_ids=landing_receipt) %>% 
  # Format date
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Convert numeric
  mutate(across(.cols=c(lat_dd, long_dd, depth_fa, 
                        hours_searching, hours_lighting, duration_min, 
                        remaining_t, sold_t, bait_lbs), .fns=as.numeric)) %>% 
  # Format duration
  mutate(duration_min=ifelse(duration_min==0, NA, duration_min)) %>% 
  # Format captain
  mutate(captain_orig=case_when(captain_orig=="," ~ NA,
                           T ~ captain_orig)) %>% 
  mutate(captain_orig=gsub("SUSIBLAIR|SUSI BLAIR", "SUSI-BLAIR", captain_orig)) %>% 
  # Format permit number
  mutate(vessel_permit=toupper(vessel_permit) %>% gsub("`", "", .)) %>% 
  mutate(vessel_permit=ifelse(vessel_permit=="LBT", NA, vessel_permit)) %>% 
  # Format receipt ids
  mutate(receipt_ids=toupper(receipt_ids)) %>% 
  mutate(receipt_ids=ifelse(receipt_ids=="0", NA, receipt_ids)) %>% 
  # Format long location
  mutate(location_long=toupper(location_long),
         location_long=gsub("\\?", "", location_long)) %>% 
  # Fix long location
  mutate(location_long=case_when(location_code=="CA-OR" ~ "SANTA CATALINA ISLAND-ORANGE ROCKS",
                                 location_code=="CR-BK" ~ "SANTA CRUZ ISLAND-BK",
                                 T ~ location_long)) %>% 
  # Format GPS position
  mutate(gps_position=case_when(gps_position=="Lat.  Long." ~ NA,
                                T ~ gps_position)) %>% 
  # Add GPS position type
  mutate(gps_position_type=case_when(grepl("°", gps_position) ~ "GPS position",
                                     nchar(gps_position)==3 ~ 'Block id',
                                     T ~ NA)) %>% 
  # Format location code (before steps below)
  mutate(location_code=recode(location_code, "643, 656"="643/656")) %>% 
  # Extract block ids from location code column
  mutate(block_id=ifelse(grepl("CDFW Block Code", location_code), 
                         gsub("CDFW Block Code ", "", location_code), NA)) %>% 
  mutate(block_id=ifelse(grepl("/", location_code), location_code, block_id)) %>% 
  # Remove blocks ids from location code column
  mutate(location_code=ifelse(grepl("CDFW Block Code|/", location_code), NA, location_code)) %>% 
  # Finish location code cleaning
  mutate(location_code=gsub("_", "-", location_code)) %>% 
  # Extract blocks from GPS position
  mutate(block_id=ifelse(gps_position_type=="Block id", gps_position, block_id)) %>% 
  # Remove blocks for GPS position
  mutate(gps_position=ifelse(gps_position_type=="Block id", NA, gps_position)) %>% 
  select(-gps_position_type) %>% 
  # Correct a few block ids
  mutate(block_id=as.numeric(block_id),
         block_id=case_when(location_code=="CA-OR" ~ 761,
                            location_code=="CR-CH" ~ 685,
                            T ~ block_id)) %>% 
  # Fill missing location codes
  group_by(location_long) %>% 
  fill(location_code, .direction="updown") %>% 
  ungroup() %>% 
  # Fill missing block ids
  group_by(location_code) %>% 
  fill( block_id, .direction="updown") %>% 
  ungroup() %>% 
  # Convert longitude
  mutate(long_dd=long_dd * -1) %>% 
  # Replace lat/long==0 with NA
  # Overwrite invalid lat/long
  mutate(lat_dd=case_when(lat_dd==0 ~ NA, 
                          grepl("Invalid coordina Lat.", gps_position) ~ NA,
                          lat_dd > 1000 ~ NA,
                          lat_dd > 42 ~ NA,
                          lat_dd < 10 ~ NA,
                          lat_dd < 30 ~ NA,
                          T ~ lat_dd),
         long_dd=case_when(long_dd==0 ~ NA, 
                           grepl("Invalid coordina Long.", gps_position) ~ NA,
                           long_dd < -1000 ~ NA,
                           long_dd < -125 ~ NA,
                           long_dd > -10 ~ NA,
                           T ~ long_dd)) %>%
  # Arrange
  select(logbook_id,
         vessel_id, vessel, vessel_permit, seiner_id,
         captain_id, captain_orig,
         date,
         gps_position, location_code, location_long, block_id,
         lat_dd, long_dd, depth_fa,
         hours_searching, hours_lighting,
         time_start, time_end, duration_min,
         birds_yn, mammals_yn,
         remaining_t, sold_t, bait_lbs, bycatch_lbs, receipt_ids,
         comments, everything())

# Inspect
str(data)
head(data)
freeR::complete(data)
(freeR::complete(data) / nrow(data) * 100) %>% round(2)

# Dates
range(data$date)

# Location codes
sort(unique(data$location_code))

# Location long names
sort(unique(data$location_long))

# Location key
loc_key1 <- data %>% 
  count(location_code, location_long, block_id)

# Location key
loc_key2 <- data %>% 
  count(location_code, location_long, block_id, gps_position, lat_dd, long_dd)

# Depth
boxplot(data$depth_fa)

# Duration
boxplot(data$duration_min/60)

# Vessels
vessel_key <- data %>% 
  count(vessel_id, vessel)
freeR::which_duplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel)

# Vessel permit
sort(unique(data$vessel_permit))

# Seiner id
sort(unique(data$seiner_id))

# Captains
captain_key <- data %>% 
  count(captain_id, captain_orig)
freeR::which_duplicated(captain_key$captain_id)

# Comments
sort(unique(data$comments))

# Other
table(data$birds_yn)
table(data$mammals_yn)
sort(unique(data$receipt_ids))

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


# Build location key
################################################################################

# Build location key
loc_key <- data %>% 
  # Unique
  group_by(location_code, location_long) %>% 
  summarize(lat_dd=median(lat_dd, na.rm=T),
            long_dd=median(long_dd, na.rm=T),
            block_ids=paste(unique(block_id) %>% na.omit(), collapse=", ")) %>% 
  ungroup()

freeR::which_duplicated(loc_key$location_code)
freeR::which_duplicated(loc_key$location_long)

# Export key
write.csv(loc_key, file=file.path(outdir, "squid_logbook_location_key.csv"), row.names = F)


# Update captain info
################################################################################

# Captains
captain_key1 <- data %>% 
  count(captain_id, captain_orig) %>% 
  filter(!is.na(captain_orig)) %>% 
  # Separate last/first
  separate(captain_orig, into=c("last", "first"), sep=", ") %>% 
  # Count length of first
  mutate(first_nchar=nchar(first)) %>% 
  # Retain longest first name
  arrange(captain_id, last, desc(first_nchar)) %>% 
  group_by(captain_id, last) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Merge names
  mutate(captain=paste(last, first, sep=", ")) %>% 
  # Simplify
  select(captain_id, captain) %>% 
  # Remove the ID with two names
  filter(captain_id!="L91239")
freeR::which_duplicated(captain_key1$captain_id)
freeR::which_duplicated(captain_key1$captain)

# Add captian name to data
data2 <- data %>% 
  # Add corrected captain name
  left_join(captain_key1, by="captain_id") %>% 
  # Move captain name
  relocate(captain, .before=captain_orig) %>% 
  # Use original if missing
  mutate(captain=ifelse(is.na(captain), captain_orig, captain)) %>% 
  select(-captain_orig)

# Check captain key
captain_check <-  data2 %>% 
  count(captain_id, captain)
freeR::which_duplicated(captain_check$captain_id)
freeR::which_duplicated(captain_check$captain)


# Export data
################################################################################

# Export data
saveRDS(data2, file.path(outdir, "CDFW_2000_2022_squid_lightboat_logbook_data.Rds"))




