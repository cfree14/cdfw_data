

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/shrimp_prawn_logbooks/raw"
outdir <- "data/confidential/shrimp_prawn_logbooks/processed"

# Read data
data_orig <- read.csv(file.path(indir, "LogSummary_shrimp-prawn_2024.03.29_Free data request_revised 1982-2023.csv"), 
                      na.strings = "")

# Read species key
spp_key_orig <- readRDS("data/public/cdfw_keys/processed/CDFW_species_key.Rds")

# HIGH PRIORITY
# Convert LORAN to latlong
# Mark GPS reliability

# LOW PRIORITY
# Fill in missing vessel names/ids from another dataset


# Helper functions
################################################################################

# Function to calculate the decimal hour of a day
time_vec <- c("18:15", "07:30", "12:45")
convert_to_decimal_hour <- function(time_str) {
  # Split the time string into hours and minutes
  time_parts <- strsplit(time_str, ":")
  
  # Convert hours and minutes to numeric and calculate decimal hour for each time
  decimal_hours <- sapply(time_parts, function(tp) {
    hours <- as.numeric(tp[1])
    minutes <- as.numeric(tp[2])
    hours + minutes / 60
  })
  
  return(decimal_hours)
}
convert_to_decimal_hour(time_vec)

# Calculate durations
hr1 <- 6.2; hr2 <- 12.5
hr1 <- 23; hr2 <- 2
calc_duration <- function(hr1, hr2){
  
  # Duration when hour2 is on the same day
  duration1 <- (hr2 - hr1) * 60
  
  # Duration when hour2 is on the next day
  duration2 <- (24-hr1) + hr2
  
  # Select the right one
  out <- ifelse(hr2>hr1, duration1, duration2)
  
  return(out)
  
}

# Format data
################################################################################

# Format data
colnames(data_orig)
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(logbook_id=logserialnum,
         date_depart=departuredate,
         date_return=landingdate, 
         date_tow=detaildate,
         year_orig=oldyear,
         vessel_id=vessel_num, 
         vessel=vessel_name,
         port=port_desc,
         tow_number=dragnumber,
         depth_fa_set=setdepth,
         depth_fa_up=updepth,
         block_id=blocknumber,
         net_type=net_type_desc, 
         time_set=settime,
         time_up=uptime,
         duration_min_orig=totaltime,
         spp_code=species_code,
         species_orig=market_cat_desc,
         catch_lbs=totalpounds,
         comments1=tbl_log_comments,
         comments2=tbl_log_detail_comments,
         # Coordinates
         set_lat_deg=setlatdeg,             
         set_lat_dec=setlatdec,             
         set_long_deg=setlngdeg,             
         set_long_dec=setlngdec,             
         up_lat_deg=uplatdeg,              
         up_lat_dec=uplatdec,             
         up_long_deg=uplngdeg,              
         up_long_dec=uplngdec,             
         set_loran_x=setlorancx,            
         set_loran_y=setlorancy,            
         set_loran_w=setlorancw,            
         up_loran_x=uplorancx,            
         up_loran_y=uplorancy,             
         up_loran_w=uplorancw,             
         set_loran_amin=setloranamin,          
         set_loran_amax=setloranamax,          
         up_loran_amin=uploranamin,           
         up_loran_amax=uploranamax) %>% 
  # Format port
  mutate(port=stringr::str_to_title(port),
         port=ifelse(port=="Unknown Or Missing Port (Wpd 9-14-94)", NA, port),
         port_code=ifelse(port==0, NA, port_code)) %>% 
  # Format vessel id
  mutate(vessel_id=ifelse(vessel_id %in% c(".", "0"), NA, vessel_id)) %>% 
  # Format vessel name
  mutate(vessel=ifelse(vessel=="Unknown/ not reported", NA, vessel)) %>% 
  # Add species name
  left_join(spp_key_orig %>% select(spp_code_num, comm_name), by=c("spp_code"="spp_code_num")) %>% 
  rename(species=comm_name) %>% 
  # Format dates
  mutate(date_depart=lubridate::mdy(date_depart),
         date_return=lubridate::mdy(date_return),
         date_tow=lubridate::mdy(date_tow)) %>% 
  # Add year
  mutate(year=ifelse(!is.na(date_tow), lubridate::year(date_tow),
                     ifelse(!is.na(date_return), lubridate::year(date_return), lubridate::year(date_depart))),
         year=ifelse(is.na(year), year_orig, year)) %>% 
  # Fix a longitude
  mutate(set_long_deg=ifelse(set_long_dec==11954.37, 119, set_long_deg),
         set_long_dec=ifelse(set_long_dec==11954.37, 54.37, set_long_dec)) %>%  
  # Calculate lat/long
  mutate(lat_dd_set=set_lat_deg+set_lat_dec/60,
         lat_dd_up=up_lat_deg+up_lat_dec/60,
         long_dd_set=(set_long_deg+set_long_dec/60)*-1,
         long_dd_up=(up_long_deg+up_long_dec/60)*-1) %>% 
  # Replace 0s with NAs
  mutate(lat_dd_set=ifelse(lat_dd_set==0, NA, lat_dd_set),
         lat_dd_up=ifelse(lat_dd_up==0, NA, lat_dd_up),
         long_dd_set=ifelse(long_dd_set==0, NA, long_dd_set),
         long_dd_up=ifelse(long_dd_up==0, NA, long_dd_up)) %>% 
  # Format durations
  mutate(duration_min_orig=ifelse(duration_min_orig<0, NA, duration_min_orig)) %>% 
  # Convert times
  mutate(time_set_num=convert_to_decimal_hour(time_set),
         time_up_num=convert_to_decimal_hour(time_up)) %>% 
  # Calculate duration
  mutate(duration_min=calc_duration(time_set_num, time_up_num),
         duration_min=ifelse(is.na(duration_min), duration_min_orig, duration_min)) %>% 
  # Fix tow numbers
  # 1327: logbook #56621 - tow 6 (13:27)
  # 1338: logbook #56648 - tow 5 (13:38), 
  # 1339: logbook #56648 - tow 6 (15:27)
  # 1420: logbook #12561 - tow 3 (14:20)
  mutate(tow_number=case_when(tow_number==1327 ~ 6,
                              tow_number==1338 ~ 5,
                              tow_number==1339 ~ 6,
                              tow_number==1420 ~ 3,
                              T ~ tow_number)) %>% 
  # Arrange
  select(logbook_id, vessel_id, vessel, port_code, port, 
         year, year_orig, date_depart, date_return, 
         date_tow, tow_number, tow_id,
         block_id,
         lat_dd_set, long_dd_set,
         lat_dd_up, long_dd_up,
         depth_fa_set, depth_fa_up,
         time_set, time_up, 
         time_set_num, time_up_num,
         duration_min_orig, duration_min,
         net_type, 
         spp_code,  species_orig, species,
         catch_lbs,
         comments1, comments2,
         everything()) 


# Inspect
str(data)
freeR::complete(data)

# Species key
spp_key <- data %>% 
  count(spp_code, species_orig, species)
freeR::which_duplicated(spp_key$spp_code)

# Vessel
# Danita does not have a vessel id
vessel_key <- data %>% 
  count(vessel_id, vessel)
freeR::which_duplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel)

# Ports
port_key <- data %>% 
  count(port_code, port)

# Dates
range(data$date_tow, na.rm=T)
range(data$date_return, na.rm=T)
range(data$date_depart, na.rm=T)

# Ports
table(data$port)
table(data$block_id)

# Net type
table(data$net_type)

# Tow number
table(data$tow_number)

# Time key
time_key_set <- data %>% 
  count(time_set, time_set_num)
time_key_up <- data %>% 
  count(time_up, time_up_num)

# Duration check
plot(duration_min ~ duration_min_orig, data)

# GPS key
gps_key <- data %>% 
  count(lat_dd_set, set_lat_deg, set_lat_dec,
        long_dd_set, set_long_deg, set_long_dec)

# LORAN key
loran_key <- data %>% 
  count(set_loran_x, set_loran_y, set_loran_w,            
        up_loran_x, up_loran_y, up_loran_w)

# Location key
loc_key <- data %>% 
  select(lat_dd_set, long_dd_set, set_loran_x, set_loran_y,   set_loran_w) %>% 
  unique()


# Map
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
ggplot(data, aes(x=long_dd_set, y=lat_dd_set, color=port)) +
  geom_sf(data=usa, fill="grey85", color="white", inherit.aes = F) +
  geom_point() +
  # coord_sf(xlim=range(data$long_dd_set, na.rm=T),
  #          ylim=range(data$lat_dd_set, na.rm=T)) +
  coord_sf(xlim=c(-116, -128),
           ylim=c(32, 44)) +
  theme_bw()

# Comments
freeR::uniq(data$comments1)
freeR::uniq(data$comments2)



# # LORAN-C conversions
# ################################################################################
# 
# # Get LORAN-C Coordinates
# loran_test <- wcfish::loran_to_gps(chain=9940,
#                                    loran_x=loran_key$set_loran_x[1:30], 
#                                    loran_w=loran_key$set_loran_w[1:30])
# 
# # Map results
# usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
# ggplot(loran_test, aes(x=long_dd, y=lat_dd)) +
#   geom_sf(data=usa, fill="grey85", color="white", inherit.aes = F) +
#   geom_point() +
#   coord_sf(xlim=range(loran_test$long_dd, na.rm=T),
#            ylim=range(loran_test$lat_dd, na.rm=T)) +
#   # coord_sf(xlim=c(-116, -128),
#   #          ylim=c(32, 44)) +
#   theme_bw()


# Prep for export
################################################################################

# Data out
data_out <- data %>% 
  # Remove useless columns
  select(-c(species_orig, year_orig, duration_min_orig)) %>% 
  # Remove useless lat/long columns
  select(-c(set_lat_deg, set_lat_dec,
            set_long_deg, set_long_dec,
            up_lat_deg, up_lat_dec,
            up_long_deg, up_long_dec)) %>%
  # Remove these columns because they are empty
  select(-c(set_loran_amin, set_loran_amax, up_loran_amin, up_loran_amax)) %>% 
  # Move LORAN Y values into W column then remove Y columns
  mutate(set_loran_w=ifelse(is.na(set_loran_w), set_loran_y, set_loran_w),
         up_loran_w=ifelse(is.na(up_loran_w), up_loran_y, up_loran_w)) %>% 
  select(-c(set_loran_y, up_loran_y))
  

# Inspect
freeR::complete(data_out)


# Export data
################################################################################

# Export data
saveRDS(data_out, file.path(outdir, "CDFW_1982_2023_shrimp_prawn_logbook_data.Rds"))

