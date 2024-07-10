

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
# Finalize GPS coordinates
# Mark lat/longs in block (derive block id)

# LOW PRIORITY
# Ask about units for head rope length?
# Fill in missing vessel names/ids from another dataset


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
         duration_min=totaltime,
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
  # Format dates,
  mutate(date_depart=lubridate::mdy(date_depart),
         date_return=lubridate::mdy(date_return),
         date_tow=lubridate::mdy(date_tow)) %>% 
  # Add year
  mutate(year=ifelse(!is.na(date_tow), lubridate::year(date_tow),
                     ifelse(!is.na(date_return), lubridate::year(date_return), lubridate::year(date_depart))),
         year=ifelse(is.na(year), year_orig, year)) %>% 
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
  # Arrange
  select(logbook_id, vessel_id, vessel, port_code, port, 
         year, year_orig, date_depart, date_return, 
         date_tow, tow_number,
         block_id,
         lat_dd_set, long_dd_set,
         lat_dd_up, long_dd_up,
         depth_fa_set, depth_fa_up,
         time_set, time_up, duration_min,
         net_type, 
         spp_code,  species_orig, species,
         catch_lbs,
         comments1, comments2,
         everything()) %>% 
  # Remove useless lat/long columns
  select(-c(set_lat_deg, set_lat_dec,
            set_long_deg, set_long_dec,
            up_lat_deg, up_lat_dec,
            up_long_deg, up_long_dec)) %>% 
  # Remove these columns because they are empty
  select(-c(set_loran_amin, set_loran_amax, up_loran_amin, up_loran_amax))

# Inspect
str(data)
freeR::complete(data)

# Inspect
str(data)
freeR::complete(data)

# Species key
spp_key <- data %>% 
  count(spp_code, species_orig, species)
freeR::which_duplicated(spp_key$spp_code)

# Vessel
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
  coord_sf(xlim=range(data$long_dd_set, na.rm=T),
           ylim=range(data$lat_dd_set, na.rm=T)) +
  theme_bw()




# Prep for export
################################################################################

# Data out
data_out <- data %>% 
  # Remove useless columns
  select(-c(species_orig))

# Export data
################################################################################

# Export data
saveRDS(data_out, file.path(outdir, "CDFW_1982_2023_shrimp_prawn_logbook_data.Rds"))

