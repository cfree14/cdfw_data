

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
data_orig <- read.csv(file.path(indir, "old/LogSummary_shrimp-prawn_2024.03.05_Free data request.csv"), 
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
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(logbook_id=log_serial_num,
         vessel_id=vessel_num, 
         vessel=vessel_name,
         port=port_desc,
         date_depart=departure_date,
         date_return=landing_date, 
         date_drag=detail_date,
         tow_number=drag_number,
         depth_fa_set=set_depth,
         depth_fa_up=up_depth,
         block_id=block_number,
         net_type=net_type_desc, 
         receipt_id=ft_num,
         time_set=set_time,
         time_up=up_time,
         duration_min=total_time,
         spp_code_orig=species_id,
         species_orig=market_cat_desc,
         catch_lbs=total_pounds) %>% 
  # Format date
  mutate(date_depart=lubridate::mdy(date_depart),
         date_return=lubridate::mdy(date_return),
         date_tow=lubridate::mdy(date_drag)) %>% 
  # Format species name
  mutate(species=wcfish::convert_names(species_orig, to="regular"),
         species=recode(species,
                        "Butterfish (Pacific pompano)"="Pacific pompano",
                        "Unspecified sanddab"="Sanddab",        
                        "Moon snail"="Moon snails",                 
                        "Unspecified crab rock"="Unspecified rock crab",       
                        "Ocean (pink) shrimp"="Pacific pink shrimp")) %>% 
  # Add species codes
  left_join(spp_key_orig %>% select(spp_code_num, comm_name), by=c("species"="comm_name")) %>% 
  rename(spp_code=spp_code_num) %>% 
  # Calculate lat/long
  mutate(lat_dd_set=set_lat_deg+set_lat_dec/60,
         lat_dd_up=up_lat_dec+up_lat_dec/60,
         long_dd_set=-1*(set_lng_deg+set_lng_dec/60),
         long_dd_up=-1*(up_lng_dec+up_lng_dec/60)) %>% 
  # Format port
  mutate(port=stringr::str_to_title(port), 
         port=recode(port, "Unknown Or Missing Port (Wpd 9-14-94)"="Unknown"),
         port=ifelse(is.na(port), "Unknown", port),
         port_code=ifelse(is.na(port_code), 0, port_code)) %>% 
  # Format vessel id
  mutate(vessel_id=ifelse(vessel_id==".", NA, vessel_id)) %>% 
  # Arrange
  select(logbook_id, vessel_id, vessel, port_code, port, 
         date_depart, date_return, 
         date_tow, tow_number,
         block_id,
         lat_dd_set, long_dd_set,
         lat_dd_up, long_dd_up,
         depth_fa_set, depth_fa_up,
         time_set, time_up, duration_min,
         net_type, headrope_length, 
         spp_code_orig,  species_orig, 
         spp_code, species, 
         catch_lbs,
         everything())


# Inspect
str(data)
freeR::complete(data)

# Species key
spp_key <- data %>% 
  count(spp_code_orig, species_orig, species, spp_code)
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
range(data$date_depart)

# Ports
table(data$port)
table(data$block_id)

# Net type
table(data$net_type)


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
  select(-c(spp_code_orig, species_orig))

# Export data
################################################################################

# Export data
saveRDS(data_out, file.path(outdir, "CDFW_1994_2023_shrimp_prawn_logbook_data.Rds"))

