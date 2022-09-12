

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
blocks_sf <- wcfish::blocks
blocks <- blocks_sf %>% 
  sf::st_drop_geometry()

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
         lat_dd_orig=latitude_dec,
         long_dd_orig=longitude_dec,
         n_nights=nights_in_water) %>% 
  # Format date
  mutate(date=lubridate::mdy(date),
         month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  # Format fisher id
  mutate(fisher_id=ifelse(fisher_id %in% c("LE END", "L", "LS. C"), NA, fisher_id)) %>% 
  # Format fisher
  # fill missing fisher
  mutate(fisher_last=ifelse(fisher_id=="L04589", "BROOKS", fisher_last),
         fisher_first=ifelse(fisher_id=="L04589", "B", fisher_first)) %>%
  # Format vessel
  mutate(vessel=case_when(vessel=="JUDITH AN II" ~ "JUDITH ANN II",
                          vessel_id=="06901" ~ "C-ROBIN",
                          vessel_id=="01178" ~ "SKIFF",
                          T ~ vessel)) %>% 
  # Format block id
  # Review these decisions carefully
  mutate(block_id=stringr::str_squish(block_id_orig),
         block_id=ifelse(grepl("°", block_id), "", block_id),
         block_id=recode(block_id,
                         "O306"="0306",
                         "O326"="0326",
                         "O345"="0345",
                         "719+"="719",
                         "756'"="756",
                         "'822"="822",
                         ".842"="842",
                         "075+"="",
                         "68/0"="680",
                         "68/8"="688",
                         "68*9"="689",
                         "A0708"="708",
                         "86+1"="861",
                         "RR22"="") %>% as.numeric()) %>%
  # Add block info
  left_join(blocks %>% select(block_id, block_state, block_type)) %>% 
  # Format location
  mutate(location_orig=stringr::str_trim(location_orig)) %>% 
  # Add coordinates in block id to location
  mutate(location_orig=ifelse(grepl("°", block_id), paste(location_orig, "lat.", block_id_orig, "long."), location_orig)) %>% 
  # Format longitude
  mutate(long_dd_orig=long_dd_orig*-1) %>% 
  # Format receipt id
  mutate(receipt_id=stringr::str_trim(receipt_id)) %>% 
  # Arrange
  select(logbook_id, year, month, date, 
         vessel_id, vessel, 
         fisher_id, fisher_last, fisher_first, crew_id,
         depth_ft, 
         block_id_orig, block_id, block_type, block_state, 
         location_orig, lat_dd_orig, long_dd_orig,
         multi_day_yn, n_traps_set, n_traps_pulled, n_nights,
         n_kept, n_released,
         everything())


# Inspect data
################################################################################

# Inspect
str(data)

# Date
range(data$date)
range(data$year)
range(data$month)

# Inspect
table(data$multi_day_yn)

# Log key
# Not sure what to take away here except that a single log can include a bunch of activities
log_key <- data %>% 
  select(logbook_id, year, date, vessel_id, vessel, fisher_id, crew_id, block_id, depth_ft) %>% 
  unique()

# Fisher key
fisher_key <- data %>% 
  select(fisher_id, fisher_last, fisher_first) %>% 
  unique() %>% 
  arrange(fisher_id)
freeR::which_duplicated(fisher_key$fisher_id)

# Vessel key
vessel_key <- data %>% 
  select(vessel_id, vessel) %>% 
  unique() %>% 
  arrange(vessel_id)
freeR::which_duplicated(vessel_key$vessel_id) # there are a few vessel ids with two names

# Block key - many of these blocks are absurdly invalid
block_key <- data %>% 
  select(block_id_orig, block_id, block_type, block_state) %>% 
  unique()

# Numeric block ids that are invalid
block_key %>% 
  filter(!is.na(block_id)) %>% 
  filter(is.na(block_type)) %>% 
  pull(block_id) %>% sort()

# Crew id
# Derive N crew?
# Anyway to check these?
crew_key <- tibble(crew_id=freeR::uniq(data$crew_id))

# Receipt id
# Write a function to check these?
receipt_key <- tibble(receipt_id=freeR::uniq(data$receipt_id))

# Numeric values
boxplot(data$depth_ft)
boxplot(data$n_kept)
boxplot(data$n_released)
boxplot(data$n_nights)
boxplot(data$n_traps_pulled)
boxplot(data$n_traps_set)


# Fix locations
################################################################################

x <- "32°40.17'N"
calc_lat_dd <- function(x){
  lat_out <- purrr::map_dbl(x, function(x){
    lat_list <- strsplit(x, split="°")
    lat1 <- lat_list[[1]][1] %>% as.numeric()
    lat2 <- lat_list[[1]][2] %>% gsub("'N", "", .) %>% as.numeric() 
    lat_out <- lat1 +   lat2/60
  })
  return(lat_out)
}
calc_lat_dd(x)

x <- "120°05.69'W"
calc_long_dd <- function(x){
  long_out <- purrr::map_dbl(x, function(x){
    long_list <- strsplit(x, split="°")
    long1 <- long_list[[1]][1] %>% as.numeric()
    long2 <- long_list[[1]][2] %>% gsub("'W", "", .) %>% as.numeric() 
    long_out <- (long1 +  long2/60) * -1
  })
  return(long_out)
}
calc_long_dd(x)


# Location key
location_key <- data %>% 
  # Unique locations
  select(location_orig, lat_dd_orig, long_dd_orig) %>% 
  unique() %>% 
  arrange(location_orig) %>% 
  # Add location type
  mutate(location_type=ifelse(grepl("°", location_orig) | grepl("32|33|34", location_orig), "lat/long", "place"),
         location_type=ifelse(location_orig=="So Laguna To 33 30 050", "place", location_type)) %>% 
  mutate(location=ifelse(location_type=="place", stringr::str_to_title(location_orig), location_orig), 
         location=location %>% stringr::str_conv(endocid) %>% stringr::str_squish())

# Locations
location_key_dd <- location_key %>% 
  # Reduce to lat/long
  filter(location_type=="lat/long") %>% 
  # Replace a few
  mutate(location=recode(location,
                         "32 43/117 16"="32°43'N, 117°16'W",
                         "32 44/117 15"="32°44'N, 117°15'W",
                         "32 44/117 16"="32°44'N, 117°16'W",
                         "32 45/117 15"="32°45'N, 117°15'W",
                         "32° 29.57' 117"="32°29.57'N, 117°00.00'W",
                         "32° 29.57' 117 17.32"="32°29.57'N, 117°17.32'W",
                         "32° 29.57' 117 17.58"="32°29.57'N, 117°17.58'W",
                         "32° 29.57' 117 1744"="32°29.57'N, 117°17.44'W",
                         "33-56-09 119-57-66"="33°56.09'N, 119°57.66'W",
                         "33-56-49 119-57-66"="33°56.49'N, 119°57.66'W",
                         "33-59-07 120-15-35"="33°59.07'N, 120°15.35'W",
                         "33° 00.34 119 33.48"="33°00.34'N, 119°33.48'W",
                         "33° 42.49' 118.56.25"="33°42.49'N, 118°56.25'W",
                         "33° 42.92' 118 05.79"="33°42.92'N, 118°05.79'W",
                         "33° 43.14' 118 06.28"="33°43.14'N, 118°06.28'W",
                         "33° 43.22' 118.07.14"="33°43.22'N, 118°07.14'W",
                         "34 01.32 119 37.31"="34°01.32'N, 119°37.31'W",
                         "34 57.62 119 47.75"="34°57.62'N, 119°47.75'W",
                         "34-01-23 120-13-58"="34°01-23'N, 120°13.58'W",
                         "34-01-25 120-13-58"="34°01-25'N, 120°13.58'W",
                         "34-03-16 120 04-76"="34°03-16'N, 120°04.76'W",
                         "34-03-16 120-04-76"="34°03-16'N, 120°04.76'W",
                         "340123 1201358"="34°0123'N, 120°13.58'W",
                         "340127 1200476"="34°0127'N, 120°04.76'W",
                         "340316 1200476"="34°0316'N, 120°04.76'W",
                         "33.30.050"="33°30.05'N, NA",
                         "34°02.80'N, 1201040W"="34°02.80'N, 120°10.40'W",
                         "32°41.00'N, 117°15.35W"="32°41.00'N, 117°15.35'W",
                         "32°44.00N, 117°15.00'W"="32°44.00'N, 117°15.00'W",
                         "34°01-23'N, 120°13.58'W"="34°01.23'N, 120°13.58'W",
                         "34°01-25'N, 120°13.58'W"="34°01.25'N, 120°13.58'W",
                         "34°00.03'N, 1192243W"="34°00.03'N, 119°22.43'W", 
                         "34°02.50'N, 1200700W"="34°02.50'N, 120°07.00'W", 
                         "34°02.80'N, 1201040W"="34°02.80'N, 120°10.40'W", 
                         "34°09.36'N, 1191392W"="34°09.36'N, 119°13.92'W",
                         "50 17 50N, 117°17.20'W"="50°17.50'N, 117°17.20'W",
                         "34°27.57N, 120°05.69'W"="34°27.57'N, 120°05.69'W",
                         "34°27.58N, 120°05.71'W"="34°27.58'N, 120°05.71'W",
                         "34°00.03'N,  1192243'W"="34°00.03'N, 119°22.43'W",
                         "34°59.99N, 120°37.31'W"="34°59.99'N, 120°37.31'W",
                         "50 17 50N, 117°17.20'W"="50°17.50'N, 117°17.20'W",
                         "34°02.50'N, 1200700'W"="34°02.50'N, 120°07.00'W",
                         "34°00.03'N, 1192243'W"="34°00.03'N, 119°22.43'W",
                         "34°00.03'N, 1192243'W"="34°00.03'N, 119°22.43'W",
                         "34°02.50'N, 1200700'W"="34°02.50'N, 120°07.00'W",
                         "34°02.80'N, 1201040'W"="34°02.80'N, 120°10.40'W",
                         "34°59.99N,  120°37.31'W"="34°59.99'N, 120°37.31'W",
                         "34°09.36'N, 1191392'W"="34°09.36'N, 119°13.92'W",
                         "50 17 50'N,  117°17.20'W"="50°17.50'N, 117°17.20'W")) %>% 
  # Special overwrise
  mutate(location=case_when(location_orig == "50 17 50 lat. 117° 17.20' long." ~ "50°17.50'N, 117°17.20'W",
                            location_orig == "34° 00.03' lat. 1192243 long." ~ "34°00.03'N, 119°22.43'W",
                            location_orig == "34° 02.50' lat. 1200700 long." ~ "34°02.50'N, 120°07.00'W",
                            location_orig == "34° 02.80' lat. 1201040 long." ~ "34°02.80'N, 120°10.40'W",
                            location_orig == "34° 09.36' lat. 1191392 long." ~ "34°09.36'N, 119°13.92'W",
                            T ~ location)) %>% 
  # Update latitude
  mutate(location=gsub(" lat.", "N, ", location)) %>% 
  mutate(location=ifelse(stringr::str_detect(location, "'$"), gsub("'", "'N", location), location)) %>%
  mutate(location=ifelse(stringr::str_detect(location, "\\dN"), gsub("N", "'N", location), location)) %>%
  # Update longitude
  mutate(location=gsub(" long.", "W", location),
         location=gsub("\\.W", "'W", location)) %>% 
  # Update longitude
  mutate(location=gsub("°W", "°0.00'W", location)) %>% 
  mutate(location=ifelse(stringr::str_detect(location, "\\dW"), gsub("W", "'W", location), location)) %>%
  # Update ones without long
  mutate(location=gsub(", W", ", NA", location)) %>%
  mutate(location=ifelse(stringr::str_detect(location, "'N$"), gsub("'N", "'N, NA", location), location)) %>% 
  # Update ones without lat
  mutate(location=gsub("lat. ", "NA, ", location)) %>% 
  # Eliminate spaces around degree
  mutate(location=gsub("° ", "°", location)) %>% 
  # Replace hyphens
  mutate(location=gsub("-", ".", location)) %>% 
  # Split lat/long
  separate(location, into = c("lat_dms", "long_dms"), sep =", ", remove = F) %>% 
  # Format more
  mutate(long_dms=stringr::str_trim(long_dms)) %>% 
  # Update NAs
  mutate(long_dms=ifelse(long_dms=="NA", NA, long_dms),
         lat_dms=ifelse(lat_dms=="NA", NA, lat_dms)) %>% 
  # Compute
  mutate(lat_dd=calc_lat_dd(lat_dms),
         long_dd=calc_long_dd(long_dms)) %>% 
  # Add missing lat/long from original if available
  mutate(long_dd=ifelse(is.na(long_dd), long_dd_orig, long_dd))


# Format for merge
location_key_dd_simple <- location_key_dd %>% 
  select(location_orig, location_type, location, lat_dd, long_dd)
location_key_place <- location_key %>% 
  filter(location_type=="place") %>% 
  rename(lat_dd=lat_dd_orig, long_dd=long_dd_orig) %>% 
  select(location_orig, location_type, location, lat_dd, long_dd)

# Create final key
location_key_final <- bind_rows(location_key_dd_simple, location_key_place)


# Add location type
################################################################################

# More formatting
data2 <- data %>% 
  # Add location type
  left_join(location_key_final, by="location_orig") %>% 
  # Arrange
  select(logbook_id:location_orig, 
         location, location_type, 
         lat_dd_orig, long_dd_orig, lat_dd, long_dd,
         everything())

# Inspect
str(data2)

# Export
################################################################################

# Export
saveRDS(data2, file=file.path(outdir, "CDFW_2000_2020_logbook_data.Rds"))


# # Add block id
# ################################################################################
# 
# # Convert blocks to sp
# block_sp <- blocks_sf %>% 
#   sf::as_Spatial()
# 
# # Make sp of unique coordinates
# data_xy_sp <- data %>% 
#   filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
#   select(lat_dd, long_dd) %>% 
#   unique() %>% 
#   sf::st_as_sf(., coords=c("long_dd", "lat_dd"), crs=sf::st_crs(blocks_sf)) %>% 
#   sf::as_Spatial()
# 
# # Assgn blocks to unique coordinates
# data_xy_sp_blocks <- sp::over(data_xy_sp, block_sp)
# 
# # Create a block id key
# data_xy_block_key <- data_xy_sp %>% 
#   as.data.frame() %>% 
#   setNames(c("long_dd", "lat_dd")) %>% 
#   cbind(data_xy_sp_blocks %>% select(block_id)) %>% 
#   rename(block_id_latlong=block_id)
# 
# # Add block id of latlong to data
# data2 <- data %>% 
#   # Add block id
#   left_join(data_xy_block_key, by=c("long_dd", "lat_dd")) %>% 
#   # Arrange
#   select(logbook_id:block_id, block_id_latlong, everything())
# 
# 
# 
# # Location
# ###################################################
# 
# # Get land
# usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
# foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
# 
# # Plot 
# g <- ggplot() +
#   # Labels
#   labs(x="", y="") +
#   # Plot land
#   geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
#   geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
#   # Plot points
#   # geom_point(data, mapping=aes(x=long_dd, y=lat_dd), pch=1, color="grey30", alpha=0.5) +
#   geom_point(location_key_dd, mapping=aes(x=long_dd, y=lat_dd), pch=1, color="grey30", alpha=0.5) +
#   # Crop
#   # coord_sf(x=c(-120.5, -119.5), y=c(33.8,34.2)) +
#   coord_sf(x=c(-125, -114), y=c(31.5,42)) +
#   # coord_sf(x=range(data$long_dd, na.rm=T), y=range(data$lat_dd, na.rm=T)) +
#   # Theme
#   theme_bw()
# g
# 
# 






