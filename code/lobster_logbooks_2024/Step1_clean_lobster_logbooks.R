

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/lobster_logbooks_2024/raw"
outdir <- "data/confidential/lobster_logbooks_2024/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
species_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
blocks_sf <- wcfish::blocks
blocks <- blocks_sf %>% 
  sf::st_drop_geometry()

# Read data
data_orig <- read.csv(file.path(indir, "Free_LobsterLogbookData_1980-2022_240121.csv"), as.is=T, na.strings=c("", "NA"))


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
         receipt_ids=landing_receipt, 
         crew_ids=crew_member_id,
         location_orig=trap_location,
         lat_dd_orig=latitude_dec,
         long_dd_orig=longitude_dec,
         n_nights=nights_in_water) %>% 
  # Format date
  mutate(date=lubridate::mdy(date),
         month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  # Format vessel id
  mutate(vessel_id=case_when(vessel_id==0 ~ NA,
                             T ~ vessel_id)) %>% 
  # Format vessel
  mutate(vessel=toupper(vessel)) %>% 
  # Format block id
  mutate(block_id=case_when(block_id==0.842 ~ 842,
                            block_id %in% c(3860, 3900, 6530, 7230, 
                                            8460, 8520, 8600, 8850, 8860, 8960,
                                            7110, 7520, 8630,  8650,  8660, 8760) ~ block_id/10,
                            T ~ block_id)) %>% 
  # Add block info
  left_join(blocks %>% select(block_id, block_state, block_type), by="block_id") %>% 
  # Format longitude
  mutate(long_dd_orig=long_dd_orig*-1) %>% 
  # Format fisher name
  mutate(fisher_last=toupper(fisher_last) %>% stringr::str_trim(.),
         fisher_first=toupper(fisher_first) %>% stringr::str_trim(.)) %>% 
  # Format reciept id
  mutate(receipt_ids=gsub("\\.,|\\)", "", receipt_ids) %>% stringr::str_trim(.) %>% toupper(.),
         receipt_ids=ifelse(receipt_ids %in% c("", "."), NA, receipt_ids)) %>% 
  # Format crew ids
  # Squish, remove commas and underscores, add commas back in
  mutate(crew_ids=stringr::str_squish(crew_ids) %>% gsub(",|_", "", .) %>% gsub(" ", ", ", .) %>% toupper(.),
         crew_ids=case_when(crew_ids=="" ~ NA,
                            crew_ids=="NAN" ~ NA,
                            T ~ crew_ids)) %>% 
  # Add number of crew
  mutate(ncrew=nchar(gsub("[^,]", "", crew_ids))+1) %>% 
  # Format location
  mutate(location=stringr::str_squish(location_orig) %>% toupper(.),
         location=gsub("� ", "°", location),
         location=gsub("�", "°", location),
         location=case_when(location %in% c(".", '" "', '"', "?", "???") ~ NA,
                            T ~ location),
         location=gsub(" LAT.", "N", location),
         location=gsub(" LONG.", "W", location),
         location=gsub("NW", "N", location),
         location=gsub("LONG.", "W", location),
         location=recode(location,
                         "32 43/117 16" = "32°43'N 117°16'W",      
                         "32 44/117 15" = "32°44'N 117°15'W",       
                         "32 44/117 16" = "32°44'N 117°16'W",       
                         "32 45/117 15" = "32°45'N 117°15'W",
                         "33-56-09 119-57-66" = "33°56.09'N 119°57.66'W",        
                         "33-56-49 119-57-66" = "33°56.49'N 119°57.66'W",          
                         "33-59-07 120-15-35" = "33°59.07'N 120°15.35'W",           
                         "33.30.050" = "33°30.05'N",
                         "34 01.32 119 37.31" = "34°01.32'N 119°37.31'W", 
                         "34 57.62 119 47.75" = "34°57.62'N 119°47.75'W", 
                         "34-01-23 120-13-58" = "34°01.23'N 120°13.58'W",
                         "34-01-25 120-13-58" = "34°01.25'N 120°13.58'W", 
                         "34-03-16 120 04-76" = "34°03.16'N 120°04.76'W", 
                         "34-03-16 120-04-76" = "34°03.16'N 120°04.76'W", 
                         "340123 1201358" = "34°01.23 120°13.58'W",     
                         "340127 1200476" = "34°01.27 120°04.76'W",     
                         "340316 1200476" = "34°03.16 120°04.76'W",
                         "50 17 50N 117°17.20'W"="50°17.50'N 117°17.20'W",
                         "33°42.49' 118.56.25" = "33°42.49' 118°56.25'W",
                         "33°43.22' 118.07.14" = "33°43.22' 118°07.14'W",
                         "34°00.03'N 1192243W" = "34°00.03'N 119°22.43'W",
                         "34°02.50'N 1200700W" = "34°02.50'N 120°07.00'W",
                         "34°02.80'N 1201040W" = "34°02.80'N 120°10.40'W",
                         "34°09.36'N 1191392W" = "34°09.36'N 119°13.92'W")) %>% 
  # Mark location type
  mutate(location_type=ifelse(grepl("°", location), "GPS position", "Landmark")) %>% 
  # Arrange
  select(logbook_id, year, month, date, 
         vessel_id, vessel, 
         fisher_id, fisher_last, fisher_first, ncrew, crew_ids,
         depth_ft, 
         block_id, block_type, block_state, 
         location_type, location_orig, location, lat_dd_orig, long_dd_orig,
         multi_day_yn, n_traps_set, n_traps_pulled, n_nights,
         n_kept, n_released,
         everything())


# Inspect data
################################################################################

# Inspect
str(data)
freeR::complete(data)

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
freeR::which_duplicated(fisher_key$fisher_id)
sort(unique(fisher_key$fisher_last))

# Vessel key
vessel_key <- data %>% 
  select(vessel_id, vessel) %>% 
  unique() %>% 
  arrange(vessel_id)
freeR::which_duplicated(vessel_key$vessel_id) # there are a few vessel ids with two names

# Block key - many of these blocks are absurdly invalid
block_key <- data %>% 
  select(block_id, block_type, block_state) %>% 
  unique()

# Numeric block ids that are invalid
block_key %>% 
  filter(is.na(block_type)) %>% 
  pull(block_id) %>% sort()

# Crew id
# Anyway to check these?
crew_key <- data %>% 
  count(crew_ids, ncrew)

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
    lat2 <- lat_list[[1]][2] %>% gsub("'|N|W", "", .) %>% as.numeric() 
    lat_out <- lat1 +   lat2/60
  })
  return(lat_out)
}
calc_lat_dd(x)

x <- "120°05.69'W"
x <- "118°W"
calc_long_dd <- function(x){
  long_out <- purrr::map_dbl(x, function(x){
    long_list <- strsplit(x, split="°")
    long1 <- long_list[[1]][1] %>% as.numeric()
    long2 <- long_list[[1]][2] %>% gsub("'|W", "", .) %>% as.numeric() 
    if(!is.na(long2)){
      long_out <- (long1 +  long2/60) * -1
    }else{
      long_out <- long1 * -1
    }
  })
  return(long_out)
}
calc_long_dd(x)

# Locations
freeR::uniq(data$location)

# Location key
loc_key <- data %>% 
  count(location_type, location_orig, location, lat_dd_orig, long_dd_orig)
loc_key$location[loc_key$location_type=="Landmark" & grepl("32|34|33", loc_key$location)]

# Format locations
loc_key2 <- loc_key %>% 
  filter(location_type=="GPS position") %>% 
  select(location_orig, location) %>% 
  separate(location, into=c("lat_dms", "long_dms"), sep=" ", remove=F) %>% 
  mutate(lat_dd=calc_lat_dd(lat_dms),
         long_dd=calc_long_dd(long_dms))

loc_key2$location[!is.na(loc_key2$long_dms) & is.na(loc_key2$long_dd)]

# Format for merge
loc_key3 <- loc_key2 %>% 
  select(location, lat_dd, long_dd) %>% 
  unique()


# Add locations
################################################################################

# Add data
data2 <- data %>% 
  # Add locations
  left_join(loc_key3, by="location") %>% 
  # Erase invalid coords
  mutate(long_dd=ifelse(abs(long_dd)>180, NA, long_dd),
         lat_dd=ifelse(abs(lat_dd)>90, NA, lat_dd)) %>% 
  # Fill in with original where possible
  mutate(lat_dd=ifelse(!is.na(lat_dd), lat_dd, lat_dd_orig),
         long_dd=ifelse(!is.na(long_dd), long_dd, long_dd_orig)) %>% 
  # Relocate
  relocate(lat_dd, long_dd, .after=lat_dd_orig) %>% 
  # Remove original lat/long b/c they basically don't exist
  select(-c(long_dd_orig, lat_dd_orig)) %>% 
  # Add block id
  mutate(block_id_gps=wcfish::block_from_gps(lat_dd=lat_dd, long_dd=long_dd)) %>% 
  # Mark reliability
  mutate(gps_reliable_yn=block_id==block_id_gps) %>% 
  # Relocate
  relocate(block_id_gps, .after=block_id) %>% 
  relocate(gps_reliable_yn, .after=long_dd)



# Export
################################################################################

# Export
saveRDS(data2, file=file.path(outdir, "CDFW_1980_2022_lobster_logbook_data.Rds"))







