

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/dive_logbooks/raw"
outdir <- "data/confidential/dive_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
species_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
blocks_sf <- wcfish::blocks
blocks <- blocks_sf %>% 
  sf::st_drop_geometry()

# Read data
data_orig <- read.csv(file.path(indir, "Dive Log Extract_cucumberONLY.csv"), as.is=T, na.strings="")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(month=log_group_month,
         year=log_group_year,
         logbook_id=log_serial_num,
         vessel_combo=vessel, 
         fisher_id=permitee_id,
         fisher=permittee_name,
         date=log_date_string,
         block_id=cdfw_block,
         location_orig=position,
         lat_dd_orig=latitude_dec,
         long_dd_orig=longitude_dec,
         depth_min_ft=min_depth_feet,
         depth_max_ft=max_depth_feet,
         hours=diver_hours,
         port_id=port_code, 
         dealer=dealer_name,
         species_id_lbs_combo=dive_species_pounds,
         species_id=species_code,
         catch_lbs=pounds_harvested,
         receipt_id=landing_receipt_number) %>% 
  # Format date
  mutate(date=lubridate::mdy(date)) %>% 
  # Split vessel
  mutate(vessel_combo=stringr::str_squish(vessel_combo),
         vessel_combo=recode(vessel_combo, 
                             "30329"="30329 - WILSON!",
                             "30329 - Wilson!"="30329 - WILSON!")) %>% 
  separate(col=vessel_combo, into=c("vessel_id", "vessel"), sep = "-", remove=F) %>% 
  mutate(across(.cols=c(vessel_id, vessel), stringr::str_trim)) %>%
  mutate(vessel=ifelse(vessel=="", NA, vessel)) %>% 
  # Format fisher id 
  mutate(fisher_id=stringr::str_squish(fisher_id),
         fisher_id=ifelse(is.na(fisher_id), fisher, fisher_id),
         fisher_id=ifelse(!is.na(fisher_id), gsub("L", "", fisher_id) %>% paste0("L", .), fisher_id)) %>% 
  # Format fisher
  mutate(fisher=recode(fisher,
                       "L94862"="TOM - LEWSADDER",
                       'A - KIRYCHENKA'='ALIAKSANDR - KIRYCHENKA',
                       'A - MAKUL'='ANTHONY - MAKUL',
                       'A - RASMUSSEN'='RASMUSSEN - ALEX',
                       'A - SIDENKO'='ALEXANDER - SIDENKO',
                       'B - HARVEY'='BRUCE E - HARVEY',
                       'C - MICHALAK'='CHRIS - MICHALAK',
                       'D - DEMARTINO'='DANNY - DEMARTINO',
                       'D - DIRKSE'='DOUGLAS B - DIRKSE',
                       'D - SCHROEDER'='DON - SCHROEDER',
                       'D - SHAW'='DAVID - SHAW',
                       'D - SOLLENDER'='DEVIN - SOLLENDER',
                       'D - STEPHENS'='DAVE - STEPHENS',
                       "G - TRUMPER"="GARY - TRUMPER",
                       'G - DEXTER'='GLENN J - DEXTER',
                       'G - STAEHLING'='GEORGE - STAEHLING',
                       'G - THOMPSON'='GARY - THOMPSON',
                       'G - WOLLEMANN'='GARY W - WOLLEMANN',
                       'H - LIQUORNIK'='HARRY - LIQUORNIK',
                       'J - DOW'='JOSEPH L Jr. - DOW',
                       'J - EBERHARDT'='JIM - EBERNARDT',
                       'J - MCCLELLAND'='JAMES P - MECLELLAND',
                       'J - SEARS'='JON - SEARS',
                       'J - URQUHART'='JIM - URQUHART',
                       'J - WOODS'='JASON - WOODS',
                       'J - WU'='JOE - WU',
                       "JAMES - BEGLEY"="JAMES M - BEGLEY",
                       'Jeff - Gritsch'='JEFF - GRITSCH',
                       "KENNETH - BOEHCHER"="KENNETH - BOETTCHER",
                       'K - MOCHIZUKI'='KEN - MOCHIZUKI',
                       'K - QUIDER'='KEVIN - QUIDER',
                       "M - ABAN"="MARCOS - A ABAN",
                       'M - FAIR'='MIKE - FAIR',
                       'M - HASTIE'='MARK - HASTIE',
                       'M - KENNY'='MIKE - KENNY',
                       'M - MOORE'='MIKE - MOORE',
                       'M - NEIL'='MIKE - NEIL',
                       'N - HERZIK'='NICHOLAS - HERZIK',
                       'N - ROSSER'='NATHAN - ROSSER',
                       'P - CHAREST'='PIERRE - CHAREST',
                       'R - GREINER'='RAYMOND - GREINER',
                       'R - HARPER'='RICHARD - HARPER',
                       'R - HILL'='RYAN - HILL',
                       'R - KROENER'='RICHARD - KROENER',
                       'S - COOPER'='Stephen - Cooper',
                       'S - LANG'='SAMUEL - LANG',
                       'T - CRYER'='TERRELL E - CRYER',
                       'T - HERZIK'='TERRY - HERZIK',
                       'T - LEWSADDER'='TOM - LEWSADDER',
                       'W - STAEHLING'='WYATT - STAEHLING'),
         fisher=stringr::str_to_title(fisher),
         fisher=gsub("-", "", fisher) %>% stringr::str_squish()) %>% 
  # Format dealer
  mutate(dealer=dealer %>% stringr::str_trim() %>% stringr::str_to_title() ) %>% 
  # Format landmark
  mutate(landmark=stringr::str_to_upper(landmark) %>% stringr::str_squish() %>% stringr::str_trim()) %>% 
  # Format location
  mutate(location_orig=stringr::str_trim(location_orig)) %>% 
  # Format longitude
  mutate(long_dd_orig=long_dd_orig*-1) %>% 
  # Add port
  left_join(port_key %>% select(port_code, port, port_complex), by=c("port_id"="port_code")) %>%
  # Add block info
  left_join(blocks %>% select(block_id, block_state, block_type), by="block_id") %>% 
  # Format species / weight combo
  mutate(species_id_lbs_combo=stringr::str_squish(species_id_lbs_combo),
         species_id_lbs_combo=recode(species_id_lbs_combo,
                                     "| , 752 | 500"="NA | NA, 752 | 500",
                                     "683 | , 757 | 10"="683 | NA, 757 | 10")) %>% 
  # Add species
  left_join(species_key %>% select(spp_code_num, comm_name, sci_name), by=c("species_id"="spp_code_num")) %>%
  # Create dive id
  mutate(dive_id=paste(logbook_id,  date, vessel_id, fisher_id, port_id, block_id, depth_min_ft, depth_max_ft, sep="-")) %>% 
  # Arrange
  select(logbook_id, dive_id,
         year, month, date, 
         vessel_combo, vessel_id, vessel, 
         fisher_id, fisher, 
         dealer_id, dealer, receipt_id, 
         port_complex, port, port_id, 
         block_state, block_type, block_id, 
         location_orig, lat_dd_orig, long_dd_orig, landmark, depth_min_ft, depth_max_ft, 
         species_id_lbs_combo, 
         species_id, comm_name, sci_name,
         dive_number, hours,
         catch_lbs, everything())


# Inspect data
################################################################################

# Inspect
head(data)
str(data)

# Date
table(data$year)
table(data$month)
range(data$date)

# Logbook id
freeR::uniq(data$logbook_id)

# Dive id
dive_key <- data %>% 
  select(dive_id, logbook_id, vessel_id, fisher_id, port_id, block_id, depth_min_ft, depth_max_ft) %>% 
  unique()
freeR::which_duplicated(dive_key$dive_id)

# Vessel
vessel_key <- data %>% 
  select(vessel_combo, vessel_id, vessel) %>% 
  unique() %>% 
  arrange(vessel_id)
freeR::which_duplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel)

# Fisher
fisher_key <- data %>% 
  select(fisher_id, fisher) %>% 
  unique() %>% 
  arrange(fisher_id)
freeR::which_duplicated(fisher_key$fisher_id)
freeR::which_duplicated(fisher_key$fisher)

# Dealer
dealer_key <- data %>% 
  select(dealer_id, dealer) %>% 
  unique() %>% 
  arrange(dealer_id)
freeR::which_duplicated(dealer_key$dealer_id)
freeR::which_duplicated(dealer_key$dealer)

# Port
port_key_check <- data %>% 
  select(port_id, port, port_complex) %>% 
  unique() %>% 
  arrange(port_id)
freeR::which_duplicated(port_key_check$port_id)
freeR::which_duplicated(port_key_check$port)

# Block
block_key <- data %>% 
  select(block_id, block_state, block_type) %>% 
  unique() %>% 
  arrange(block_id)
freeR::which_duplicated(block_key$block_id)

# Species
species_key_check <- data %>% 
  select(species_id, comm_name, sci_name) %>% 
  unique() %>% 
  arrange(species_id)
freeR::which_duplicated(species_key_check$species_id)


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

# Bad locations
bad_locations <- c("Lat.  Long.",                        
                   "Lat. ___° __.__' Long.",
                   "__° __.__' Lat.  Long.")

# Location
location_key <- data %>% 
  # Unique locations
  select(location_orig, lat_dd_orig, long_dd_orig) %>% 
  unique() %>% 
  arrange(location_orig) %>% 
  # Fix locations
  mutate(location=ifelse(location_orig %in% bad_locations, "", location_orig)) %>% 
  # Update invalid
  mutate(location=gsub("Invalid coordina Long.", "invalid", location)) %>% 
  mutate(location=gsub(" Lat.  Long.", "N, invalid", location)) %>% 
  mutate(location=ifelse(stringr::str_detect(location, "^Lat. "), 
                         gsub("Lat. ", "invalid, ", location), location)) %>% 
  # Replace lat/long
  mutate(location=gsub(" Long.", "W", location),
         location=gsub(" Lat.", "N,", location)) %>% 
  # Remove spaces
  mutate(location=gsub("° ", "°",location)) %>% 
  # Add missing 1 to some longitudes
  mutate(location=recode(location,
                         "33°00.02'N, 7°25.06'W"="33°00.02'N, 117°25.06'W", 
                         "34°00.60'N, 19°51.60'W"="34°00.60'N, 119°51.60'W", 
                         "34°03.50'N, 19°54.29'W"="34°03.50'N, 119°54.29'W", 
                         "34°03.13'N, 19°54.34'W"="34°03.13'N, 119°54.34'W", 
                         "34°03.50'N, 19°55.23'W"="34°03.50'N, 119°55.23'W", 
                         "33°23.10'N, 33°28.47'W"="33°23.10'N, 133°28.47'W")) %>% 
  # Split into DMS
  separate(col=location, into=c("lat_dms", "long_dms"), remove=F, sep=", ") %>% 
  # Convert invalids to NA
  mutate(lat_dms=ifelse(lat_dms %in% c("", "invalid"), NA, lat_dms),
         long_dms=ifelse(long_dms %in% c("", "invalid"), NA, long_dms)) %>% 
  # Convert to decimial degrees
  mutate(lat_dd=calc_lat_dd(lat_dms),
         long_dd=calc_long_dd(long_dms))

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
  geom_point(location_key, mapping=aes(x=long_dd, y=lat_dd), pch=1, alpha=0.5) +
  # Crop
  coord_sf(x=range(location_key$long_dd, na.rm=T), y=range(location_key$lat_dd, na.rm=T)) +
  # Theme
  theme_bw()
g


# Add locations
################################################################################

# Add locations
data_xy <- data %>% 
  # Remove vessel combo
  select(-vessel_combo) %>% 
  # Add revised locations and lat/longs
  left_join(location_key %>% select(location_orig, location, lat_dd, long_dd), by="location_orig") %>% 
  # Arrange
  select(logbook_id:long_dd_orig, location, lat_dd, long_dd, everything())


# Split data with multiple species
################################################################################

# ID / weight combos
freeR::uniq(data$species_id_lbs_combo)

# Data with one species
data1 <- data_xy %>% 
  filter(!grepl(",", species_id_lbs_combo))
freeR::uniq(data1$species_id_lbs_combo)

# Data with multiple species
data2 <- data_xy %>% 
  filter(grepl(",", species_id_lbs_combo))
freeR::uniq(data2$species_id_lbs_combo)

# Expand data with multiple species
x <- 1
data2_exp <- purrr::map_df(1:nrow(data2), function(x){
  
  # Row do
  row_do <- data2[x,]
  
  # Species id / weight combo
  id_lbs <- row_do$species_id_lbs_combo
  id_lbs_vec <- strsplit(id_lbs, split=", ") %>% unlist()
  
  # Loop through vec
  y <- 1
  row_out <- purrr::map_df(1:length(id_lbs_vec), function(y){
    
    # Get values
    id_lbs_do <- id_lbs_vec[y]
    id <- strsplit(id_lbs_do, split=" ")[[1]][1] %>% ifelse(.=="NA", NA, .) %>% as.numeric()
    lbs <- strsplit(id_lbs_do, split=" ")[[1]][3] %>% ifelse(.=="NA", NA, .) %>% as.numeric()
    
    # Make row
    sub_row_out <- row_do
    sub_row_out$species_id <- id
    sub_row_out$comm_name <- NA
    sub_row_out$sci_name <- NA
    sub_row_out$catch_lbs <- lbs
    
    # Return
    sub_row_out
    
  })
  
  # Return
  row_out
  
})

# Add sci/comm names
table(data2_exp$species_id)
data2_exp_corr <- data2_exp %>% 
  mutate(comm_name=recode(species_id,
                          "683"="Keyhole limpet",
                          "731"="Kellet's whelk",
                          "747"="Top snail",
                          "752"="Red sea urchin",
                          "757"="Warty sea cucumber")) %>% 
  mutate(sci_name=recode(comm_name, 
                         "Keyhole limpet"="Megathura crenulata",
                         "Kellet's whelk"="Kelletia kelletii",
                         "Top snail"="Megastraea undosa",
                         "Red sea urchin"="Mesocentrotus franciscanus",
                         "Warty sea cucumber"="Parastichopus parvimensis"))

# Merge data
data_out <- bind_rows(data1, data2_exp_corr) %>% 
  arrange(date, vessel_id, logbook_id)


# Export
################################################################################

# Export
saveRDS(data_out, file=file.path(outdir, "CDFW_2000_2020_dive_logbooks.Rds"))


