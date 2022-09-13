

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

# Read data
data_orig <- read.csv(file.path(indir, "Dive Log Extract_cucumberONLY.csv"), as.is=T, na.strings="")


# Format data
################################################################################

# Bad locations
bad_locations <- c("Lat.  Long.",                        
                   "Lat. ___° __.__' Long.",
                   "__° __.__' Lat.  Long.")

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
         dive_id=dive_number,
         block_id=cdfw_block,
         location=position,
         lat_dd=latitude_dec,
         long_dd=longitude_dec,
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
  # Format location
  mutate(location=stringr::str_trim(location),
         location=ifelse(location %in% bad_locations, "", location)) %>% 
  # Format longitude
  mutate(long_dd=long_dd*-1,
         long_dd=ifelse(long_dd < -150, NA, long_dd),
         long_dd=ifelse(long_dd > -100, NA, long_dd)) %>% 
  # Add port
  left_join(port_key %>% select(port_code, port, port_complex), by=c("port_id"="port_code")) %>%
  # Add species
  left_join(species_key %>% select(spp_code_num, comm_name, sci_name), by=c("species_id"="spp_code_num")) %>%
  # Arrange
  select(logbook_id,
         year, month, date, 
         vessel_combo, vessel_id, vessel, 
         fisher_id, fisher, 
         dealer_id, dealer, receipt_id, 
         port_complex, port, port_id, 
         block_id, location, lat_dd, long_dd, landmark, depth_min_ft, depth_max_ft, 
         species_id_lbs_combo, 
         species_id, comm_name, sci_name,
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

# Species
species_key_check <- data %>% 
  select(species_id, comm_name, sci_name) %>% 
  unique() %>% 
  arrange(species_id)
freeR::which_duplicated(species_key_check$species_id)

# Location
location_key <- data %>% 
  select(location, lat_dd, long_dd) %>% 
  unique() %>% 
  arrange(location)


# Location
###################################################

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
  geom_point(data, mapping=aes(x=long_dd, y=lat_dd, color=comm_name), pch=1, alpha=0.5) +
  # Crop
  coord_sf(x=range(data$long_dd, na.rm=T), y=range(data$lat_dd, na.rm=T)) +
  # Theme
  theme_bw()
g


# Export
###################################################

# Final
data_out <- data %>% 
  select(-vessel_combo)

# Export
saveRDS(data_out, file=file.path(outdir, "CDFW_2000_2020_dive_logbooks.Rds"))


