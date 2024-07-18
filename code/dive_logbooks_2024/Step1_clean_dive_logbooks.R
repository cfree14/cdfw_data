

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/dive_logbooks_2024/raw"
outdir <- "data/confidential/dive_logbooks_2024/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
species_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
blocks_sf <- wcfish::blocks
blocks <- blocks_sf %>% 
  sf::st_drop_geometry()

# Read data
data_orig <- readxl::read_excel(file.path(indir, "MLS_Dive_log_DSA_CFree_240322.xlsx"), sheet=1, col_types = "text")


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
         species_id=species_code,
         catch_lbs=pounds_harvested,
         receipt_id=landing_receipt_number) %>% 
  # Format date
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Convert to numeric
  mutate(across(.cols=c(species_id, dive_number, hours, 
                        depth_min_ft, depth_max_ft, lat_dd_orig, long_dd_orig, catch_lbs), as.numeric)) %>% #  port_id,   
  # Split vessel id and name
  mutate(vessel_combo=toupper(vessel_combo) %>% stringr::str_squish(.),
         vessel_combo=recode(vessel_combo, 
                             "30329"="30329 - WILSON!",
                             "38344"="38344 - FLYING SCOTT",
                             "ALVIN"="31068 - ALVIN",
                             "CYNDI"="36470 - CYNDI LYNN")) %>% 
  separate(col=vessel_combo, into=c("vessel_id", "vessel"), sep = " - ", remove=F) %>% 
  # Format vessel id
  mutate(vessel_id=gsub("-", "", vessel_id) %>% stringr::str_trim(.)) %>% 
  # Format vessel
  mutate(vessel=stringr::str_squish(vessel)) %>% 
  # Format fisher
  mutate(fisher=toupper(fisher) %>% stringr::str_squish(.)) %>% 
  # Move fisher ids in fisher name column into id column
  mutate(fisher_id=case_when(grepl("^[0-9]+$", fisher) ~ fisher,
                             fisher %in% c("L07191", "L10669", "L94862") ~ fisher,
                             T ~ fisher_id),
         fisher=case_when(grepl("^[0-9]+$", fisher) ~ NA,
                          fisher %in% c("L07191", "L10669", "L94862") ~ NA,
                          T ~ fisher)) %>% 
  # Format fisher id
  mutate(fisher_id=ifelse(!is.na(fisher_id), gsub("L", "", fisher_id) %>% paste0("L", .), fisher_id)) %>% 
  # Format block id
  mutate(block_id=as.numeric(block_id)) %>% 
  # Format port id
  # The vessel that put PRS is always out of 223 and sold to Pacific Rim Seafood
  mutate(port_id=recode(port_id, 
                        "PRS"="223"), # Pacific Rim Seafood
         port_id=as.numeric(port_id)) %>% 
  # Add port
  left_join(port_key %>% select(port_code, port_complex, port), by=c("port_id"="port_code")) %>% 
  # Add species
  mutate(species_id=ifelse(species_id==0, NA, species_id)) %>% 
  left_join(species_key %>% select(spp_code_num, comm_name), by=c("species_id"="spp_code_num")) %>% 
  # Format depth
  mutate(depth_min_ft=ifelse(depth_min_ft==0, NA, depth_min_ft) %>% abs(),
         depth_max_ft=ifelse(depth_min_ft==0, NA, depth_max_ft) %>% abs()) %>% 
  # Format hours
  mutate(hours=ifelse(hours==0, NA, hours)) %>% 
  # Format dealer
  mutate(dealer=dealer %>% stringr::str_squish() %>% stringr::str_to_upper() ) %>% 
  # Format location
  mutate(landmark=toupper(landmark)) %>% 
  # Format lat/long
  mutate(lat_dd_orig=ifelse(lat_dd_orig==0, NA, lat_dd_orig),
         long_dd_orig=ifelse(long_dd_orig==0, NA, long_dd_orig)) %>% 
  # Arrange
  select(year, month, date,
         logbook_id, 
         port_complex, port, port_id,
         vessel_combo, vessel_id, vessel, 
         fisher_id, fisher, 
         block_id, depth_min_ft, depth_max_ft,
         location_orig, lat_dd_orig, long_dd_orig, landmark, 
         dive_number, hours,
         species_id, comm_name,
         catch_lbs, receipt_id, dealer, remarks,
         everything())


# Inspect
str(data)
freeR::complete(data)

# Date
table(data$year)
range(data$date)
table(data$month)

# Vessel key
vessel_key <- data %>% 
  count(vessel_combo, vessel_id, vessel)
freeR::which_duplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel)

# Fisher id
fisher_key <- data %>% 
  count(fisher_id, fisher)

# Species key
spp_key <- data %>% 
  count(species_id, comm_name)

# Depth
boxplot(data$depth_max_ft)
boxplot(data$depth_min_ft)

# Landmark
freeR::uniq(data$landmark)


# Location key
################################################################################

x <- "32°40.17'"
calc_lat_dd <- function(x){
  lat_out <- purrr::map_dbl(x, function(x){
    lat_list <- strsplit(x, split="°")
    lat1 <- lat_list[[1]][1] %>% as.numeric()
    lat2 <- lat_list[[1]][2] %>% gsub("'", "", .) %>% as.numeric() 
    lat_out <- lat1 +   lat2/60
  })
  return(lat_out)
}
calc_lat_dd(x)

x <- "120°05.69'"
calc_long_dd <- function(x){
  long_out <- purrr::map_dbl(x, function(x){
    long_list <- strsplit(x, split="°")
    long1 <- long_list[[1]][1] %>% as.numeric()
    long2 <- long_list[[1]][2] %>% gsub("'", "", .) %>% as.numeric() 
    long_out <- (long1 +  long2/60) * -1
  })
  return(long_out)
}
calc_long_dd(x)

# Location
loc_key <- data %>% 
  # Unique
  count(location_orig) %>% 
  # Split lat/long
  separate(col=location_orig, sep="Lat.", into=c("lat_dms", "long_dms"), remove=F) %>% 
  # Format lat
  mutate(lat_dms=gsub(" ", "", lat_dms),
         lat_dms=case_when(lat_dms=="" ~ NA,
                           lat_dms=="__°__.__'" ~ NA,
                           lat_dms=="0°00.00'" ~ NA,
                           lat_dms=="Invalidcoordina" ~ NA,
                           T ~ lat_dms)) %>% 
  # Format longitude
  mutate(long_dms=gsub("Long.", "", long_dms) %>% gsub(" ", "", .),
         long_dms=case_when(long_dms=="" ~ NA,
                            long_dms=="__°__.__'" ~ NA,
                            long_dms=="___°__.__'" ~ NA,
                            long_dms=="Invalidcoordina" ~ NA,
                            long_dms=="420°" ~ NA,
                            long_dms=="394°33." ~ NA,
                            long_dms=="30" ~ NA,
                            T ~ long_dms)) %>% 
  # Calculate lat/long
  mutate(lat_dd=calc_lat_dd(lat_dms),
         long_dd=calc_long_dd(long_dms))

freeR::uniq(loc_key$lat_dms)
freeR::uniq(loc_key$long_dms)


# Fisher key
################################################################################

# Fix fisher key
fishery_key1 <- fisher_key %>% 
  # Fix some fisher combos
  mutate(fisher=recode(fisher,
                       "DAVID - ABERNATHY"="DAVID BRUCE - ABERNATHY",
                       "ABERNATHY - DAVID BRUCE"="DAVID BRUCE - ABERNATHY",
                       "MARCOS - A ABAN"="MARCOS - ABAN",
                       "BOWIE - BRYAN"="BRYAN - BOWIE",
                       "JIM - EBERNARDT"="JIM - EBERHARDT",
                       "D - MARTINO"="DANNY - DEMARTINO",
                       "C - KAVANAGH"="CHARLES - KAVANAUGH",
                       "J - BUTTLER"="JACK - BUTLER",
                       "J - MCCLELLAND"="JAMES P - MECLELLAND",
                       "N - KHENSOUVANN"="NOY - KHENSOUVAN",
                       "JAMES - MCCLELLAND"="JAMES P - MECLELLAND",
                       "KENNETH - BOEHCHER"="KENNETH - BOETTCHER", 
                       "RASMUSSEN - ALEX"="ALEX - RASMUSSEN",
                       "MICHAEL, P - KELLY"="MICHAEL P - KELLY",
                       "T - LEWSADDER"="TOM - LEWSADDER")) %>% 
  # Split
  filter(!is.na(fisher)) %>% 
  separate(fisher, into=c("first", "last"), sep=" - ", remove=F) %>% 
  # Format
  mutate(first=stringr::str_trim(first),
         last=stringr::str_trim(last)) %>% 
  # Number of character
  mutate(first_nchar=nchar(first)) %>% 
  # Get longest first name
  arrange(fisher_id, last, desc(first_nchar)) %>% 
  group_by(fisher_id, last) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Rename
  rename(fisher_orig=fisher) %>% 
  mutate(fisher=paste(last, first, sep=", "))
  
# Inspect
freeR::which_duplicated(fishery_key1$fisher_id)


# Finalize
################################################################################

# Finalize
data_out <- data %>% 
  # Add fisher
  select(-fisher) %>% 
  left_join(fishery_key1 %>% select(fisher_id, fisher), by="fisher_id") %>% 
  relocate(fisher, .after=fisher_id) %>% 
  # Add lat/long
  left_join(loc_key %>% select(location_orig, lat_dd, long_dd), by="location_orig") %>% 
  # Relocate
  relocate(lat_dd, long_dd, .after=location_orig) %>% 
  # Use reported when derived is is missing
  mutate(lat_dd=ifelse(is.na(lat_dd), lat_dd_orig, lat_dd),
         long_dd=ifelse(is.na(long_dd), long_dd_orig, long_dd)) %>% 
  # Erase invalid
  mutate(long_dd=ifelse(abs(long_dd)>180, NA, long_dd),
         lat_dd=ifelse(abs(lat_dd)>90, NA, lat_dd)) %>% 
  # Get GPS block id
  mutate(block_id_gps=wcfish::block_from_gps(long_dd=long_dd, lat_dd=lat_dd)) %>% 
  relocate(block_id_gps, .after=block_id) %>% 
  # Mark reliable points
  mutate(latlong_good_yn=block_id==block_id_gps) %>% 
  relocate(latlong_good_yn, .after=long_dd) %>% 
  # Remove a few
  select(-c(vessel_combo, lat_dd_orig, long_dd_orig))

# Inspect
freeR::complete(data_out)

# Plot coords
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
ggplot(data_out, aes(x=long_dd, y=lat_dd)) +
  geom_sf(data=usa, color="white", fill="grey85", inherit.aes = F) +
  geom_point(shape=1) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim=c(-116,-128),
           ylim=c(32,42)) +
  # coord_sf(xlim=c(range(data_out$long_dd, na.rm=T)),
  #          ylim=c(range(data_out$lat_dd, na.rm=T))) +
  # Theme
  theme_bw()

fisher_check <- data_out %>% 
  count(fisher_id, fisher)
freeR::which_duplicated(fisher_check$fisher_id)


# Export
################################################################################

# Export
saveRDS(data_out, file=file.path(outdir, "CDFW_1980_2023_dive_logbooks.Rds"))


