

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/trawl_logbooks/raw"
outdir <- "data/confidential/trawl_logbooks/processed"
plotdir <- "figures/trawl_logbooks"
keydir <- "data/public/cdfw_keys/processed"

# Read data
list.files(indir)
data_orig <- read.csv(file.path(indir, "MLS_TrawlExtract_03182022.csv"), na.strings = "")

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
species_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
blocks_sf <- wcfish::blocks
blocks <- blocks_sf %>% 
  sf::st_drop_geometry()


# Helper function
################################################################################

# Function to collapse repeated dates
collapse_dates <- function(x){
  # Break and unique
  z <- purrr::map_chr(x, function(x){
    y <- str_split(x, pattern=", ", simplify = T) %>% as.character() %>% unique()
  })
  return(z)
}
collapse_dates(x=c("2010-02-11, 2010-02-11", "2010-02-18, 2010-02-18, 2010-02-18"))


# Format data
################################################################################

# Format data
# Rename and fix date
data1 <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(logbook_id=serial_number,
         vessel=vessel_name,
         return_date=return_date_string,
         set_lat_dd=set_latitude,
         set_long_dd=set_longitude,
         up_lat_dd=up_latitude,
         up_long_dd=up_longitude,
         block_id=block,
         depth_avg_fathoms=average_depth,
         target_spp_code_pacfin=target_strategy,
         catch_lb=pounds,
         catch_lb_conv=converted_pounds,
         spp_code_cdfw=species_code,
         spp_code_pacfin=pac_fin_species_code,
         value_usd=revenue,
         efp_trip_yn=is_efp_trip,
         observed_trip_yn=is_observed_trip) %>%
  # Convert return/departure dates
  mutate(departure_date=lubridate::mdy(departure_date),
         return_date=lubridate::mdy(return_date)) %>%
  # Build tow date
  mutate(tow_date=paste(tow_year, tow_month, tow_day, sep="-") %>% lubridate::ymd()) %>%
  # Fix and convert landing dates
  mutate(landing_date=collapse_dates(x=landing_date),
         landing_date=lubridate::ymd(landing_date))


# Format even more
################################################################################

# Format data some more
data2 <- data1 %>%
  # Add tow id
  mutate(tow_id=paste(logbook_id, tow_date, tow_number, set_time, sep="-")) %>%
  # Fix longitudes
  mutate(set_long_dd=set_long_dd*-1,
         up_long_dd=up_long_dd*-1) %>%
  # Format net type
  mutate(net_type=recode(net_type,
                         "B"="Bottom",
                         "D"="Danish or Scottish seine",
                         "F"="Selective flatfish",
                         "L"="Large footrope",
                         "M"="Midwater",
                         "S"="Small footrope")) %>%
  # Format region
  mutate(region=recode_factor(region,
                              "N"="North (above 40°10'N)",
                              "NC"="North Central (36° to 40°10'N)",
                              "C"="Central (34°27'N to 36°N)",
                              "S"="Southern (below 34°27'N)")) %>%
  # Add target species
  left_join(pacfin_spp_key %>% select(spp_code, comm_name), by=c("target_spp_code_pacfin"="spp_code")) %>%
  rename(target_spp=comm_name) %>%
  # Add landed species common name (based on CDFW code)
  rename(spp_code_pacfin_orig=spp_code_pacfin) %>%
  left_join(cdfw_spp_key  %>% select(spp_code_num, pacfin_code, comm_name), by=c("spp_code_cdfw"="spp_code_num")) %>%
  rename(species_cdfw=comm_name,
         spp_code_pacfin=pacfin_code) %>%
  # Add landed species common name (based on PACFIN code)
  left_join(pacfin_spp_key  %>% select(spp_code, comm_name), by=c("spp_code_pacfin_orig"="spp_code")) %>%
  rename(species_pacfin=comm_name) %>%
  # Finalize common name
  mutate(species=ifelse(!is.na(species_cdfw), species_cdfw, species_pacfin),
         species=recode(species, "Bocaccio rockfish"="Bocaccio"),
         spp_code_pacfin=ifelse(!is.na(spp_code_pacfin), spp_code_pacfin, spp_code_pacfin_orig)) %>%
  # Fill missing species
  mutate(species=ifelse(spp_code_cdfw==677 & !is.na(spp_code_cdfw), "Shortraker rockfish", species)) %>%
  # Fill in missing CDFW species codes
  group_by(species) %>%
  mutate(spp_code_cdfw=paste(sort(unique(na.omit(spp_code_cdfw))), collapse=", ")) %>%
  ungroup() %>%
  mutate(spp_code_cdfw=as.numeric(spp_code_cdfw)) %>%
  # Standardize PACFIN species codes
  group_by(species) %>%
  mutate(spp_code_pacfin=paste(sort(unique(na.omit(spp_code_pacfin))), collapse=", ")) %>%
  ungroup() %>%
  # Correct a few PACFIN species codes
  mutate(spp_code_pacfin=recode(spp_code_pacfin,
                                "BGL1, BLGL"="BLGL",
                                "BCAC, BCC1"="BCAC",
                                "CBZ1, CBZN"="CBZN",
                                "DBR1, DBRK"="DBRK",
                                "LSP1, LSPN"="LSPN",
                                "SSP1, SSPN"="SSPN",
                                "OFLT, SLNS"="SLNS")) %>%
  # Remove some duplicated data
  filter(!(logbook_id=="31701" & vessel=="TAUNY ANN")) %>%
  # Arrange
  select(logbook_id, vessel_id, federal_doc_number, vessel, crew_size,
         departure_port_code, departure_date, departure_time,
         return_port_code, return_date, return_time,
         landing_date, landing_receipt, efp_trip_yn, observed_trip_yn,
         tow_id, tow_year, tow_month, tow_day, tow_date, tow_number,
         tow_hours, set_time, set_lat_dd, set_long_dd, up_time, up_lat_dd, up_long_dd,
         region, block_id, depth_avg_fathoms, net_type,
         target_spp_code_pacfin, target_spp,
         spp_code_pacfin_orig, spp_code_pacfin, spp_code_cdfw, species_cdfw, species_pacfin, species,
         catch_lb, catch_lb_conv, value_usd, comments,
         everything()) %>%
  # Remove
  select(-c(species_cdfw, species_pacfin, spp_code_pacfin_orig))


# Inspect data
################################################################################

# Inspect
str(data2)
#freeR::complete(data2)

# Inspect
range(data2$tow_date)
table(data2$net_type)
table(data2$target_spp)
table(data2$target_spp_code_pacfin)
table(data2$observed_trip_yn)
table(data2$efp_trip_yn)
table(data2$region)

# Target species key
# Unmatched target species codes: BRSH, BRSL, DTS, DWD, NSM, THHD,
target_key <- data2 %>%
  select(target_spp_code_pacfin, target_spp) %>%
  unique() %>%
  arrange(target_spp_code_pacfin)
freeR::which_duplicated(target_key$target_spp)
freeR::which_duplicated(target_key$target_spp_code_pacfin)

# Landed species key
spp_key_check <- data2 %>%
  select(spp_code_pacfin, spp_code_cdfw, species) %>%
  unique() %>%
  arrange(spp_code_cdfw)
freeR::which_duplicated(spp_key_check$species)
freeR::which_duplicated(spp_key_check$spp_code_cdfw)
freeR::which_duplicated(spp_key_check$spp_code_pacfin) # PACFIN codes aren't perfect


# Inspect tow id
##########################################

# Build tow key
# Crew size, return time, departure time aren't perfectly filled in
tow_key <- data2 %>%
  select(tow_id, logbook_id:target_spp) %>%
  select(-c(crew_size, departure_time, return_time)) %>%
  unique()

# Are the tow ids unique?
freeR::which_duplicated(tow_key$tow_id) # looks good



# Inspect lat/long
##########################################

# World
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Plot points
# g <- ggplot() +
#   # Country
#   geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes=F) +
#   geom_sf(data=mexico, fill="grey80", color="white", lwd=0.2, inherit.aes=F) +
#   # Tows
#   geom_point(data=data2, mapping=aes(x=set_long_dd, y=set_lat_dd), size=0.1, color="red") +
#   geom_point(data=data2, mapping=aes(x=up_long_dd, y=up_lat_dd), size=0.1, color="blue") +
#   # Labels
#   labs(x="", y="") +
#   # Crop
#   coord_sf(y=c(30, 45), x=c(-110, -130)) +
#   # Theme
#   theme_bw()

# Calculate block id
################################################################################

# Set coordinates
#################################

# Set coordinates
set_xy <- data2 %>%
  mutate(id=1:n()) %>%
  select(id, set_long_dd, set_lat_dd, block_id) %>%
  rename(block_id_orig=block_id) %>%
  filter(!is.na(set_long_dd) & !is.na(set_lat_dd))

# Set sf
set_xy_sf <- sf::st_as_sf(set_xy, coords = c("set_long_dd", "set_lat_dd"), crs = sf::st_crs(blocks))

# Intersect trawl set points with blocks
set_xy_block_indices <- sf::st_intersects(x=set_xy_sf, y=blocks)
set_xy_block_indices_vec <- set_xy_block_indices %>% as.numeric()
set_xy_block_ids <- blocks$block_id[set_xy_block_indices_vec]

# Add to original
set_xy_new <- set_xy %>%
  mutate(block_id=set_xy_block_ids)

# Up coordinates
#################################

# up coordinates
up_xy <- data2 %>%
  mutate(id=1:n()) %>%
  select(id, up_long_dd, up_lat_dd, block_id) %>%
  rename(block_id_orig=block_id) %>%
  filter(!is.na(up_long_dd) & !is.na(up_lat_dd))

# up sf
up_xy_sf <- sf::st_as_sf(up_xy, coords = c("up_long_dd", "up_lat_dd"), crs = sf::st_crs(blocks))

# Intersect trawl up points with blocks
up_xy_block_indices <- sf::st_intersects(x=up_xy_sf, y=blocks)
up_xy_block_indices_vec <- up_xy_block_indices %>% as.numeric()
up_xy_block_ids <- blocks$block_id[up_xy_block_indices_vec]

# Add to original
up_xy_new <- up_xy %>%
  mutate(block_id=up_xy_block_ids)


# Add block id to dataframe
################################################################################

# Build data frame
data3 <- data2 %>%
  # Add id
  mutate(id=1:n()) %>%
  # Rename block
  rename(block_id_orig=block_id) %>%
  # Add set block
  left_join(set_xy_new %>% select(id, block_id), by="id") %>%
  rename(set_block_id=block_id) %>%
  # Add up block
  left_join(set_xy_new %>% select(id, block_id), by="id") %>%
  rename(up_block_id=block_id) %>%
  # Arrange
  select(-id) %>%
  select(logbook_id, vessel_id, federal_doc_number, vessel, crew_size,
         departure_port_code, departure_date, departure_time,
         return_port_code, return_date, return_time,
         landing_date, landing_receipt, efp_trip_yn, observed_trip_yn,
         tow_id, tow_year, tow_month, tow_day, tow_date, tow_number, tow_hours,
         set_time, set_lat_dd, set_long_dd, set_block_id,
         up_time, up_lat_dd, up_long_dd, up_block_id,
         region, block_id_orig, depth_avg_fathoms, net_type,
         target_spp_code_pacfin, target_spp,
         spp_code_pacfin, spp_code_cdfw, species,
         catch_lb, catch_lb_conv, value_usd, comments,
         everything())

# Export data
################################################################################

# Export
saveRDS(data3, file=file.path(outdir, "CDFW_2000_2020_trawl_logbook_data.Rds"))


