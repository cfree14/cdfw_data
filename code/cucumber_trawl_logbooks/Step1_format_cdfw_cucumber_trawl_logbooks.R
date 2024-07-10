

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/cucumber_trawl_logbooks/raw"
outdir <- "data/confidential/cucumber_trawl_logbooks/processed"
plotdir <- "figures/cucumber_trawl_logbooks"
keydir <- "data/public/cdfw_keys/processed"

# Read data
list.files(indir)
data_orig <- read.csv(file.path(indir, "LogSummary_CucumberTrawl_Free_2024.04.05.csv"), na.strings = c("", "NA"))

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
species_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
blocks_sf <- wcfish::blocks
blocks <- blocks_sf %>% 
  sf::st_drop_geometry()

# TO-DO LIST
# Convert LORAN coordinates
# Add block id from landing receipts
# Identify reliable coordinates
# Format times and derive tow lengths
# There are negative durations - are the times flipped?


# Helper function
################################################################################

# Replace zero with NA
replace_zero_with_na <- function(x){
  y <- ifelse(x==0, NA, x)
  return(y)
}

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  rename(logbook_id=log_serial_num,
         fisher_id=fisher_num,
         vessel_id=vessel_num,
         vessel=vessel_name,
         # Date
         date_tow=detail_date,
         date_depart=departure_date,
         date_landing=landing_date,
         # Location
         port=port_desc,
         block_id=block_number,
         # Tow details
         tow_number=drag_number,
         net_type=net_type_desc,
         headrope_ft=headrope_length,
         # Species/catch
         species_orig=market_cat_desc,
         species_id=species_code,
         catch_lbs=total_pounds,
         comments=detail_comments,
         # Time
         duration_min=total_time,
         time_set=set_time,
         time_up=up_time,
         # Depth
         depth_fa_set=set_depth,
         depth_fa_up=up_depth) %>% 
  # Format port
  mutate(port=stringr::str_to_title(port)) %>% 
  # Add species
  left_join(species_key %>% select(spp_code_num, comm_name), by=c("species_id"="spp_code_num")) %>% 
  rename(species=comm_name) %>% 
  # Format vessel name
  mutate(vessel=toupper(vessel),
         vessel=case_when(vessel=="KAY D" ~ "KAY-D",
                          vessel=="ST. PETER" ~ "ST PETER",
                          vessel_id=="16034" ~ "LINDA C",
                          vessel_id=="28559" ~ "LOIS-D",
                          vessel_id=="29524" ~ "SAN ANTONIO",
                          vessel_id=="3587" ~ "GENOA",
                          T ~ vessel)) %>% 
  # Format vessel id
  mutate(vessel_id=ifelse(vessel_id %in% c(".", "x", "z", "00000"), NA, vessel_id),
         vessel_id=case_when(vessel=="NEW HAZARD" ~ "02454",
                             vessel=="PIEFACE" ~ "01139",
                             vessel=="SIX BROS" ~ "08956",
                             T ~ vessel_id)) %>% 
  # Format fisher id
  mutate(fisher_id=gsub("L|l", "", fisher_id) %>% paste0("L", .),
         fisher_id=ifelse(fisher_id=="LNA", NA, fisher_id)) %>% 
  # Format dates
  mutate(date_depart=lubridate::mdy(date_depart),
         date_landing=lubridate::mdy(date_landing),
         date_tow=recode(date_tow, "5/15/5200"="5/15/2000"),
         date_tow=lubridate::mdy(date_tow)) %>% 
  # Format times
  # mutate(time_set=chron::chron(times=time_set)) %>% 
  # Format tow numbers
  # I figured this out by looking at the logbooks for the high numbers
  mutate(tow_number=case_when(tow_number==1022 ~ 4,
                              tow_number==1130 ~ 3,
                              tow_number==927 ~ 2,
                              tow_number==928 ~ 3,
                              T ~ tow_number)) %>% 
  # Format block id
  mutate(block_id=ifelse(block_id==0, NA, block_id)) %>% 
  # Format duration
  mutate(duration_min=ifelse(duration_min<=0, NA, duration_min)) %>% 
  # Format depths
  mutate(depth_fa_set=ifelse(depth_fa_set==0, NA, depth_fa_set),
         depth_fa_up=ifelse(depth_fa_up==0, NA, depth_fa_up)) %>% 
  # Format headrope length
  mutate(headrope_ft=ifelse(headrope_ft==0, NA, headrope_ft)) %>% 
  # Add year
  mutate(year=ifelse(!is.na(date_tow), lubridate::year(date_tow),
                     ifelse(!is.na(date_landing), lubridate::year(date_landing), lubridate::year(date_depart))),
         year=ifelse(is.na(year), old_year, year)) %>% 
  # Build latitudes
  mutate(lat_dd_set=set_lat_deg+set_lat_dec/60,
         lat_dd_up=up_lat_deg+up_lat_dec/60) %>% 
  mutate(lat_dd_set=ifelse(lat_dd_set==0, NA, lat_dd_set),
         lat_dd_up=ifelse(lat_dd_up==0, NA, lat_dd_up)) %>% 
  # Build longitudes
  mutate(long_dd_set=(set_long_deg+set_long_dec/60) * -1,
         long_dd_up=(up_long_deg+up_long_dec/60) * -1) %>% 
  mutate(long_dd_set=ifelse(long_dd_set==0, NA, long_dd_set),
         long_dd_up=ifelse(long_dd_up==0, NA, long_dd_up)) %>% 
  # Format LORAN coordinates
  mutate_at(vars(set_loran_cx:up_loran_amax), replace_zero_with_na) %>% 
  # Arrange
  select(logbook_id, fisher_id, vessel_id, vessel,
         port_code, port,
         year, old_year,
         date_depart, date_landing, date_tow, tow_number,
         net_type, headrope_ft,
         block_id, 
         lat_dd_set, long_dd_set,
         lat_dd_up, long_dd_up,
         time_set, time_up, duration_min, 
         depth_fa_set, depth_fa_up,
         species_id, species, species_orig,
         catch_lbs, comments,
         everything()) %>% 
  arrange(year, logbook_id)


# Inspect data
str(data)
freeR::complete(data)

# Vessel key
vessel_key <- data %>% 
  count(vessel_id, vessel)
freeR::which_duplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel)

# Fisher id
sort(unique(data$fisher_id))

# Port key
port_key <- data %>% 
  count(port_code, port)
freeR::which_duplicated(port_key$port_code)
freeR::which_duplicated(port_key$port)

# Species key
spp_key <- data %>% 
  count(species_id, species_orig, species)
freeR::which_duplicated(spp_key$species_id)
freeR::which_duplicated(spp_key$species)

# Dates
range(data$date_depart, na.rm = T)
range(data$date_landing, na.rm = T)
range(data$date_tow, na.rm = T)

# Inspect character
table(data$net_type)

# Inspect numeric
table(data$tow_number)

# Map
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
ggplot(data, aes(x=long_dd_set, y=lat_dd_set)) +
  geom_sf(data=usa, color="white", fill="grey85", inherit.aes = F) +
  geom_point() +
  # Labels
  labs(x="", y="") +
  # Crop
  # coord_sf(xlim=c(range(data$long_dd_set, na.rm=T)),
  #          ylim=c(range(data$lat_dd_set, na.rm=T))) +
  coord_sf(xlim=c(-128, -116),
           ylim=c(30, 45)) +
  theme_bw()



# Export data
################################################################################

# Prep
data_out <- data %>% 
  # Remove useless
  select(-c(species_orig, 
            set_lat_deg, set_lat_dec, set_long_deg, set_long_dec, 
            up_lat_deg, up_lat_dec, up_long_deg, up_long_dec))

# Export
saveRDS(data_out, file=file.path(outdir, "1982_2020_cucumber_trawl_logbooks.Rds"))




  