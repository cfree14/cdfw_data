

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/gillnet_logbooks/raw"
outdir <- "data/confidential/gillnet_logbooks/processed"
plotdir <- "data/confidential/gillnet_logbooks/figures"

# Read data
list.files(indir)

# Read data
data_orig <- readxl::read_excel(file.path(indir, "GillnetLogs_2017-2020.xlsx"), col_types = "text", na=c("N/A"))

# Read species key
spp_key <- readRDS("data/public/cdfw_keys/processed/CDFW_species_key.Rds") %>% 
  select(comm_name, sci_name, spp_code_num) %>% 
  arrange(comm_name, spp_code_num) %>% 
  group_by(comm_name) %>% 
  slice(1) %>% 
  ungroup()
freeR::which_duplicated(spp_key$comm_name)

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(logbook_id=sn,
         receipt_id=fish_game_receipt_no,
         boat_num=boatno,
         date_orig=fishing_date,
         target_spp=tarspc,
         target_spp_final=final_target_species,
         net_type_orig=drift_set,
         net_type_final=final_net_type_set_drift,
         block_id=fg_blocks,
         depth_fa=depths,
         net_length_ft=net_length,
         mesh_size_in=mesh_size,
         buoy_line_depth_fa=bouy_line_depth,
         soak_hr=hours_net_soaked,
         comm_name1=common_name,
         comm_name2=final_mlds_common_name,
         spp_code=mlds_species_code,
         catch_n=num_catch,
         catch_lb=weights) %>%
  # Convert date
  mutate(date1=ifelse(grepl("/", date_orig), date_orig, NA) %>% lubridate::mdy(.) %>% as.character(),
         date2=ifelse(grepl("/", date_orig), NA, date_orig) %>% openxlsx::convertToDate() %>% as.character(),
         date=ifelse(!is.na(date1), date1, date2) %>% lubridate::ymd()) %>%
  select(-c(date1, date2, date_orig)) %>%
  # Convert to numeric
  mutate(across(.cols=c(year, vessel_id, boat_num), .fns=as.numeric)) %>%
  # Convert numeric catch
  mutate(catch_n=catch_n %>% stringr::str_trim(.) %>% as.numeric(.)) %>%
  mutate(predator=ifelse(catch_lb=="Sea Lion", "Sea lion", predator),
         catch_lb=catch_lb %>% gsub("Sea Lion", "", .) %>% stringr::str_trim(.) %>% as.numeric(.)) %>%
  # Format net type
  mutate(net_type=recode(net_type_orig, "D"="Drift", "S"="Set"),
         net_type=ifelse(net_type %in% c("1", "2", "3", "67") | is.na(net_type), net_type_final, net_type)) %>%
  # Format status
  mutate(status=stringr::str_to_sentence(status)) %>%
  # Format predator
  mutate(predator=stringr::str_to_sentence(predator),
         predator=recode(predator,
                         "?"="Unknown",
                         "Birds"="Seabird",
                         "Blue sharks"="Blue shark",
                         "Harbor seals"="Harbor seal",
                         "Harbor seals and sea lions"="Harbor seal, sea lion",
                         "Mako"="Mako shark",
                         "Sea lions"="Sea lion",
                         "Sea lion - hagfish"="Sea lion, hagfish",
                         "Sea lions and harbor seals"="Sea lion, harbor seal",
                         "Sea lions and slime eels"="Sea lion, slime eel",
                         "Seal?"="Seal",
                         "Seals"="Seal",
                         "Seals, sea lion"="Seal, sea lion",
                         "Slime eels"="Slime eel",
                         "Unknown, maybe seal"="Unknown",
                         "National marine fisheries"="NMFS",
                         "Sea lions"="Sea lion")) %>%
  # Format common name
  mutate(spp_code=recode(spp_code,
                         "1520"="152", "154/158"="154") %>% as.numeric(.)) %>%
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("spp_code"="spp_code_num")) %>%
  mutate(comm_name=case_when(spp_code==90 ~ "Swordfish",
                             spp_code==241 ~ "Curlfin turbot",
                             spp_code==243 ~ "C-O turbot",
                             spp_code==677 ~ "Shortraker rockfish",
                             spp_code==980 ~ "Sea lion",
                             T ~ comm_name)) %>%
  # Fill in missing common names
  rowwise() %>%
  mutate(comm_name=ifelse(is.na(comm_name), paste(comm_name1, comm_name2, collapse=", "), comm_name)) %>%
  ungroup() %>%
  mutate(comm_name=recode(comm_name,
                          "NA NA"="",
                          "Blue Mackerel NA"="Blue mackerel",
                          "SB NA"="",
                          "X Unknown"="",
                          "Crab Crab,RockUnspecified"="Unspecified rock crab",
                          "Harbor Seal NA"="Harbor seal",
                          "S NA"="",
                          "Grass Back NA"="",
                          "Verde NA"="",
                          "NA Crab,RockUnspecified"="Unspecified rock crab",
                          "NA SeaUrchin,unspecified"="Unspecified sea urchin"),
         comm_name=ifelse(comm_name=="", NA, comm_name)) %>%
  # Format target species
  mutate(target_spp=gsub("H, ", "Halibut, ", target_spp),
         target_spp=gsub("B, ", "Barracuda, ", target_spp),
         target_spp=gsub("W, ", "White seabass, ", target_spp),
         target_spp=gsub("C, ", "White croaker, ", target_spp),
         target_spp=gsub("S, ", "Shark/swordfish, ", target_spp),
         target_spp=gsub("X, ", "Soupfin shark, ", target_spp),
         target_spp=gsub(", X", ", Soupfin shark", target_spp),
         target_spp=gsub("YELTL", "Yellowtail", target_spp)) %>%
  mutate(target_spp=recode(target_spp,
                           "B"="Barracuda",
                           "H"="Halibut",
                           "W"="White seabass",
                           "S"="Shark/swordfish",
                           "X"="Soupfin shark",
                           "Croaker_Kingfish"="White croaker",
                           "Barracuda, W"="Barracuda, White croaker",
                           "Halibut, Sole, S"="Halibut, Sole, Shark/swordfish",
                           "Halibut, W"="Halibut, White seabass",
                           "Soupfin shark, W"="Soupfin shark, White seabass",
                           "White seabass, H"="White seabass, Halibut",
                           "Yellowtail, H"="Yellowtail, Halibut")) %>%
  # Add missing vessel id
  # I looked at the landings receipts for VICTORIA by KUNZEL and its vessel 30330
  mutate(vessel_id=ifelse(vessel_name=="Victoria" & is.na(vessel_id), 30330, vessel_id)) %>% 
  # Remove empty columns
  select(-c(target_spp_final, boat_num)) %>%
  # Remove columns that I'm not interested in
  select(-c(crew_member_1:x40)) %>%
  # Arrange
  select(logbook_id, receipt_id, vessel_id, vessel_name, skipper_name, permit,
         year, date, block_id, depth_fa,
         target_spp, net_type, net_length_ft, mesh_size_in, buoy_line_depth_fa,
         soak_hr,
         spp_code, comm_name, comm_name1, comm_name2,
         status, predator, catch_n, catch_lb, everything()) %>%
  # Remove a few after checking
  select(-c(net_type_orig, net_type_final, comm_name1, comm_name2))

# Inspect
str(data)
freeR::complete(data)

# Numeric
range(data$year)
range(data$date)

# Character
sort(unique((data$target_spp))) # Y/T/SW are unknown (Yellowtail/Thresher/Swordfish?); could be standardized more
table(data$net_type)
table(data$status)
table(data$predator)


# Inspect net type
if(F){
  net_key <- data %>%
    select(net_type, net_type_orig, net_type_final) %>%
    unique()

  spp_key_check <- data %>%
    select(spp_code, comm_name, comm_name1, comm_name2) %>%
    unique()
}

# Export
saveRDS(data, file=file.path(outdir, "CDFW_2017_2020_gillnet_logbooks.Rds"))
write.csv(data, file=file.path(outdir, "CDFW_2017_2020_gillnet_logbooks.csv"), row.names=F)






