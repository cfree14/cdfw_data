

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/cpfv_logbooks/raw/CPFV logs"
outdir <- "data/confidential/cpfv_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
species_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
block_key <- wcfish::blocks %>% 
  sf::st_drop_geometry()


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Loop through and merge
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- read.csv(file.path(indir, x), na.strings = c("", "NA")) %>% 
    mutate(filename=x) %>% 
    select(filename, everything()) %>% 
    mutate_all(as.character)
  
})


# Target species, method, baid
################################################################################

# Which target species are most population
target_n <- rev(sort(nrow(data_orig)-freeR::complete(data_orig %>% select(TargetSpeciesLingcod:TargetSpeciesMiscBay))))
method_n <- rev(sort(nrow(data_orig)-freeR::complete(data_orig %>% select(FishingMethodTrolling:FishingMethodOther))))
bait_n <- rev(sort(nrow(data_orig)-freeR::complete(data_orig %>% select(BaitUsedAnchoviesLive:BaitUsedOtherDead))))


# Target species
table(data_orig$TargetSpeciesLingcod)
table(data_orig$TargetSpeciesOther)
table(data_orig$TargetSpeciesRockfishes)
table(data_orig$TargetSpeciesSalmon)
table(data_orig$TargetSpeciesSharks)
table(data_orig$TargetSpeciesStripedBass)
table(data_orig$TargetSpeciesSturgeon)
table(data_orig$TargetSpeciesTuna)
table(data_orig$TargetSpeciesPotluck)
table(data_orig$TargetSpeciesMiscCoastal)
table(data_orig$TargetSpeciesMiscBay)
table(data_orig$TargetSpeciesMiscOffshore)

# Fishing method

# Bait used


# Helper functions
################################################################################

# Function to convert HH:MM to HH.HH
x <- "16:34"
conv_hm_to_hr <- function(x){
  y <- strsplit(x, ":")[[1]] %>% as.numeric()
  z <- y[1] + y[2]/60
}


# Target species
target_spp <- data_orig %>% 
  select(names(target_n)) %>% 
  slice(1:20) %>% 
  mutate(target_spp=paste(TargetSpeciesLingcod, TargetSpeciesMiscBay, sep="-")) %>% 
  ungroup()
  
table(target_spp$target_spp)




# Clean data
################################################################################

# Mark months w/out fishing correctly
# Merge target species, method, baits used

# Clean data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(logbook_id=serial_number,
         date=log_date,
         month=log_month,
         day=log_day,
         year=log_year,
         block_id=block,
         hm_fished=hours_minutes_fished,
         comm_name_orig=species, 
         temp_f=temperature,
         depth_ft=depth,
         n_fishers=number_of_fishers,
         hrs_fished=hours_fished,
         n_kept=number_kept,
         n_released=number_released,
         n_lost_to_sea_lions=number_lost_to_sea_lions,
         n_caught_by_crew=number_of_fish_caught_by_crew,
         n_crew_fished=number_of_crew_fished,
         operator=operator_name) %>% 
  # Format date
  mutate(date=lubridate::mdy(date),
         date_received=lubridate::mdy(date_received),
         date_submitted=lubridate::mdy(date_submitted)) %>% 
  mutate(across(.cols=month:year, .fns=as.numeric)) %>% 
  # Make unique logbook id
  mutate(logbook_id_use=paste(vessel_id, date, logbook_id, sep="-")) %>% 
  # Format port code
  mutate(port_code=as.numeric(port_code)) %>% 
  # Add port details
  left_join(port_key %>% select(port_code, port, port_complex), by="port_code") %>% 
  # mutate(port=ifelse(is.na(port), "Invalid", port)) %>% 
  # mutate(port_complex=ifelse(is.na(port_complex), "Invalid", port_complex)) %>% 
  # Format trip type
  mutate(trip_type=stringr::str_to_sentence(trip_type)) %>% 
  # Add block info
  mutate(block_id=as.numeric(block_id)) %>% 
  left_join(block_key %>% select(block_id, block_state, block_type), by="block_id") %>% 
  # Format depth/temperature
  mutate(depth_ft=as.numeric(depth_ft) %>% abs(.)) %>% 
  mutate(temp_f=as.numeric(temp_f)) %>% 
  # Format effort
  mutate(n_fishers=as.numeric(n_fishers),
         hrs_fished=as.numeric(hrs_fished)) %>% 
  # Format HH:MM effort
  mutate(hm_fished=stringr::str_squish(hm_fished), 
         hm_fished=recode(hm_fished, 
                          "800"="",
                          "100"="",
                          "145"="", 
                          "600"="",  
                          "300"="", 
                          "830"="", 
                          "730"="", 
                          "330"="", 
                          "251"="", 
                          "615"=""), 
           hm_fished=ifelse(hm_fished=="", NA, hm_fished)) %>% 
  # Format species
  mutate(comm_name_orig=stringr::str_squish(comm_name_orig)) %>% 
  # Format species code
  mutate(species_code=recode(species_code, 
                             "8/0"="",
                             "Oarfish"="") %>% as.numeric(.)) %>% 
  # Add species info
  left_join(species_key %>% select(spp_code_num, comm_name, sci_name), by=c("species_code"="spp_code_num")) %>%
  # Fix oarfish
  mutate(comm_name=ifelse(comm_name_orig=="Oarfish", "Oarfish", comm_name),
         sci_name=ifelse(comm_name=="Oarfish", "Regalecus glesne", sci_name)) %>% 
  # Format catch
  mutate(across(.cols=n_kept:n_crew_fished, .fns=as.numeric)) %>% 
  # Replace catch NAs with zeros
  mutate(across(.cols=n_kept:n_caught_by_crew, .fns=function(x){ifelse(is.na(x), 0, x)})) %>% 
  # Format operator
  mutate(operator=operator %>% stringr::str_to_title() %>% stringr::str_squish() %>% stringr::str_trim()) %>% 
  # Format target species
  mutate(target_species_rockfishes=ifelse(!is.na(target_species_rockfishes), "Rockfish", "NA"),
         target_species_misc_coastal=ifelse(!is.na(target_species_misc_coastal), "Misc. coastal", "NA"),
         target_species_other=ifelse(!is.na(target_species_other), "Other", "NA"),
         target_species_lingcod=ifelse(!is.na(target_species_lingcod), "Lingcod", "NA"),
         target_species_tuna=ifelse(!is.na(target_species_tuna), "Tuna", "NA"),
         target_species_misc_offshore=ifelse(!is.na(target_species_misc_offshore), "Misc. offshore", "NA"),
         target_species_salmon=ifelse(!is.na(target_species_salmon), "Salmon", "NA"),
         target_species_potluck=ifelse(!is.na(target_species_potluck), "Potluck", "NA"),
         target_species_striped_bass=ifelse(!is.na(target_species_striped_bass), "Striped bass", "NA"),
         target_species_sharks=ifelse(!is.na(target_species_sharks), "Sharks", "NA"),
         target_species_sturgeon=ifelse(!is.na(target_species_sturgeon), "Sturgeon", "NA"),
         target_species_misc_bay=ifelse(!is.na(target_species_misc_bay), "Misc. bay", "NA")) %>% 
  mutate(target_species=paste(target_species_rockfishes, target_species_misc_coastal, 
                              target_species_other, target_species_lingcod, 
                              target_species_tuna, target_species_misc_offshore, 
                              target_species_salmon, target_species_potluck, 
                              target_species_striped_bass, target_species_sharks, 
                              target_species_sturgeon, target_species_misc_bay, sep=", ")) %>% 
  mutate(target_species=gsub("NA, |, NA", "", target_species)) %>% 
  select(-c(target_species_lingcod:target_species_misc_bay)) %>% 
  # Format fishing method
  mutate(fishing_method_anchored=ifelse(!is.na(fishing_method_anchored), "Anchored", "NA"),
         fishing_method_drifting=ifelse(!is.na(fishing_method_drifting), "Drifting", "NA"),
         fishing_method_trolling=ifelse(!is.na(fishing_method_trolling), "Trolling", "NA"),
         fishing_method_light_tackle=ifelse(!is.na(fishing_method_light_tackle), "Light tackle", "NA"),
         fishing_method_diving=ifelse(!is.na(fishing_method_diving), "Diving", "NA"),
         fishing_method_other=ifelse(!is.na(fishing_method_other), "Other", "NA"),
         fishing_method_mooching=ifelse(!is.na(fishing_method_mooching), "Mooching", "NA")) %>% 
  mutate(fishing_method=paste(fishing_method_anchored, fishing_method_drifting, 
                              fishing_method_trolling, fishing_method_light_tackle, 
                              fishing_method_diving, fishing_method_other, 
                              fishing_method_mooching, sep=", ")) %>% 
  mutate(fishing_method=gsub("NA, |, NA", "", fishing_method)) %>% 
  select(-c(fishing_method_trolling:fishing_method_other)) %>% 
  # Format bait used
  mutate(bait_used_squid_dead=ifelse(!is.na(bait_used_squid_dead), "Squid (dead)", "NA"),
         bait_used_sardines_live=ifelse(!is.na(bait_used_sardines_live), "Sardines (live)", "NA"),
         bait_used_anchovies_live=ifelse(!is.na(bait_used_anchovies_live), "Anchovies (live)", "NA"),
         bait_used_squid_live=ifelse(!is.na(bait_used_squid_live), "Squid (live)", "NA"),
         bait_used_other_dead=ifelse(!is.na(bait_used_other_dead), "Other (dead)", "NA"),
         bait_used_anchovies_dead=ifelse(!is.na(bait_used_anchovies_dead), "Anchovies (dead)", "NA"),
         bait_used_sardines_dead=ifelse(!is.na(bait_used_sardines_dead), "Sardines (dead)", "NA"), 
         bait_used_other_live=ifelse(!is.na(bait_used_other_live), "Other (live)", "NA")) %>% 
  mutate(bait_used=paste(bait_used_squid_dead, bait_used_sardines_live, 
                         bait_used_anchovies_live, bait_used_squid_live, 
                         bait_used_other_dead, bait_used_anchovies_dead, 
                         bait_used_sardines_dead, bait_used_other_live, sep=", ")) %>% 
  mutate(bait_used=gsub("NA, |, NA", "", bait_used)) %>% 
  select(-c(bait_used_anchovies_live:bait_used_other_dead)) %>% 
  # Arrange
  select(filename, logbook_id_use, logbook_id, 
         year, month, day, 
         date, date_submitted, date_received,
         vessel_id, vessel_name, operator,
         port_complex, port_code, port,
         no_activity_month,
         trip_type, non_paying, descending_device, bird_interaction,
         target_species, fishing_method, bait_used, 
         block_id, block_type, block_state,
         depth_ft, temp_f, 
         departure_time, return_time, hm_fished, hrs_fished, n_fishers, n_crew_fished,
         species_code, comm_name_orig, comm_name, sci_name,
         n_kept, n_released, n_lost_to_sea_lions, n_caught_by_crew,
         everything())



# Build logbook key
################################################################################

# Build logbook id
log_key <- data %>% 
  group_by(logbook_id_use, logbook_id, 
           vessel_id, vessel_name, 
           year, date,
           port_complex, port, port_code, 
           no_activity_month, trip_type, non_paying, 
           departure_time, return_time,
           block_id, block_type, block_state,
           depth_ft, temp_f,
           hm_fished, hrs_fished,
           n_fishers, n_crew_fished) %>% 
  summarize(n_kept=sum(n_kept)) %>% 
  ungroup()

# Are the logbook ids unique? Mine should be, there's shouldn't be
freeR::which_duplicated(log_key$logbook_id_use)
freeR::which_duplicated(log_key$logbook_id)

# Does number of fishers always exceed number of crew?
sum(log_key$n_crew_fished < log_key$n_fishers, na.rm=T)



# Inspect data
################################################################################

# Inspect
head(data)
str(data)
# na_check <- freeR::complete(data)
# (100 - na_check / nrow(data) * 100) %>% round(., 2)

# Date
range(data$date)
range(data$year)
range(data$month)
range(data$day)
range(data$date_received, na.rm=T)
range(data$date_submitted, na.rm=T) # 1900-01-01 = NA?

# Vessels
vessel_key <- data %>% 
  select(vessel_id, vessel_name) %>% 
  unique() %>% 
  group_by(vessel_id) %>% 
  summarize(n_names=n_distinct(vessel_name),
            vessel_names=paste(unique(vessel_name), collapse=", ")) %>% 
  ungroup()

# Operator
sort(unique(data$operator))

# Ports
port_key_check <- data %>% 
  select(port_complex, port_code, port) %>% 
  unique() %>% 
  arrange(port_code)
port_key_check$port_code[is.na(port_key_check$port)]

# Block
block_key_check <- data %>% 
  select(block_id, block_type, block_state) %>% 
  unique()
sort(block_key_check$block_id[is.na(block_key_check$block_type)])

# Trip type
sort(unique(data$trip_type))
table(data$non_paying) # N, Y
table(data$no_activity_month)

# Descending device
table(data$descending_device) # B, N, Y
table(data$bird_interaction) # B, N, Y

# Species
sort(unique(data$species_code))
sort(unique(data$species))
species_key_check <- data %>% 
  select(species_code, comm_name_orig, comm_name, sci_name) %>% 
  unique()
sort(species_key_check$species_code[is.na(species_key_check$comm_name)])

# Temperature
g <- ggplot(data, aes(y=temp_f)) +
  geom_boxplot() +
  labs(y="Temperature (°F)") +
  lims(y=c(0,100)) +
  theme_bw()
g

# Depth
g <- ggplot(data, aes(y=depth_ft)) +
  geom_boxplot() +
  labs(y="Depth (feet)") +
  lims(y=c(0, 500)) +
  theme_bw()
g

# Times
freeR::uniq(data$departure_time)
freeR::uniq(data$return_time)
freeR::uniq(data$hm_fished)

# Inspect duration
duration_key <- data %>% 
  select(hm_fished, hrs_fished) %>% 
  unique() %>% 
  arrange(hrs_fished)
duration_key_check <- duration_key %>% 
  mutate(nchar=nchar(hm_fished),
         colon_yn=grepl(":", hm_fished)) %>% 
  rowwise() %>% 
  mutate(hrs_fished2=conv_hm_to_hr(hm_fished)) %>% 
  ungroup() %>% 
  mutate(hrs_check=near(hrs_fished, hrs_fished2, tol=0.0001))

# Target species, fishing method, bait used
table(data$target_species)
table(data$fishing_method)
table(data$bait_used)



# Final processing
################################################################################

# Final format
data_out <- data %>% 
  # Add HR calculated from HM
  # (proven to be more comprehensive and identical)
  left_join(duration_key_check %>% select(hm_fished, hrs_fished2), by="hm_fished") %>% 
  select(-hrs_fished) %>% 
  rename(hrs_fished=hrs_fished2) %>% 
  select(filename:hm_fished, hrs_fished, everything()) 

# Export data
################################################################################

# Export data
saveRDS(data_out, file=file.path(outdir, "CDFW_2000_2020_cpfv_logbook_data.Rds"))

