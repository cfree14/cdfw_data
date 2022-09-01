

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


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Loop through and merge
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- read.csv(file.path(indir, x), na.strings = "") %>% 
    mutate(filename=x) %>% 
    select(filename, everything()) %>% 
    mutate_all(as.character)
  
})


# Clean data
################################################################################

# Does Oarfish (Regalecus glesne) have a code?

# Fix species
# Confirm depth/temperature units
# Are blanks in catch zeroes?

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
  # Format port code
  mutate(port_code=as.numeric(port_code)) %>% 
  # Add port details
  left_join(port_key %>% select(port_code, port, port_complex), by="port_code") %>% 
  mutate(port=ifelse(is.na(port), "Invalid", port)) %>% 
  mutate(port_complex=ifelse(is.na(port_complex), "Invalid", port_complex)) %>% 
  # Format trip type
  mutate(trip_type=stringr::str_to_sentence(trip_type)) %>% 
  # Format block
  mutate(block_id=as.numeric(block_id)) %>% 
  # Format depth/temperature
  mutate(depth_ft=as.numeric(depth_ft)) %>% 
  mutate(temp_f=as.numeric(temp_f)) %>% 
  # Format effort
  mutate(n_fishers=as.numeric(n_fishers),
         hrs_fished=as.numeric(hrs_fished)) %>% 
  # Format species
  mutate(comm_name_orig=stringr::str_squish(comm_name_orig)) %>% 
  # Format species code
  mutate(species_code=recode(species_code, 
                             "Oarfish"="") %>% as.numeric()) %>% 
  # Add species info
  left_join(species_key %>% select(spp_code_num, comm_name, sci_name), by=c("species_code"="spp_code_num")) %>%
  # Fix oarfish
  mutate(comm_name=ifelse(comm_name_orig=="Oarfish", "Oarfish", comm_name),
         sci_name=ifelse(comm_name=="Oarfish", "Regalecus glesne", sci_name)) %>% 
  # Format catch
  mutate(across(.cols=n_kept:n_crew_fished, .fns=as.numeric)) %>% 
  # Format operator
  mutate(operator=operator %>% stringr::str_to_title() %>% stringr::str_squish() %>% stringr::str_trim()) %>% 
  # Arrange
  select(filename, logbook_id, 
         year, month, day, 
         date, date_submitted, date_received,
         vessel_id, vessel_name, operator,
         port_complex, port_code, port,
         no_activity_month,
         trip_type, non_paying,
         block_id, 
         depth_ft, temp_f, 
         departure_time, return_time, hm_fished, hrs_fished, n_fishers, n_crew_fished,
         species_code, comm_name_orig, comm_name, sci_name,
         n_kept, n_released, n_lost_to_sea_lions, n_caught_by_crew,
         everything())


# Inspect data
################################################################################

# Inspect
head(data)
str(data)
# freeR::complete(data)

# Date
range(data$date)
range(data$year)
range(data$month)
range(data$day)
range(data$date_received, na.rm=T)
range(data$date_submitted, na.rm=T)

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

# Trip type
sort(unique(data$trip_type))
table(data$non_paying) # N, Y

# Descending device
table(data$descending_device) # B, N, Y
table(data$bird_interaction) # B, N, Y

# Species
sort(unique(data$species_code))
sort(unique(data$species))
species_key_check <- data %>% 
  select(species_code, comm_name_orig, comm_name, sci_name) %>% 
  unique()

# Temperature
boxplot(data$depth)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDWF_2000_2020_cpfv_logbook_data.Rds"))
