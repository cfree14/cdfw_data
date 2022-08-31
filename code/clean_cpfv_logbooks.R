

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/cpfv_logbooks/raw/CPFV logs"
outdir <- "data/confidential/cpfv_logbooks/processed"


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(indir)

# Loop through and merge
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- read.csv(file.path(indir, x)) %>% 
    mutate(filename=x) %>% 
    select(filename, everything()) %>% 
    mutate_all(as.character)
  
})


# Clean data
################################################################################


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
         temp_f=temperature,
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
  # Format block
  mutate(block_id=as.numeric(block_id)) %>% 
  # Format depth/temperature
  mutate(depth=as.numeric(depth)) %>% 
  mutate(temp_f=as.numeric(temp_f)) %>% 
  # Format effort
  mutate(n_fishers=as.numeric(n_fishers),
         hrs_fished=as.numeric(hrs_fished)) %>% 
  # Format species
  mutate(species=stringr::str_squish(species)) %>% 
  # Format catch
  mutate(across(.cols=n_kept:n_crew_fished, .fns=as.numeric)) %>% 
  # Format operator
  mutate(operator=operator %>% stringr::str_to_title() %>% stringr::str_squish() %>% stringr::str_trim())

# Inspect
head(data)
str(data)

# Species
sort(unique(data$species))

# Operator
sort(unique(data$operator))

# Temperature
boxplot(data$depth)




