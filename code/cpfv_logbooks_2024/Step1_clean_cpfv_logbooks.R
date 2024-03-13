

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/cpfv_logbooks_2024/raw"
outdir <- "data/confidential/cpfv_logbooks_2024/processed"
keydir <- "data/public/cdfw_keys/processed"


# Merge data
################################################################################

# Files to merge
files2merge <- list.files(indir, recursive = T, pattern=".csv")

# Loop through files and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  data_f <- read.csv(file.path(indir, x), colClasses="character", na.strings = "") %>% 
    mutate(filename=basename(x)) %>% 
    select(filename, everything())
  
  
})


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(logbook_id=serial_number,
         logbook_id_flag=serial_number_flag,
         date=log_date, 
         date_flag=log_date_flag, 
         month=log_month,
         day=log_day,
         year=log_year, 
         block_id=block,
         block_id_flag=block_flag,
         sst_f=temperature) %>% 
  # Format date
  mutate(date=lubridate::mdy(date),
         date_received=lubridate::mdy(date_received),
         date_submitted=lubridate::mdy(date_submitted)) %>% 
  # Convert to numeric
  mutate(across(.cols=c(month, day, year, port_code, block_id, 
                        hours_fished, number_of_fishers, depth, sst_f,
                        number_kept, number_released, number_lost_to_sea_lions,
                        number_of_fish_caught_by_crew, number_of_crew_fished), .fns=as.numeric))


# Inspect
str(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "1980_2022_cpfv_logbooks.Rds"))



