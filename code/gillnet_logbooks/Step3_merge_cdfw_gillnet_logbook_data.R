

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
data1_orig <- readRDS(file=file.path(outdir, "CDFW_2000_2018_gillnet_logbooks.Rds"))
data2_orig <- readRDS(file=file.path(outdir, "CDFW_2017_2020_gillnet_logbooks.Rds"))


# Merge data
################################################################################

# Column names
colnames(data1_orig)
colnames(data2_orig)

# Date range
range(data1_orig$date)
range(data2_orig$date)

# Format data 1
data1 <- data1_orig %>%
  # Rename
  rename(spp_code=spp_code_num) %>%
  # Format
  mutate(across(.cols=c(set_id, block_id, depth_fa,
                        net_length_ft, mesh_size_in, buoy_line_depth_fa,
                        soak_hr), .fns=as.character)) %>%
  # Arrange
  select(set_id, vessel_id, vessel_name, skipper_name,
         year, date, block_id, depth_fa,
         target_spp, net_type, net_length_ft, mesh_size_in, buoy_line_depth_fa,
         soak_hr, spp_code, comm_name, status, predator, catch_n, catch_lb)

# Format data 2
data2 <- data2_orig %>%
  # Build set id
  mutate(set_id=paste(vessel_id, date, block_id, depth_fa, target_spp, net_type)) %>%
  # Arrange
  select(set_id, vessel_id, vessel_name, skipper_name,
         year, date, block_id, depth_fa,
         target_spp, net_type, net_length_ft, mesh_size_in, buoy_line_depth_fa,
         soak_hr, spp_code, comm_name, status, predator, catch_n, catch_lb) %>%
  # Use data 1 for overlapping data
  filter(date>max(data1$date))

# Merge data
data <- bind_rows(data1, data2) %>% 
  mutate(block_id1=as.numeric(block_id))


# Export data
################################################################################

# Export data
saveRDS(data, file.path(outdir, "CDFW_2000_2020_gillnet_logbook_data.Rds"))






