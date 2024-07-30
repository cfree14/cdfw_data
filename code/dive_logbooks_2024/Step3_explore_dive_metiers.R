

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
plotdir <- "figures/dive_logbooks_2024"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_1980_2023_dive_logbooks.Rds"))


# Analyze data
################################################################################

freeR::complete(data_orig)
table(data_orig$comm_name)

# Format data
stats <- data_orig %>% 
  # Delete duplicated rows
  unique() %>% 
  # Add dive id
  mutate(trip_id=paste(date, vessel_id, sep="-"),
         dive_id=paste(date, vessel_id, dive_number, 
                       block_id, hours, depth_min_ft, depth_max_ft, sep="-")) %>% 
  # Group
  group_by(dive_id) %>% 
  mutate(nrows=n()) %>% 
  ungroup() %>% 
  # Categorize species
  mutate(spp_catg=case_when(grepl("urchin", comm_name) ~ "Urchin",
                            grepl("cucumber", comm_name) ~ "Cucumber",
                            T ~ "Other")) %>% 
  # Calculate percents
  group_by(dive_id, spp_catg, comm_name) %>% 
  summarise(catch_lbs=sum(catch_lbs)) %>% 
  ungroup() %>% 
  # Mark target
  arrange(dive_id, desc(catch_lbs)) %>% 
  group_by(dive_id) %>% 
  summarize(nspp=n_distinct(comm_name),
            spp=paste(comm_name, collapse = ", "),
            target_spp=comm_name[1],
            target_lbs=catch_lbs[comm_name==target_spp],
            total_lbs=sum(catch_lbs)) %>% 
  ungroup() %>% 
  mutate(target_prop=target_lbs / total_lbs)

hist(stats$nspp)

ggplot(stats, aes(x=target_prop)) +
  geom_boxplot()



