

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/landings_receipts/processed"
outdir <- "requests/output"

# Read data
data_orig <- readRDS(file.path(indir, "CDFW_2000_2020_landings_receipts.Rds"))


# Build data
################################################################################

# Build data
range(data_orig$date)
yrs <- 2000:2020
nyr <- length(yrs)
data <- data_orig %>% 
  group_by(block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id), 
            nfishers=n_distinct(fisher_id),
            nbusinesses=n_distinct(business_id),
            value_usd=sum(value_usd, na.rm=T)/nyr,
            landings_lb=sum(landings_lb, na.rm=T)/nyr) %>% 
  ungroup()

# Redact confidential data
data_nonconf <- data %>% 
  # Mark blocks that are confidential
  mutate(confidential=ifelse(nvessels >= 3 & nfishers >= 3 & nbusinesses >= 3, "", "***")) %>% 
  # Redact blocks that are confidential
  mutate(value_usd=ifelse(confidential=="***", NA, value_usd),
         landings_lb=ifelse(confidential=="***", NA, landings_lb))

# Export data
write.csv(data_nonconf, file=file.path(outdir, "gonzalez_data.csv"))


