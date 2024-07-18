

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outdir1 <- "data/confidential/squid_logbooks/processed"
outdir2 <- "data/confidential/squid_lightboat_logbooks/processed"
outdir3 <- "data/confidential/landing_receipts_2023/processed/"

# Read data
data1 <- readRDS(file=file.path(outdir1, "CDFW_1999_2022_squid_logbook_data.Rds"))
data2 <- readRDS(file=file.path(outdir1, "CDFW_2000_2022_squid_lightboat_logbook_data.Rds"))
data3 <- readRDS(file=file.path(outdir3, "1980_2022_landings_receipts.Rds"))


