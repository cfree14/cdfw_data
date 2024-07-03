

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/halibut_trawl_logbooks/raw"
outdir <- "data/confidential/halibut_trawl_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read data
data_n <- readxl::read_excel(file.path(indir, "Northern CA 1980-2020 tows with CHLBxlsx.xlsx"), col_types="text")
data_nc <- readxl::read_excel(file.path(indir, "North_Central 1980-2022 tows with CHLB.xlsx"), col_types="text")
data_c <- readxl::read_excel(file.path(indir, "Central CA 1981_2022 tows with CHLB.xlsx"), col_types="text")
data_s <- readxl::read_excel(file.path(indir, "Southern 1980-2022 tows with CHLB.xlsx"), col_types="text")

# Merge data
data_orig <- bind_rows(data_n, data_nc, data_c, data_s)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(logbook_id=serial_number,
         block_id=block,
         species_code_pacfin=pac_fin_species_code,
         receipt_id=landing_receipt,
         receipt_date=landing_date,
         efp_trip_yn=is_efp_trip,
         em_trip_yn=is_em_trip,
         observed_trip_yn=is_observed_trip,
         signed_yn=is_signed,
         void_yn=is_void) %>% 
  # Add set/up data
  mutate(date_set=paste(set_year, set_month, set_day, sep="-") %>% lubridate::ymd(.),
         date_up=paste(up_year, up_month, up_day, sep="-") %>% lubridate::ymd(.)) %>% 
  # Convert to numeric
  mutate(across(.cols=c(crew_size,
                        departure_port_code, return_port_code,
                        tow_number, tow_hours, 
                        set_latitude, set_longitude,
                        up_latitude, up_longitude,
                        set_month, set_day, set_year, 
                        up_month, up_day, up_year,
                        block_id,
                        average_depth, ticket_pounds, estimated_pounds, revenue), .fns=as.numeric)) %>% 
  # Arrange
  select(logbook_id, vessel_id, vessel_name, crew_size,
         departure_port_code, return_port_code,
         departure_date, departure_time,
         return_date, return_time,
         tow_number, net_type, target_strategy, codend_capacity, average_depth, tow_hours, 
         set_year, set_month, set_day, date_set, set_time, set_latitude, set_longitude,
         up_year, up_month, up_day, date_up, up_time, up_latitude, up_longitude,
         region, block_id, 
         species_code, species_code_pacfin, 
         estimated_pounds, ticket_pounds, revenue, receipt_id, receipt_date,
         everything())

# Inspect
str(data)

table(data$net_type)
table(data$target_strategy)
table(data$codend_capacity)
table(data$region)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_1981_2022_halibut_trawl_data.Rds"))


