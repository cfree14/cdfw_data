

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/halibut_trawl_logbooks_2024/raw"
outdir <- "data/confidential/halibut_trawl_logbooks_2024/processed"
keydir <- "data/public/cdfw_keys/processed"

# Read key
spp_key_orig <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
port_key_orig <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))

# PACFIN species
pacfin_spp <- wcfish::pacfin_species


# Merge data
################################################################################

# Files 2 merge
files2merge <- list.files(indir)

# Merge data
data_orig <- purrr::map_df(files2merge, function(x){
  df <- read.csv(file.path(indir, x), as.is=T, na.strings=c(""))
})


# Helper functions
################################################################################

# Convert 500 to 05:00
format_time <- function(time){
  
  # Pad strings
  time1 <- time %>% as.character() %>% stringr::str_pad(., width=4, pad="0", side = "left")
  
  # Insert colon
  time2 <- paste0(substr(time1,1,2), ":", substr(time1,3,4))
  
  time3 <- ifelse(time2=="NA:NA", NA, time2)
  
}

convert_to_decimal_hours <- function(time_str) {
  # Split the time string into hours and minutes
  time_parts <- strsplit(time_str, ":")[[1]]
  
  # Convert hours and minutes to numeric
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  
  # Calculate decimal hours
  decimal_hours <- hours + minutes / 60
  return(decimal_hours)
}

# Convert a vector of times to decimal hours
convert_time <- function(time_vector) {
  sapply(time_vector, convert_to_decimal_hours, USE.NAMES=F)
}

# Example usage
time_vector <- c("08:30", "14:45", "23:15", "00:00")
decimal_hours <- convert_time(time_vector)


calc_duration_hr <- function(hr1, hr2){
  
  # Duration when hour2 is on the same day
  duration1 <- (hr2 - hr1)
  
  # Duration when hour2 is on the next day
  duration2 <- (24-hr1) + hr2
  
  # Select the right one
  out <- ifelse(hr2>hr1, duration1, duration2)
  
  return(out)
  
}


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Remove columns that are entirely empty
  select(-c(UpMonth, UpDay, UpYear, CodendCapacity)) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(logbook_id=serial_number,
         # Dates
         date_depart=departure_date,
         date_return=return_date,
         # Times
         time_depart=departure_time,
         time_return=return_time,
         time_set_orig=set_time,
         time_up_orig=up_time,
         duration_hrs_orig=tow_hours,
         # Ports
         port_code_depart=departure_port_code,
         port_code_return=return_port_code,
         # Vessel
         vessel=vessel_name,
         # Location
         block_id=block,
         lat_dd_set=set_latitude, 
         long_dd_set=set_longitude,
         lat_dd_up=up_latitude, 
         long_dd_up=up_longitude,
         # Depth
         depth_fa_avg=average_depth,
         # Species
         target_spp_code=target_strategy,
         spp_code=species_code,
         spp_code_pacfin=pac_fin_species_code,
         # Catch
         catch_lbs_est=estimated_pounds, 
         catch_lbs_receipt=ticket_pounds,
         revenue_usd=revenue,
         # Receipts
         receipt_ids=landing_receipt,
         receipt_dates=landing_date,
         # Trip details
         efp_trip_yn=is_efp_trip,
         em_trip_yn=is_em_trip,
         observed_trip_yn=is_observed_trip,
         signed_yn=is_signed,
         void_yn=is_void) %>% 
  # Format dates
  mutate(date_depart=lubridate::mdy(date_depart),
         date_return=lubridate::mdy(date_return)) %>% 
  # Build set date
  # Missing some: ignore warning
  mutate(date_set=paste(set_year, set_month, set_day, sep="-") %>% lubridate::ymd(.)) %>% 
  # Add ports
  left_join(port_key_orig %>% select(port_code, port), by=c("port_code_depart"="port_code")) %>% 
  rename(port_depart=port) %>% 
  left_join(port_key_orig %>% select(port_code, port), by=c("port_code_return"="port_code")) %>% 
  rename(port_return=port) %>% 
  # Add target species
  left_join(pacfin_spp %>% select(spp_code, comm_name), by=c("target_spp_code"="spp_code")) %>% 
  rename(target_spp=comm_name) %>% 
  # Fill in target species with unmatched codes with codes
  # Update a few of these based on Travis Tanaka official confirmation
  mutate(target_spp=ifelse(is.na(target_spp), target_spp_code, target_spp),
         target_spp=recode(target_spp,
                           "DTS"="Dover sole, thornyhead, sablefish",
                           "DWD"="Deep-water dover sole",
                           "NSM"="Nearshore mix",
                           "BRSH"="Bottom rockfish-shelf",
                           "BRSL"="Bottom rockfish-slope",
                           "THHD"="Thornyheads (mixed)")) %>% 
  # Format target species
  mutate(target_spp=gsub("Unsp.", "Unspecified", target_spp),
         target_spp=gsub("Misc.", "Miscellaneous", target_spp),
         target_spp=stringr::str_to_sentence(target_spp)) %>% 
  # Fill in missing species codes
  mutate(spp_code=case_when(is.na(spp_code) & spp_code_pacfin=="ARTH" ~ 201,
                            is.na(spp_code) & spp_code_pacfin=="BCAC" ~ 253,
                            is.na(spp_code) & spp_code_pacfin=="BLGL" ~ 667,
                            is.na(spp_code) & spp_code_pacfin=="CBZN" ~ 261,
                            is.na(spp_code) & spp_code_pacfin=="DBRK" ~ 257,
                            is.na(spp_code) & spp_code_pacfin=="DOVR" ~ 211,
                            is.na(spp_code) & spp_code_pacfin=="DUSK" ~ 250,
                            is.na(spp_code) & spp_code_pacfin=="EGLS" ~ 206,
                            is.na(spp_code) & spp_code_pacfin=="GRDR" ~ 198,
                            is.na(spp_code) & spp_code_pacfin=="GREN" ~ 198,
                            is.na(spp_code) & spp_code_pacfin=="LCOD" ~ 195,
                            is.na(spp_code) & spp_code_pacfin=="LSKT" ~ 147,
                            is.na(spp_code) & spp_code_pacfin=="LSPN" ~ 678,
                            is.na(spp_code) & spp_code_pacfin=="MISC" ~ 999,
                            is.na(spp_code) & spp_code_pacfin=="MSC2" ~ 999,
                            is.na(spp_code) & spp_code_pacfin=="OCRB" ~ 802,
                            is.na(spp_code) & spp_code_pacfin=="OFLT" ~ 230,
                            is.na(spp_code) & spp_code_pacfin=="PTRL" ~ 209,
                            is.na(spp_code) & spp_code_pacfin=="PWHT" ~ 495,
                            is.na(spp_code) & spp_code_pacfin=="RATF" ~ 166,
                            is.na(spp_code) & spp_code_pacfin=="RCRB" ~ 801,
                            is.na(spp_code) & spp_code_pacfin=="REDS" ~ 250,
                            is.na(spp_code) & spp_code_pacfin=="REX"  ~ 207,
                            is.na(spp_code) & spp_code_pacfin=="RPRW" ~ 813,
                            is.na(spp_code) & spp_code_pacfin=="SABL" ~ 190,
                            is.na(spp_code) & spp_code_pacfin=="SHAD" ~ 325,
                            is.na(spp_code) & spp_code_pacfin=="SKCR" ~ 804,
                            is.na(spp_code) & spp_code_pacfin=="SLNS" ~ 210,
                            is.na(spp_code) & spp_code_pacfin=="SPRW" ~ 815,
                            is.na(spp_code) & spp_code_pacfin=="SSPN" ~ 679,
                            is.na(spp_code) & spp_code_pacfin=="UCRB" ~ 802,
                            is.na(spp_code) & spp_code_pacfin=="UFLT" ~ 230,
                            is.na(spp_code) & spp_code_pacfin=="USCU" ~ 755,
                            is.na(spp_code) & spp_code_pacfin=="USHR" ~ 973,
                            is.na(spp_code) & spp_code_pacfin=="USKT" ~ 175,
                            is.na(spp_code) & spp_code_pacfin=="USMN" ~ 300,
                            T ~ spp_code)) %>%
  # Add species
  left_join(spp_key_orig %>% select(spp_code_num, comm_name), by=c("spp_code"="spp_code_num")) %>%
  rename(species=comm_name) %>%
  # Format net type
  mutate(net_type=recode(net_type,
                         "B" = "bottom", 
                         "D" = "Danish or Scottish seine", 
                         "F" = "selective flatfish", 
                         "L" = "large footrope", 
                         "M" = "midwater", 
                         "S" = "small footrope")) %>% 
  # Format region
  mutate(region=recode(region,
                       "C" = "Central", 
                       "N" = "North", 
                       "NC" = "North Central", 
                       "S" = "South")) %>% 
  # Format times
  # Some times go over 2400
  mutate(time_set=ifelse(time_set_orig>2400, time_set_orig-2400, time_set_orig),
         time_up=ifelse(time_up_orig>2400, time_up_orig-2400, time_up_orig)) %>% 
  # If both times are zero, assume unknown
  mutate(time_set=ifelse(time_set==0 & time_up==0, NA, time_set),
         time_up=ifelse(is.na(time_set) & time_up==0, NA, time_up)) %>%
  # Convert times
  mutate(time_set=format_time(time_set),
         time_up=format_time(time_up)) %>% 
  # Convert times to numeric
  mutate(time_set_num=convert_time(time_set),
         time_up_num=convert_time(time_up)) %>% 
  # Calculate duration
  mutate(duration_hrs=calc_duration_hr(time_set_num, time_up_num),
         # Use our duration if available (matches time)
         duration_hrs=ifelse(is.na(duration_hrs), duration_hrs_orig, duration_hrs),
         # Calculate difference
         duration_diff=duration_hrs-duration_hrs_orig) %>%
  # Convert longitudes
  mutate(long_dd_set=long_dd_set *-1,
         long_dd_up=long_dd_up * -1) %>% 
  # Select
  select(logbook_id,
         vessel_id, vessel, crew_size,
         date_depart, time_depart,
         date_return, time_return,
         port_code_depart, port_depart,
         port_code_return, port_return,
         region, block_id, 
         lat_dd_set, long_dd_set,
         lat_dd_up, long_dd_up,
         depth_fa_avg,
         net_type, target_spp_code, target_spp,
         tow_number,  
         date_set, 
         time_set_orig, time_up_orig, time_set, time_up, time_set_num, time_up_num,
         duration_hrs_orig, duration_hrs, duration_diff,
         spp_code, spp_code_pacfin, species, 
         catch_lbs_est, catch_lbs_receipt, revenue_usd,
         receipt_ids, receipt_dates, comments,
         observed_trip_yn, em_trip_yn,  efp_trip_yn,
         signed_yn, void_yn,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Dates
range(data$date_depart, na.rm=T)
range(data$date_return, na.rm=T)
range(data$date_set, na.rm=T)

# Port keys
port_key1 <- data %>% 
  count(port_code_depart, port_depart) # a few invalid port codes
port_key2 <- data %>% 
  count(port_code_return, port_return)

# Vessel key
vessel_key <- data %>% 
  count(vessel_id, vessel)
freeR::which_duplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel)

# Species key
spp_key <- data %>% 
  count(spp_code_pacfin, spp_code, species)
freeR::which_duplicated(spp_key$spp_code)
spp_key$spp_code_pacfin[is.na(spp_key$spp_code)]

# Region
table(data$region)

# Net type
table(data$net_type)

# Target strategy
targ_spp_key <- data %>% 
  count(target_spp_code, target_spp)
table(data$target_spp_code)

# Time key
time_key1 <- data %>% 
  count(time_set_orig, time_set, time_set_num)
time_key2 <- data %>% 
  count(time_up_orig, time_up, time_up_num)

# Tow number
table(data$tow_number)

# Times - fix druation
# plot(duration_hrs ~ duration_hrs_orig, data)
duration_check <- data %>% 
  count(time_set, time_up, duration_hrs) %>% 
  unique()

# Depth key
depth_key <- data %>% 
  count(depth_fa_avg)

# Yes/no questions
table(data$observed_trip_yn) # N and Y
table(data$em_trip_yn)  # 0s and 1s
table(data$efp_trip_yn) # N and Y
table(data$signed_yn) # 1 only
table(data$void_yn) # 0 only

# Plot map
gdata <- data %>% 
  select(lat_dd_set, long_dd_set) %>% 
  unique() %>% 
  na.omit()
ggplot(gdata, aes(x=long_dd_set, y=lat_dd_set)) +
  geom_point()


# Build vessel key
################################################################################

# Vessel key use
vessel_key_use <- data %>% 
  # Correct names
  mutate(vessel=recode(vessel, 
                       "F/V NORTHERN LIGHT"="NORTHERN LIGHT",
                       "SEAHAWK"="SEA HAWK")) %>% 
  # Unique
  count(vessel_id, vessel) %>% 
  # Ones with names
  filter(!is.na(vessel)) %>% 
  # Remove ones with two names
  filter(!vessel_id %in% c(13549, 20026, 28019, 37902))
  
freeR::which_duplicated(vessel_key_use$vessel_id)

# Export data
################################################################################

# Simplify
data_out <- data %>% 
  # Remove useless
  select(-c(spp_code_pacfin, 
            time_set_orig, time_up_orig, 
            duration_hrs_orig, duration_diff,
            set_month, set_day, set_year)) %>% 
  # Add in harmonized vessel name
  rename(vessel_orig=vessel) %>% 
  left_join(vessel_key_use %>% select(vessel_id, vessel), by="vessel_id") %>% 
  relocate(vessel, .after=vessel_id) %>%
  # Use original name when harmonized name missing (for ones with two names associated with id)
  mutate(vessel=ifelse(is.na(vessel), vessel_orig, vessel)) %>% 
  select(-vessel_orig)

str(data_out)
freeR::complete(data_out)

# Export data
saveRDS(data_out, file=file.path(outdir, "CDFW_1981_2022_halibut_trawl_data.Rds"))


