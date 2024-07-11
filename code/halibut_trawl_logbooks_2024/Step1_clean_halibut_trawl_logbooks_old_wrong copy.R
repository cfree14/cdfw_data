

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

convert_time <- function(time){
  
  # Pad strings
  time1 <- time %>% as.character() %>% stringr::str_pad(., width=4, pad="0", side = "left")
  
  # Insert colon
  time2 <- paste0(substr(time1,1,2), ":", substr(time1,3,4))
  
  time3 <- ifelse(time2=="NA:NA", NA, time2)
  
}

calc_duration <- function(time1, time2) {
 
   # Convert time strings to lubridate period objects
  time1a <- hm(time1)
  time2a <- hm(time2)
  
  # Calculate duration for times on the same day
  same_day_diff <- as.numeric(as.duration(time2a - time1a)) / 60 / 60
  
  # Handle cases where time2 is the next day
  date1 <- ymd_hm(paste("2020-01-01", time1))
  date2 <- ymd_hm(paste("2020-01-02", time2))
  next_day_diff <- as.numeric(as.duration(date2 - date1)) / 60 / 60
  
  # Combine the results using ifelse to determine if time2 is after time1
  diff_hr <- ifelse(time2a > time1a, same_day_diff, next_day_diff)
  
  return(diff_hr)
}

# Example usage
time1 <- c("23:00", "12:30", "23:59")
time2 <- c("01:00", "13:00", "00:10")
calc_duration(time1, time2)  # Should return c(2, 0.5, 0.1833333)





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
         catch_lbs_reciept=ticket_pounds,
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
                            is.na(spp_code) & spp_code_pacfin=="USMN" ~ 300)) %>% 
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
  mutate(time_set=convert_time(time_set),
         time_up=convert_time(time_up)) %>% 
  # Calculate duration
  mutate(duration_hrs=calc_duration(time_set, time_up),
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
         net_type, target_spp_code, 
         tow_number,  duration_hrs_orig, duration_hrs, duration_diff,
         date_set, time_set_orig, time_up_orig, time_set, time_up,
         spp_code, spp_code_pacfin, species, 
         catch_lbs_est, catch_lbs_reciept, revenue_usd,
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

# Times
plot(duration_hrs ~ duration_hrs_orig, data)
duration_check <- data %>% 
  count(time_set, time_up, duration_hrs) %>% 
  unique()

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
table(data$target_strategy)

# Yes/no questions
table(data$observed_trip_yn) 
table(data$em_trip_yn)  # 0s and 1s
table(data$efp_trip_yn)
table(data$signed_yn) # 1 only
table(data$void_yn) # 0 only

# Plot map
gdata <- data %>% 
  select(lat_dd_set, long_dd_set) %>% 
  unique() %>% 
  na.omit()
ggplot(gdata, aes(x=long_dd_set, y=lat_dd_set)) +
  geom_point()


# Export data
################################################################################

# Simplify
data_out <- data %>% 
  select(-c(spp_code_pacfin, 
            time_set_orig, time_up_orig, 
            duration_hrs_orig, duration_diff,
            set_month, set_day, set_year))

str(data_out)

# Export data
saveRDS(data_out, file=file.path(outdir, "CDFW_1981_2022_halibut_trawl_data.Rds"))


