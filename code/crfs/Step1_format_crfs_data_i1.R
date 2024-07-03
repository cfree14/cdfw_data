

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/crfs/raw/CRFS_Sample_Data/CDFW"
outdir <- "data/confidential/crfs/processed"

# BB -  beach and bank 
# MM - man-made structure
# PR - private and rental
# PC - party and charter 

# List files
files <- list.files(indir, recursive=T)
files1 <- files[grepl("i1", files)]
files2 <- files[grepl("i2", files)]
files3 <- files[grepl("i3", files)]
files8 <- files[grepl("i8", files)]
length(files) == length(c(files1, files2, files3, files8))

# Merge data
################################################################################

# Loop through files and merge
x <- files1[1]
data_orig <- purrr::map_df(files1, function(x){
  
  # Read data
  fdata <- readxl::read_excel(file.path(indir, x)) %>% 
    mutate(filename=x) %>% 
    mutate_all(as.character)

})


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(mode_code=mode_fx,
         gear1_code=geara,
         gear2_code=gear_b,
         area1_code=area_x,
         area2_code=area_b,
         assignment_id=assnid,
         vessel_name=bname,
         vessel_id=boatn,
         anglers_n=cntrbtrs,
         county_code=cnty, 
         county_residence_code=cnty_res,
         days_n=daysf,
         days_last_month_n=ffdays12,
         interview_id=id_code,
         site_code=intsite,
         island_code=island,
         kayak_yn=kayak,
         boats_missed_n=missed,
         mode2_code=mode_f,
         mode1_code=mode_fx,
         unlicensed_n=nolic,
         target_spp1_code=prim1,
         target_spp2_code=prim2,
         state_code=st,
         state_residence_code=st_res, 
         period_code=wave,
         zipcode_residence=zip,
         district_code=district,
         port_code=port,
         status_code=status,
         subregion_code=sub_reg,
         island_code=island,
         fishery_code=sfcode,
         survey_code=survey) %>% 
  # Arrange
  select(filename, 
         ref_number,
         interview_id, 
         assignment_id,
         survey_code,
         status_code,
         year, period_code, month,
         state_code, subregion_code, county_code, district_code, 
         port_code, site_code, island_code, area1_code, area2_code,
         state_residence_code, county_residence_code, zipcode_residence,
         mode1_code, mode2_code, 
         vessel_id, vessel_name, kayak_yn,
         gear, gear1_code, gear2_code,
         fishery_code, target_spp1_code, target_spp2_code,
         boats_missed_n,
         anglers_n, unlicensed_n, days_n, days_last_month_n,
         everything())
  
# Inspect
str(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CRFS_2016_2022_I1_interview_data.Rds"))





