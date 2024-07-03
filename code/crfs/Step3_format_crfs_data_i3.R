

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
files3 <- files[grepl("i3_", files)]
files3d <- files[grepl("i3d_", files)]
files8 <- files[grepl("i8", files)]
length(files) == length(c(files1, files2, files3, files3d, files8))

# Merge data
################################################################################

# Loop through files and merge
x <- files3[1]
data_orig <- purrr::map_df(files3, function(x){
  
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
         gear_code=gear,
         # gear2_code=gear_b,
         area_code=area_x,
         # area2_code=area_b,
         assignment_id=assnid,
         anglers_n=cntrbtrs,
         county_code=cnty, 
         interview_id=id_code,
         site_code=intsite,
         mode_code=mode_fx,
         target_spp1_code=prim1,
         target_spp2_code=prim2,
         state_code=st,
         period_code=wave,
         district_code=district,
         status_code=status,
         subregion_code=sub_reg,
         fishery_code=sfcode,
         survey_code=survey,
         spp_code_num=sp_code, 
         spp_code_alpha=alpha5,
         location_number=locn,
         species_number=spn,
         weight_kg=wgt,
         length_mm=lngth,
         sex=f_sex,
         disposition=disp3,
         wsb_scan_result=scan_rslt,
         nfish=fshinsp) %>% 
  # Arrange
  select(filename,
         ref_number,
         interview_id,
         assignment_id,
         survey_code,
         status_code,
         year, period_code, month,
         state_code, subregion_code, county_code, district_code,
         site_code, area_code, 
         mode_code, 
         gear_code, 
         fishery_code, target_spp1_code, target_spp2_code,
         anglers_n,
         spp_code_num, spp_code_alpha, nfish,
         sex, length_mm, weight_kg, disposition,
         wsb_scan_result,
         everything())
  
# Inspect
str(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CRFS_2016_2022_I3_measurement_data.Rds"))





