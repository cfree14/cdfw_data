

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
x <- files3d[1]
data_orig <- purrr::map_df(files3d, function(x){
  
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
         vessel_name=vsl_name,
         comm_name=common, 
         area_code=area_x,
         assignment_id=assnid,
         assignment_id_plus=assn,
         assignment_id_plus_start=assnn,
         county_code=cnty, 
         site_code=intsite,
         mode_code=mode_fx,
         state_code=st,
         subregion_code=sub_reg,
         spp_code_num=sp_code, 
         spp_code_alpha=alpha5,
         spp_code_recfin=recfinsp,
         weight_kg=wgt,
         length_mm=lngth,
         disposition=dispd,
         location_number=stop,
         sampler_date=sampdate) %>% 
  # Arrange
  select(filename,
         ref_number,
         assignment_id, assignment_id_plus, assignment_id_plus_start,
         sampler_date,
         year,  month,
         state_code, subregion_code, county_code, 
         site_code, area_code, 
         mode_code, 
         spp_code_num, spp_code_alpha, spp_code_recfin, comm_name,
         sex, length_mm, weight_kg, disposition,
         everything())
  
# Inspect
str(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CRFS_2016_2022_I3d_discards_measurement_data.Rds"))





