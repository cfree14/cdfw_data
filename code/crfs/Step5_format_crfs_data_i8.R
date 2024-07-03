

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
files8 <- files[grepl("i8_", files)]
files8a <- files[grepl("i8a_", files)]
files8b <- files[grepl("i8b_", files)]
files8c <- files[grepl("i8c_", files)]
files8d <- files[grepl("i8d_", files)]
length(files) == length(c(files1, files2, files3, files3d, 
                          files8, files8a, files8b, files8c, files8d))


# Merge data
################################################################################

# Loop through files and merge
x <- files8[1]
data_orig <- purrr::map_df(files8, function(x){
  
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
         area_code=area_x,
         assignment_id=assnid,
         county_code=cnty, 
         site_code=intsite,
         mode_code=mode_fx,
         state_code=st,
         island_code=island,
         subregion_code=sub_reg,
         block_id1=block1,
         microblock_id1a=box1a,
         microblock_id1b=box1b,
         microblock_id1c=box1c,
         block_id2=block2,
         microblock_id2a=box2a,
         microblock_id2b=box2b,
         microblock_id2c=box2c,
         district_code=district,
         period_code=wave) %>% 
  # Arrange
  select(filename,
         ref_number,
         assignment_id, 
         year,  month, period_code,
         state_code, subregion_code, county_code, district_code,
         island_code, site_code, area_code,
         mode_code, 
         everything())
  
# Inspect
str(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CRFS_2016_2022_I3d_discards_measurement_data.Rds"))





