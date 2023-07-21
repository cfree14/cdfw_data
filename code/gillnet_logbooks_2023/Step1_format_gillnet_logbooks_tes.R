

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/gillnet_logbooks_2023/raw"
outdir <- "data/confidential/gillnet_logbooks_2023/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Gillnet Logs_1981-2022.xlsx"), col_types = "text")


# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(boat_num=boatno,
         permit_id=permit,
         date=fishing_date,
         target_spp1=tarspc,
         target_spp2=final_target_species,
         net_type1=drift_set,
         net_type2=final_net_type_set_drift,
         block_id=fg_blocks,
         depth_fa=depths,
         net_length_fa=net_length, 
         mesh_size_in=mesh_size,
         buoy_line_depth_ft=bouy_line_depth,
         soak_hr=hours_net_soaked,
         catch_n=num_catch,
         catch_lb=weights,
         spp_code=mlds_species_code,
         comm_name1=common_name,
         comm_name2=final_mlds_common_name) %>% 
  # Remove empty columns
  select(-c(x24, x25)) %>% 
  # Format net type
  mutate(net_type1=recode(net_type1,
                         "d"="Drift",
                         "D"="Drift",
                         "s"="Set",
                         "S"="Set")) %>% 
  # Format target species
  mutate(target_spp2=recode(target_spp2,
                            "H"="Halibut",
                            "H, X, Sole"="Halibut, Soupfin Shark, Sole",
                            "H, X, Sole, Angel"="Halibut, Soupfin Shark, Sole, Angel Shark",
                            "S"="Shark/Swordfish",
                            "W"="White Seabass",
                            "W, H"="White Seabass, Halibut",
                            "W, X"="White Seabass, Soupfin Shark")) %>% 
  # Format predator
  mutate(predator=predator %>% stringr::str_squish(.) %>% stringr::str_to_sentence(.),
         predator=recode(predator, 
                         "Blue sharks"="Blue shark",
                         "Harbor"="Harbor seal", 
                         "Harbor seals"="Harbor seal",
                         "Harbor seal + slime eels"="Harbor seal, slime eel",
                         "Harbor seals and sea lions"="Harbor seal, sea lion",
                         "Mako"="Mako shark",
                         "Sea lions"="Sea lion",
                         "Sea lion - hagfish"="Sea lion, hagfish",
                         "Sea lion, harbor seal"="Harbor seal, sea lion",
                         "Sea lions + slime eels"="Sea lion, slime eel",
                         "Sea lions and harbor seals"="Sea lion, harbor seal", 
                         "Sea lions and slime eels"="Sea lion, slime eel",
                         "Sea_lion"="Sea lion",
                         "Seal?"="Unknown",
                         "?"="Unknown",
                         "Unspecified"="Unknown",
                         "Unknown, maybe seal"="Unknown")) %>% 
  # Arrange
  select(year, date, 
         vessel_id, vessel_name, boat_num, permit_id,
         block_id,
         net_type1, net_type2, 
         depth_fa, net_length_fa, mesh_size_in, buoy_line_depth_ft, soak_hr,
         target_spp1, target_spp2,
         # block_id, depths,
         spp_code, comm_name1, comm_name2,
         status, catch_n, catch_lb, predator,
         everything())
  

# Inspect
str(data1)
freeR::complete(data1)

# Date
table(data1$year)
range(data1$date, na.rm=T)

# Net type
table(data1$net_type1) # bad
table(data1$net_type2)

# Status
table(data1$status)

# Predator
table(data1$predator) # bad

# Target species
table(data1$target_spp1) # bad
table(data1$target_spp2)


