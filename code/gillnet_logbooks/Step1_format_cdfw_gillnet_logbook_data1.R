

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/gillnet_logbooks/raw"
outdir <- "data/confidential/gillnet_logbooks/processed"
plotdir <- "data/confidential/gillnet_logbooks/figures"

# Read data
list.files(indir)
data_orig <- readxl::read_excel(file.path(indir, "2000-18_GillnetLogbookDataset_220502.xlsx"), col_types = "text", na=c("N/A"))

# Read species key
spp_key <- readRDS("data/public/cdfw_keys/processed/CDFW_species_key.Rds") %>% 
  select(comm_name, sci_name, spp_code_num) %>% 
  arrange(comm_name, spp_code_num) %>% 
  group_by(comm_name) %>% 
  slice(1) %>% 
  ungroup()
freeR::which_duplicated(spp_key$comm_name)

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(date=fishing_date,
         vessel_name=current_name,
         vessel_id=cdfw_vessel_id,
         license_num=cfl,
         target_spp=tarspc,
         net_type=drift_set,
         net_length_ft=net_length,
         mesh_size_in=mesh_size,
         buoy_line_depth_fa=bouy_line_depth,
         block_id=fg_blocks,
         comm_name_orig=common_name,
         catch_n=nocatch,
         catch_lb=weights,
         depth_fa=depths,
         soak_hr=hours_net_soaked) %>%
  # Remove last two rows which are crazy
  slice(1:(nrow(.)-2)) %>%
  # Format date
  mutate(date=openxlsx::convertToDate(date) %>% as.character() %>% lubridate::ymd(),
         year=lubridate::year(date)) %>%
  # Convert to numeric
  mutate(set_id=as.numeric(set_id),
         vessel_id=as.numeric(vessel_id),
         license_num=as.numeric(license_num),
         block_id=as.numeric(block_id),
         depth_fa=as.numeric(depth_fa),
         soak_hr=as.numeric(soak_hr),
         net_length_ft=as.numeric(net_length_ft),
         mesh_size_in=as.numeric(mesh_size_in),
         buoy_line_depth_fa=as.numeric(buoy_line_depth_fa),
         catch_n=as.numeric(catch_n),
         catch_lb=as.numeric(catch_lb)) %>%
  # Format status
  mutate(status=stringr::str_to_sentence(status)) %>%
  # Format net type
  mutate(net_type=stringr::str_to_upper(net_type),
         net_type=recode(net_type, "D"="Drift", "S"="Set")) %>% # What are X and H?
  # Format target species
  mutate(target_spp=target_spp %>% gsub("_", " ", .) %>% stringr::str_to_sentence(.),
         target_spp=recode(target_spp,
                           "H"="Halibut",
                           "B"="Barracuda",
                           "C"="White croaker",
                           "W"="White seabass",
                           "S"="Shark/swordfish",
                           "X"="Soupfin shark",
                           "Croaker kingfish"="White croaker",
                           "Wite seabass"="White seabass")) %>%
  # Format predator
  mutate(predator=predator %>% gsub("_", " ", .) %>% stringr::str_to_sentence(),
         predator=recode(predator,
                         "150"="Shark",
                         "Harbor"="Harbor seal",
                         "Seal bird"="Seal, bird")) %>%
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name_orig)) %>%
  mutate(comm_name=recode(comm_name,
                          "Agars"="Agar",
                          "Alb-albacore tuna"="Albacore tuna",
                          "Alv-thresher shark"="Thresher shark",
                          "Bet-bigeye tuna"="Bigeye tuna",
                          "Black skipjack"="Black skipjack tuna",
                          "Blt-bullet tuna"="Bullet mackerel",
                          "Bluntnose sixgill (mud) shark"="Sixgill shark",
                          "Bocaccio"="Bocaccio rockfish",
                          "Bony fishes, marine"="Unspecified fish",
                          "Brown smoothhound"="Brown smoothhound shark",
                          "Bsh-blue shark"="Blue shark",
                          "Bsk-basking shark"="Basking shark",
                          "Bth-bigeye thresher"="Bigeye thresher shark",
                          "Bzx-bonitos"="Unspecified bonito",
                          "Chilipepper"="Chilipepper rockfish",
                          "Clams nei"="Unspecified clam",
                          "Colonial invertebrates nei"="Colonial invertebrates",
                          "Copper (whitebelly) rockfish"="Copper rockfish",
                          "Cow sharks nei"="Cow sharks",
                          "Cowcod"="Cowcod rockfish",
                          "Croakers & kingfishes nei"="Unspecifed croaker",
                          "Crustacea"="Unspecified crustacean",
                          "Curlfin sole"="Curlfin turbot",
                          "Dlp-dolphins nei"="Unspecified dolphin",
                          "Dol-mahi mahi (dorado)"="Dolphinfish",
                          "Eastern pacific bonito"="Pacific bonito",
                          "Flatfishes  (sole, flounder, turbot)"="Unspecified flatfish",
                          "Flounders & turbots nei"="Unspecified flounder",
                          "Gray smoothhound"="Gray smoothhound shark",
                          "Great white shark"="White shark",
                          "Groupers & sea basses"="Grouper",
                          "Hagfishes"="Unspecified hagfish",
                          "Hydrozoans"="Jellyfish",
                          "Invertebrate, unspecified"="Unspecified invertebrate",
                          "King crabs"="King crab",
                          "Mackerel-like fishes nei"="Unspecified mackerel",
                          "Mar-marlins (billfishes) nei"="Unspecified billfish",
                          "Mls-striped marlin"="Striped marlin",
                          "Mollusks nei"="Unspecified mollusk",
                          "Mox-ocean sunfish (mola)"="Ocean sunfish",
                          "Myf-bat ray"="Bat ray",
                          "Ocean (pink) shrimp"="Pacific pink shrimp",
                          "Octopuses"="Unspecified octopus",
                          "Pacific (chub) mackerel"="Pacific mackerel",
                          "Pacific angelshark"="Pacific angel shark",
                          "Pacific bluefin tuna"="Bluefin tuna",
                          "Pacific pomfret"="Pacific pompano",
                          "Pacific staghorn sculpin"="Staghorn sculpin",
                          "Perch-like fishes"="Unspecified perch-like",
                          "Piked (spiny) dogfish"="Spiny dogfish shark",
                          "Pth-pelagic thresher"="Pelagic thresher shark",
                          "Rainbow seaperch"="Rainbow surfperch",
                          "Raj-rays & skates, nei"="Unspecified ray",
                          "Rock crabs"="Unspecified rock crab",
                          "Rockfishes"="Unspecified rockfish",
                          "Rubberlip seaperch"="Rubberlip surfperch",
                          "Sanddabs"="Sanddab",
                          "Sea cucumbers nei"="Unspecified sea cucumber",
                          "Sea snails nei"="Sea snail",
                          "Sea stars nei"="Sea stars",
                          "Shk-sharks nei, various"="Unspecified shark",
                          "Shrimps nei (caridea)"="Unspecified shrimp",
                          "Skates, rays, mantas nei"="Unspecified ray",
                          "Skj-skipjack tuna"="Skipjack tuna",
                          "Sma-shortfin mako"="Shortfin mako shark",
                          "Smelts nei"="True smelts",
                          "Soupfin (tope) shark"="Soupfin shark",
                          "Spider (sheep claws) crabs"="Spider/sheep claws crab",
                          "Spz-smooth hammerhead shark"="Smooth hammerhead shark",
                          "Stt-stingrays nei"="Stingray",
                          "Surf perches nei"="Unspecified surfperch",
                          "Swo-swordfish"="Swordfish",
                          "Thornback"="Thornback skate",
                          "Tri-triggerfishes, durgons nei"="Triggerfish",
                          "Tun-tunas nei"="Unspecified tuna",
                          "White seaperch"="White surfperch",
                          "White seabass"="White sea bass",
                          "Wolf-eel"="Wolf eel",
                          "Yft-yellowfin tuna"="Yellowfin tuna",
                          "Yrg-pacific barracuda"="California barracuda",
                          "Ytc-yellowtail"="Yellowtail")) %>%
  left_join(spp_key %>% select(comm_name, sci_name, spp_code_num), by="comm_name") %>%
  # Format skipper
  mutate(skipper_name=stringr::str_to_upper(skipper_name)) %>% 
  # Format vessel
  mutate(vessel_name=recode(vessel_name, 
                            "CHARLOTTE V."="CHARLOTTE V",
                            "THREE BOYS (CF2036TJ)"="F/V THREE BOYS",
                            "MILEA"="MILEA, MALIA",
                            "DONNA MARIE"="DONNA MARIE, CRAZY AUSSIE")) %>% 
  # Add missing vessel ids (you looked at the permit data)
  mutate(vessel_id=case_when(vessel_num=="537827" ~ 20774,
                             vessel_num=="656254" ~ 37800,
                             vessel_num=="CF3836SA" ~ 28414,
                             vessel_num=="592017" ~ 30330, 
                             vessel_num=="570760" ~ 33111,    
                             vessel_num=="611940" ~ 33784,    
                             vessel_num=="638547" ~ 36135,    
                             vessel_num=="CF5821BS" ~ 38674,  
                             vessel_num=="622026" ~ 35101,    
                             vessel_num=="647513" ~ 35207,    
                             vessel_num=="512678" ~ 17466,    
                             vessel_num=="550580" ~ 24674,    
                             vessel_num=="610884" ~ 42580,    
                             vessel_num=="690689" ~ 39360,    
                             vessel_num=="563793" ~ 24723,   
                             vessel_num=="608155" ~ 33030,    
                             vessel_num=="559214" ~ 24498,    
                             vessel_num=="CF2776KR" ~ 43328,  
                             vessel_num=="611816" ~ 33828,    
                             vessel_num=="590891" ~ 30767,    
                             vessel_num=="956837" ~ 41378,    
                             vessel_num=="1044140" ~ 6620,   
                             vessel_num=="628101" ~ 35495,   
                             vessel_num=="CF4618GW" ~ 36080,  
                             vessel_num=="591067" ~ 31699,    
                             vessel_num=="627282" ~ 35341,    
                             vessel_num=="1078115" ~ 54376, 
                             vessel_num=="CF1296HU" ~ 39188,  
                             vessel_num=="292133" ~ 16612,    
                             vessel_num=="CF4430GJ" ~ 32550,  
                             vessel_num=="CF4086SD" ~ 48439,  
                             vessel_num=="CF3193ED" ~ 26478,  
                             vessel_num=="597524" ~ 30958,    
                             vessel_num=="276600" ~ 2329,   
                             vessel_num=="978988" ~ 42606,    
                             vessel_num=="906370" ~ 41872,    
                             vessel_num== "553607" ~ 23295,    
                             vessel_num=="1217070" ~ 7044,   
                             vessel_num=="620078" ~ 36463,   
                             vessel_num=="CF2803ST" ~ 1117,  
                             vessel_num=="CF4869SS" ~ 51824,  
                             vessel_num== "550062" ~ 22580,    
                             vessel_num=="CF9713TK" ~ 3239,  
                             vessel_num=="CF3571SU" ~ 48422,  
                             vessel_num=="CF0318CV" ~ 42598,  
                             vessel_num=="253247" ~ 8724,    
                             vessel_num=="644228" ~ 70613,    
                             vessel_num=="509557" ~ 16740,    
                             vessel_num=="618104" ~ 34725,    
                             vessel_num=="635102" ~ 36545,    
                             vessel_num=="545886" ~ 33028,   
                             vessel_num=="584748" ~ 16748,    
                             vessel_num=="295397" ~ 14491,    
                             vessel_num=="969797" ~ 43377,    
                             vessel_num=="CF9388HD" ~ 37537,  
                             vessel_num=="571063" ~ 27622,    
                             vessel_num=="552120" ~ 22774,    
                             vessel_num=="1107052" ~ 7350,   
                             vessel_num=="CF2036TJ" ~ 6575,  
                             vessel_num=="571399" ~ 7181,    
                             vessel_num=="248798" ~ 1633,    
                             vessel_num=="629818" ~ 35766,   
                             T ~ vessel_id)) %>% 
  # Add block type
  left_join(blocks_df %>% select(block_id, block_type), by="block_id") %>%
  mutate(block_type=ifelse(is.na(block_type), "Invalid", block_type)) %>%
  # Arrange
  select(set_id, vessel_id, vessel_num, vessel_name, license_num, skipper_name,
         year, date, block_type, block_id, depth_fa,
         target_spp, net_type, net_length_ft, mesh_size_in, buoy_line_depth_fa,
         soak_hr,
         comm_name_orig, comm_name, sci_name, spp_code_num,
         status, predator, catch_n, catch_lb)

# All species matched CDFW key?
spp_key1 <- data %>%
  select(comm_name_orig, comm_name, sci_name, spp_code_num) %>%
  unique()
spp_key1 %>% filter(is.na(spp_code_num)) %>%
  pull(comm_name) %>% sort()

# Inspect
str(data)
freeR::complete(data)

# Inspect
sort(unique(data$set_id))
table(data$vessel_name)
range(data$date, na.rm=T)
table(data$status)
table(data$net_type) # What are H and X?
table(data$target_spp) # What are J, M, N, O, R, T, and Z?
range(data$depth_fa, na.rm=T)
table(data$comm_name)
table(data$predator)


# Skipper work
################################################################################

# I didn't finish updating the skipper

# Skipper key
skipper_key_n <- data %>%
  # Unique skipper names
  count(license_num, skipper_name)
skipper_key <- skipper_key_n %>%
  # All skipper names for each license
  group_by(license_num) %>% 
  summarize(skipper_list=paste(skipper_name[!is.na(skipper_name)], collapse=", ")) %>% 
  # Add final skipper name
  mutate(skipper_name=recode(skipper_list, 
                             "THOMAS PTAK, TOMMY PTAK"="THOMAS PTAK",
                             "T WILMARTH, T. W. LMARTH, T. WILMARTH, T.W. LMARTH, WILMARTH"="T WILMARTH",
                             "STEVE, STEVE MINTZ"="STEVE MINTZ",
                             "MICHAEL KUCURA, MIKE KUCURA"="MICHAEL KUCURA",
                             "MICHAEL HARRISON, MIKE HARRISON"="MICHAEL HARRISON",
                             "M KASTLUNGER, M.KASTLUNGER"="M KASTLUNGER",
                             "L SANFILIPPO JR, L SANFILLIPO JR, L. SANFILIPPO, L. SANFILIPPO JR, L. SANFILIPPO JR."="L SANFILIPPO JR",
                             "MULCAHY, TIM MULCAHY, TIM MULCALY"="TIM MULCAHY",
                             "A MAKUL, A. MAKUL, MAKUL"="A MAKUL",
                             "AN, ANDY RASMUSSEN, RASMUSSEN"="ANDY RASMUSSEN",
                             "BALL, BOB BALL"="BOB BALL",
                             "BATEMAN, J BATEMAN, J. BATEMAN, JOHN BATEMAN"="JOHN BATEMAN",
                             "BILL, BILL SUTTON"="BILL SUTTON",
                             "BURKE, GARY BURKE"="GARY BURKE",
                             "CARLO SANFILIPPO, SANFILIPPO"="CARLO SANFILIPPO",
                             "CHRIS WILLIAMS, WILLIAMS"="CHRIS WILLIAMS",
                             "CURTIS HEBERT, CURTIS HUBURT"="CURTIS HEBERT",
                             "DAVID HAWORTH, HAWORTH"="DAVID HAWORTH",
                             "DELL, KEVIN DELL"="KEVIN DELL",
                             "ETTINGER, MICHAEL ETTINGER"="MICHAEL ETTINGER",
                             "G KUGLIS, G. KUGLIS, KUGLIS"="G KUGLIS",
                             "J. CARUSO, JOE, JOE CARUSO"="JOE CARUSO",
                             "JEFF, JEFF VOUAUX, JEFFERY VOUAUX, JEFFREY VOUAUX"="JEFFREY VOUAUX",
                             "JOE, JOSE CESENA"="JOSE CESENA",
                             "LEBECK, M LEBECK, M. LEBACK, M. LEBECK, M.LEBECK, M.LEBEEK, MARK LEBECK"="MARK LEBECK",
                             "M.LEBECK"="MARK LEBECK",
                             "M.LEBEEK"="MARK LEBECK",
                             "MIKE HARRISON"="MICHAEL HARRISON",
                             "PIETRO SANFILIPPO, SANFILIPPO"="PIETRO SANFILIPPO",
                             "R FOSTER, R. FOSTER, ROBERT FOSTER"="ROBERT FOSTER",
                             "STEVE MARDESICH, STEVEN MARDESICH"="STEVE MARDESICH",
                             "STEVEN PANTA, STEVEN PANTO"="STEVEN PANTO",
                             "ROACH, ZACHARY ROACH"="ZACHARY ROACH",
                             "CARLOS RIVAS, RIVAS"="CARLOS RIVAS",
                             "M.KASTLUNGER"="M KASTLUNGER"))

# Set key
set_key <- data %>%
  group_by(vessel_id, vessel_num, date, block_id, target_spp, depth_fa, net_length_ft, mesh_size_in, soak_hr) %>%
  summarize(nsetids=n_distinct(set_id)) %>%
  arrange(desc(nsetids))


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CDFW_2000_2018_gillnet_logbooks.Rds"))

