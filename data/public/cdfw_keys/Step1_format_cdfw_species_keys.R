

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/public/cdfw_keys/raw"
outdir <- "data/public/cdfw_keys/processed"

# Read species key
spp_key_orig1 <- readxl::read_excel(file.path(indir, "species codes 2009.xlsx")) # provided through halibut contract
spp_key_orig2 <- read.csv(file.path(indir, "SpeciesCodesExtract.csv"), na.strings=c("N/A", "")) # provided by Paulo Serpa on Sep 1, 2022
spp_key_chris <- readxl::read_excel(file.path(indir, "species_codes_that_i_know_but_cdfw_doesnt.xlsx"))


# Format species key 1 - from halibut contract
################################################################################

# This version of the CDFW species key was provided to me through the CA halibut contract.
# It includes fewer species and is dated as older than the more recent key. 
# It does not have a discontinue date. So, I treat it as less authoritative.

# Format species key
spp_key1 <- spp_key_orig1 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(spp_code_num=exsp,
         spp_code_chr=exsp_text,
         comm_name_orig=common_name,
         sci_name=scientific_name) %>%
  # Regularize common name
  mutate(comm_name=wcfish::convert_names(comm_name_orig, to="regular")) %>%
  # Format a few common names
  mutate(comm_name=recode(comm_name,
                          "Dolphin (fish)"="Dolphinfish",
                          "Black tuna skipjack"="Black skipjack tuna",
                          "Wolf (wolf-eel) eel"="Wolf-eel",
                          "Spotted cusk- eel"="Spotted cusk-eel",
                          "Group canary/vermili rockfish"="Group canary/vermilion rockfish",
                          "Pacific ocean perch rockfish"="Pacific ocean perch")) %>% 
  # Format a few scientific names
  mutate(sci_name=recode(sci_name, 
                         "Acanthocybium solanderi"="Acanthocybium solandri",             
                         "Annipis trutta"="Arripis trutta",                    
                         "Astraea undosa"="Megastraea undosa",         
                         "Callianassa californiensis"="Neotrypaea californiensis",   
                         "Cancer magister"="Metacarcinus magister",                
                         "Clupea pallasi"="Clupea pallasii pallasii",                
                         "Doscidicus gigas"="Dosidicus gigas",                 
                         "Eopsetta exilis"="Lyopsetta exilis",               
                         "Errex zachirus"="Glyptocephalus zachirus",                   
                         # "Etrumeus teres"="",                 
                         "Eusicyonia ingentus"="Sicyonia ingentis",                 
                         "Galeorhinus zyopterus"="Galeorhinus galeus",              
                         "Hemisquilla ensigera californiensis"="Hemisquilla californiensis", 
                         "Kelletia Kelleti"="Kelletia kelletii",      
                         "Lampetra tridentata"="Entosphenus tridentatus",                
                         "Loligo opalescens"="Doryteuthis opalescens",                  
                         "Lumpenus anguillaris"="Lumpenus sagitta",                
                         "Parastichopus californicus"="Apostichopus californicus",         
                         "Penaeus Californiensis"="Penaeus californiensis",              
                         "Playmera gaudichaudi"="Platymera gaudichaudii",                
                         "Pleuronectes bilineata"="Paraplagusia bilineata",              
                         "Pleuronectes isolepis"="Isopsetta isolepis",              
                         "Pleuronectes vetulus"="Parophrys vetulus",                
                         # "Procambarus clarkii"="Procambarus clarkii",                 
                         "Protothaca staminea"="Leukoma staminea",                 
                         "Rana catesbiana"="Rana catesbeiana",    # bull frog                
                         "Sardinops sagax caeruleus"="Sardinops sagax",          
                         "Strongylocentrotus franciscanu"="Mesocentrotus franciscanus",      
                         # "Symphurus atricauda"="Symphurus atricauda",              
                         "Tetrapturus audax"="Kajikia audax",                  
                         "Tresus nuttalli"="Tresus nuttallii",                   
                         "Xenistius californiensis"="Brachygenys californiensis",
                         "Sabastes/group"="Sebastes spp.",
                         "Sebastes/group"="Sebastes spp.")) %>% 
  # Mark level
  mutate(level=ifelse(freeR::nwords(sci_name)<2 | grepl("spp.|sp.|/", sci_name), "group", "species")) %>% 
  # Arrange
  select(spp_code_num, spp_code_chr, pacfin_code,
         comm_name_orig, comm_name, sci_name, everything())

# Inspect
str(spp_key1)
freeR::complete(spp_key1)

# Check scientific names
freeR::check_names(spp_key1$sci_name[spp_key1$level=="species"])

# Codes must be unique
freeR::which_duplicated(spp_key1$spp_code_num)
freeR::which_duplicated(spp_key1$spp_code_chr)

# Any duplicated
freeR::which_duplicated(spp_key1$comm_name)
freeR::which_duplicated(spp_key1$sci_name)


# Format species key 2 - from Paulo SERPA
################################################################################

# Format species key
spp_key2 <- spp_key_orig2 %>%
  # Rename
  janitor::clean_names("snake") %>% 
  rename(spp_code=species_code,
         group_code=species_group_code,
         comm_name_orig1=common_name,
         comm_name_orig2=categorized_common_name,
         sci_name=scientific_name,
         type_code=species_type_code,
         pacfin_code=pac_fin_code) %>% 
  # Convert date
  mutate(discontinue_date=lubridate::mdy(discontinue_date)) %>% 
  # Convert common name
  mutate(comm_name=wcfish::convert_names(comm_name_orig1, to="regular"),
         comm_name=stringr::str_squish(comm_name) %>% stringr::str_to_sentence()) %>% 
  # Format scientific name
  mutate(sci_name=stringr::str_squish(sci_name) %>% stringr::str_to_sentence()) %>% 
  # Append "spp" to one word scientific names
  mutate(sci_name=ifelse(freeR::nwords(sci_name)==1, paste(sci_name, "spp."), sci_name),
         sci_name=gsub("sp\\.", "spp.", sci_name)) %>% 
  # Fix some scientific names
  mutate(sci_name=recode(sci_name,
                         "Acanthocybium solanderi"="Acanthocybium solandri",          
                         "Albacore liver"="Thunnus alalunga",                   
                         "Astraea undosa"="Megastraea undosa",                  
                         "Atlantic bluefin"="Thunnus thynnus",                  
                         "Barracuda roe"="Sphyraena argentea",                     
                         "Bay smelt"="Hypomesus transpacificus",                          
                         "Bonito liver"="Sarda lineolata",                       
                         "Buck shad"="Alosa sapidissima",                        
                         "C & o turbot"="Pleuronichthys coenosus",                        
                         "Calico bass"="Paralabrax clathratus",                      
                         "Callianassa californiensis"="Neotrypaea californiensis",          
                         "Cancer magister"="Metacarcinus magister", 
                         "Clupea pallasi"="Clupea pallasii pallasii",                     
                         "Curlfin turbot"="Pleuronichthys decurrens",                   
                         "Cutthroat trout"="Oncorhynchus clarkii",                
                         "Diamond turbot"="Hypsopsetta guttulata",                     
                         "Dolly varden trout"="Salvelinus malma",                  
                         "Doscidicus gigas"="Dosidicus gigas",             
                         "Eastern brook trout"="Salvelinus fontinalis",              
                         "Eopsetta exilis"="Lyopsetta exilis",                   
                         "Errex zachirus"="Glyptocephalus zachirus",                    
                         # "Etrumeus teres"                     
                         "Eusicyonia ingentus"="Sicyonia ingentis",                 
                         "Fork tail catfish"="Ictalurus punctatus",                 
                         "Freshwater rock bass"="Ambloplites rupestris",
                         "Freshwater trout"="Salmonidae spp.",               
                         "Galeorhinus zyopterus"="Galeorhinus galeus",             
                         "Golden trout"="Oncorhynchus aguabonita",                       
                         "Guitar fish"="Rhinobatos spp.",                       
                         "Hake liver"="Merluccius productus",                        
                         "Halibut liver"="Pleuronectiformes spp.",                     
                         "Hemisquilla ensigera californiensis"="Hemisquilla californiensis",
                         "Horneyhead turbot"="Pleuronichthys verticalis",             
                         "Kelletia kelleti"="Kelletia kelletii",                  
                         "Kumamoto oyster"="Magallana sikamea",                     
                         "Lampetra tridentata"="Entosphenus tridentatus",               
                         "Large jack mackerel"="Trachurus symmetricus",                
                         "Large mouthed black bass"="Micropterus salmoides",         
                         "Large red rockfish"="Sebastes spp.",                  
                         "Loch leven brown trout"="Salmo trutta",            
                         "Loligo opalescens"="Doryteuthis opalescens",                  
                         "Lumpenus anguillaris"="Lumpenus sagitta",                
                         "Marine plants"="Marine plant spp.",                   
                         "Miscellaneous livers"="Fish spp.",              
                         "New zealand mackerel"="Scomber australasicus",               
                         "New zealand pilchard"="Sardinops sagax",             
                         "Northern anchovy"="Engraulis mordax",                   
                         "Oriental tuna"="Thunnus orientalis",
                         "Pacific cultus liver"="Ophiodon elongatus",               
                         "Parastichopus californicus"="Apostichopus californicus",         
                         "Pelican rockfish"="Sebastes spp.",                    
                         # "Penaeus californiensis"             
                         "Pleuronectes bilineata"="Paraplagusia bilineata",               
                         "Pleuronectes isolepis"="Isopsetta isolepis",             
                         "Pleuronectes vetulus"="Parophrys vetulus",               
                         # "Procambarus clarkii"                
                         "Protothaca staminea"="Leukoma staminea",              
                         "Purple coral"="Stylaster californicus",                      
                         "Rana catesbiana"="Rana catesbeiana",                    
                         "Rockfish liver"="Sebastes spp.",                    
                         "Roe shad"="Alosa sapidissima",                          
                         "Sablefish liver"="Anoplopoma fimbria",                   
                         "Sacramento perch"="Archoplites interruptus",                   
                         "Salmon roe"="Salmonidae spp.",                        
                         "Sardinops sagax caeruleus"="Sardinops sagax",           
                         "Sea elephant"="Mirounga angustirostris",                      
                         "Sea lettuce"="Ulva spp.",                       
                         "Sea lion"="Zalophus californianus",                          
                         "Shad roe"="Alosa sapidissima",                             
                         "Shark fin"="Selachii spp.",                        
                         "Shark liver"="Selachii spp.",                      
                         "Shortfin sea bass"="Cynoscion parvipinnis",                
                         "Skate liver"="Rajidae spp.",
                         "Skate wing"="Rajidae spp.",                    
                         "Small mouthed black bass"="Micropterus dolomieu",      
                         "Strongylocentrotus franciscanu"="Mesocentrotus franciscanus",     
                         "Swordfish liver"="Xiphias gladius",                 
                         # "Symphurus atricauda"                
                         "Totuava liver"="Totoaba macdonaldi",                       
                         "Tresus nuttalli"="Tresus nuttallii",                    
                         "Tuna liver"="Scombridae spp.",                         
                         "Unclassified roe"="",                  
                         "Unclassified trout"="Salmonidae spp.",             
                         "Venus clam"="Veneridae spp.",                   
                         "Whate sea bass roe"="Atractoscion nobilis",               
                         "White sea bass liver"="Atractoscion nobilis",             
                         "Xenistius californiensis"="Brachygenys californiensis")) %>% 
  # Add scientific name
  mutate(sci_name=ifelse(comm_name=="Unclassified roe", "Fish spp.", sci_name)) %>% 
  # Format a few group scientific names
  mutate(sci_name=recode(sci_name,
                         "Sabastes/group spp."="Sebastes spp.",
                         "Sebastes/group spp."="Sebastes spp.")) %>% 
  # Add level
  mutate(level=ifelse(grepl("spp.|/", sci_name), "group", "species")) %>% 
  # Arrange
  select(-comm_name_orig2) %>% 
  select(spp_code, comm_name, sci_name, level, everything())
  
# Inspect
freeR::complete(spp_key2)

# Are the species codes unique?
freeR::which_duplicated(spp_key2$spp_code) # MUST BE UNIQUE

# Are the common names unique?
freeR::which_duplicated(spp_key2$comm_name) # Should be unique? But isn't

# Are the scientific names unique?
freeR::which_duplicated(spp_key2$sci_name) # expected

# Check scientific names
freeR::check_names(spp_key2$sci_name[spp_key2$level=="species"])

# Check scientific names for groups
spp_key2$sci_name[spp_key2$level=="group"] %>% sort() %>% unique()

# Are all of the SPP1 codes in the SPP2 codes?
spp1_codes <- spp_key1$spp_code_num
spp2_codes <- spp_key2$spp_code
spp1_codes[!spp1_codes %in% spp2_codes] # 51, 101, 915 are in key 1 but not key 2!


# Compare
################################################################################

# Compare names
spp_check <- spp_key2 %>% 
  select(spp_code, comm_name, sci_name) %>% 
  left_join(spp_key1 %>% select(spp_code_num, comm_name, sci_name), by=c("spp_code"="spp_code_num")) %>% 
  mutate(sci_name_check=sci_name.x==sci_name.y)


# Merge
################################################################################

# Species 1
spp_key1_use <- spp_key1 %>% 
  # Simplify
  select(spp_code_num, comm_name, sci_name, level, pacfin_code) %>% 
  # Not in species key 2
  filter(!spp_code_num %in% spp_key2$spp_code)

# Species 2
spp_key2_use <- spp_key2 %>% 
  rename(spp_code_num=spp_code) %>% 
  select(spp_code_num, comm_name, sci_name, level, pacfin_code, discontinue_date)

# Species from me
spp_key3_use <- spp_key_chris %>% 
  select(-source) %>% 
  rename(spp_code_num=spp_code) %>% 
  filter(!spp_code_num %in% spp_key1_use$spp_code_num & !spp_code_num %in% spp_key2_use$spp_code_num)
  
# Merge
spp_key_out <- bind_rows(spp_key1_use, spp_key2_use, spp_key3_use) %>% 
  # Add character
  mutate(spp_code_chr=stringr::str_pad(spp_code_num, width=3, side="left", pad="0")) %>% 
  # Arrange
  arrange(spp_code_num) %>%
  select(spp_code_num, spp_code_chr, everything()) %>% 
  # Fix scientific names
  mutate(sci_name=recode(sci_name, 
                         "NA spp."="Invertebrate spp.",
                         "Scomber / trachurus"="Scomber/Trachurus spp.",
                         "Mycteroperca / epinephelus"="Mycteroperca/Epinephelus spp.",
                         "Kyphosidae/pomacentridae spp."="Kyphosidae/Pomacentridae spp.")) %>% 
  # Fix common names
  mutate(comm_name= recode(comm_name, 
                           "Almaco (amberjack) jack"="Almaco jack",
                           "Atlantic bluefin"="Atlantic bluefin tuna",
                           "Black tuna skipjack"="Black skipjack tuna",
                           "Black- and- yellow rockfish"="Black-and-yellow rockfish",
                           "Butterfish (pacific pompano)"="Pacific pompano",
                           "C - o turbot"="C-O turbot",
                           "C- o sole"="C-O sole",
                           "Coho) salmon roe (chinook"="Chinook/coho salmon roe",
                           "Copper (whitebelly) rockfish"="Copper rockfish",
                           "Dolphin (fish)"="Dolphinfish",
                           "Giant pacific oyster"="Giant Pacific oyster",
                           "Group black/blue rockfish"="Black/blue rockfish group",
                           "Group bocaccio/chili rockfish"="Bocaccio/chilipepper rockfish group",   
                           "Group bolina rockfish"="Bolina rockfish group",               
                           "Group canary/vermili rockfish"="Canary/vermilion rockfish group",       
                           "Group deep nearshore rockfish"="Deep nearshore rockfish group",  
                           "Group deepwater reds rockfish"="Deepwater reds rockfish group",    
                           "Group gopher rockfish"="Gopher rockfish group",   
                           "Group nearshore rockfish"="Nearshore rockfish group",            
                           "Group red rockfish"="Red rockfish group",        
                           "Group rosefish rockfish"="Rosefish rockfish group",              
                           "Group rougheye/blackspotted rockfish"="Rougheye/blackspotted rockfish group",
                           "Group shelf rockfish"="Shelf rockfish group",
                           "Group slope rockfish"="Slope rockfish group",            
                           "Group small rockfish"="Small rockfish group",
                           "Hardhead (freshwater)"="Hardhead",
                           "Invertebrate unspecified"="Unspecified invertebrate",
                           "Loch leven brown trout"="Loch Leven brown trout",
                           "Monkeyface (eel) prickleback"="Monkeyface prickleback",
                           "New zealand mackerel"="New Zealand mackerel",
                           "New zealand pilchard"="New Zealand pilchard",
                           "Ocean (pink) shrimp"="Pacific pink shrimp",
                           "Pacific - roe herring"="Pacific herring roe",
                           "Pacific - roe on kelp herring"="Pacific herring roe on kelp",
                           "Pacific ocean perch rockfish"="Pacific ocean perch",
                           "Rock unspecified crab"="Unspecified rock crab",
                           "Spotted cusk- eel"="Spotted cusk-eel",
                           "Unspecified perch- like"="Unspecified perch-like",
                           "Whate sea bass roe"="White sea bass roe",
                           "White seabass"="White sea bass",
                           "Wolf (wolf- eel) eel"="Wolf eel"))

# Inspect
freeR::complete(spp_key_out)

# Code must be unique
freeR::which_duplicated(spp_key_out$spp_code_num)

# Duplicated common names? Not ideal but they happen
freeR::which_duplicated(spp_key_out$comm_name)


# Export
################################################################################

# Export
saveRDS(spp_key_out, file=file.path(outdir, "CDFW_species_key.Rds"))
write.csv(spp_key_out, file=file.path(outdir, "CDFW_species_key.csv"), row.names=F)






