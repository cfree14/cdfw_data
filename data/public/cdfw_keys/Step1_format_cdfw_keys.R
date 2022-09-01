

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/public/cdfw_keys/raw"
outdir <- "data/public/cdfw_keys/processed"


# Format species key
################################################################################

# Read species key
spp_key_orig <- readxl::read_excel(file.path(indir, "species codes 2009.xlsx"))
spp_key_chris <- readxl::read_excel(file.path(indir, "species_codes_that_i_know_but_cdfw_doesnt.xlsx"))

# Format species key
spp_key <- spp_key_orig %>%
  # Merge
  bind_rows(spp_key_chris %>% select(-Source) %>% mutate(`EXSP text`=as.character(`EXSP text`))) %>% 
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
                         # "Tresus nuttalli"="",                   
                         "Xenistius californiensis"="Brachygenys californiensis",
                         "Sabastes/group"="Sebastes spp.",
                         "Sebastes/group"="Sebastes spp.")) %>% 
  # Mark level
  mutate(level=ifelse(freeR::nwords(sci_name)<2 | grepl("spp.|sp.|/", sci_name), "group", "species")) %>% 
  # Arrange
  select(spp_code_num, spp_code_chr, pacfin_code,
         comm_name_orig, comm_name, sci_name, everything())

# Inspect
str(spp_key)
freeR::complete(spp_key)

# Check scientific names
freeR::check_names(spp_key$sci_name[spp_key$level=="species"])

# Codes must be unique
freeR::which_duplicated(spp_key$spp_code_num)
freeR::which_duplicated(spp_key$spp_code_chr)

# Any duplicated
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)

# Export
saveRDS(spp_key, file=file.path(outdir, "CDFW_species_key.Rds"))


# Format port key
################################################################################

# Read port key
port_key_orig <- read.csv(file.path(indir, "PortCodesExtract.csv"), as.is=T)


# Format port key
port_key <- port_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(port=port_name,
         port_complex=major_port_name,
         port_complex_code=major_port_code) %>%
  # Format port
  mutate(port=stringr::str_to_title(port),
         port_complex=stringr::str_to_title(port_complex)) %>%
  # Format date
  mutate(discontinued_date=lubridate::mdy(discontinued_date)) %>%
  # Arrange
  select(port_complex, port_complex_code, port, port_code, everything()) %>%
  arrange(port_complex, port)

# Inspect
str(port_key)

# Export
saveRDS(port_key, file=file.path(outdir, "CDFW_port_key.Rds"))


# Format gear key
################################################################################

# Read port key
gear_key_orig <- read.csv(file.path(indir, "GearCodesExtract.csv"), as.is=T)

# Format port key
gear_key <- gear_key_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(gear=gear_description,
         discontinued_date=discontinue_date,
         gear_type=gear_type_chris) %>%
  # Format gear
  mutate(gear=stringr::str_to_title(gear)) %>%
  # Format date
  mutate(discontinued_date=lubridate::mdy(discontinued_date))

# Inspect
str(gear_key)

# Export
saveRDS(gear_key, file=file.path(outdir, "CDFW_gear_key.Rds"))



