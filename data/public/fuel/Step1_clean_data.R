

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(priceR)
library(tidyverse)

# Directories
indir <- "data/public/fuel/raw"
outdir <- "data/public/fuel/processed"

# Read data
# Source: https://www.psmfc.org/efin/data/fuel.html#Data
data_orig <- readxl::read_excel(file.path(indir, "fuelca.xls"))

# To-do list
# 1. Adjust for inflation
# 2. Process notes (mark which include tax)

# Format data
################################################################################

# Format
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(port_code=port,
         port=portname,
         dock_code=dockcode,
         price_usd_gal=pricegal,
         price_usd_600gal=pricettl,
         price_quoted=pxquoted) %>% 
  # Add date
  mutate(date=paste(year, month, day, sep="-") %>% lubridate::ymd(.)) %>% 
  # Clean up port names
  mutate(port=recode(port, 
                     "Crescent C"="Crescent City",
                     "Port Huene"="Port Hueneme",
                     "Santa Barb"="Santa Barbara",
                     "San Franci"="San Francisco")) %>% 
  # Clean up codes
  mutate(price_quoted=recode(price_quoted,
                           "0"="No price",
                           "1" = "Price per gallon",
                           "2" = "Total price", 
                           "3" = "Both prices")) %>% 
  # Recode prices
  mutate(price_usd_gal=ifelse(price_usd_gal==0, NA, price_usd_gal),
         price_usd_600gal=ifelse(price_usd_600gal==0, NA, price_usd_600gal)) %>% 
  # Convert price per gallon to 2025 dollars
  mutate(price_usd_gal_2024=priceR::adjust_for_inflation(price=price_usd_gal,
                                                         from_date = date,
                                                         to_date=ymd("2024-01-01"),
                                                         country = "United States")) %>% 
  # Arrange
  select(year, month, day, date, everything())

# Inspect
str(data)
freeR::complete(data)

# Port key
port_key <- data %>% 
  count(port_code, port)

# Dock key
dock_key <- data %>% 
  count(port_code, port, dock_code)

# Price type
table(data$price_quoted)

# These should be linearly related
ggplot(data, aes(x=price_usd_600gal, y=price_usd_gal, color=price_quoted)) + 
  geom_point()

# Prices over time
ggplot(data, aes(x=date, y=price_usd_gal_2024, color=port)) +
  geom_point() +
  theme_bw()

# Year
table(data$year)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "EFIN_1999_2025_CA_fuel_prices.Rds"))
