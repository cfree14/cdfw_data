
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
shpdir <- "data/public/blocks/shapefiles"
datadir <- "data/public/blocks/data"
plotdir <- "data/public/blocks/figures"

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")
utm10 <- sf::st_crs(" +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ")

# Read data
blocks_orig <- sf::st_read(dsn=shpdir, layer="CDFW_intl_blocks_try1_clipped")

# Read vertices
verts_orig <- readxl::read_excel(file.path(datadir, "CDFW_intl_block_vertices.xlsx"))

# USA
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf")


# Export data
################################################################################

# Export
saveRDS(blocks_orig, file.path(datadir, "CDFW_intl_blocks.Rds"))
sf::st_write(blocks_orig, dsn=file.path(datadir, "CDFW_intl_blocks.shp"))


# Format vertices
################################################################################

# Format vertices
verts <- verts_orig %>%
  # Add type
  mutate(type=ifelse(is.na(type), "vertex", type)) %>%
  # Build label
  mutate(label=paste(long_dd %>% round(2), lat_dd %>% round(2), sep=", "))

# Filter verts
verts_use <- verts %>% 
  filter(type=="vertex") %>% 
  filter(long_dd>-120)

# Format blocks
blocks <- blocks_orig %>% 
  rename(block_id=id)


# Function to derive coordinates
################################################################################

# Calc interpolayted longs
pt1 <- c(-79, 5); pt2 <- c(-82, -2); lat <- 2
pt1 <- c(-116.5, 27.5); pt2 <- c(-118.5, 32.5); lat <- 31
pt1 <- c(-116.5, 27.5); pt2 <- c(-118.5, 32.5); lat <- 29
pt1 <- c(-116.5, 27.5); pt2 <- c(-111, 22); lat <- 25.5
pt1 <- c(-116.5, 27.5); pt2 <- c(-111, 22); lat <- 23.6
pt1 <- c(-107.5, 20); pt2 <- c(-102, 16.25); lat <- 18
calc_long <- function(pt1, pt2, lat){

  # Derive slope and intercept
  # y = mx+b
  # b = y - mx
  # x = (y - b) / m
  m <- (pt2[2]-pt1[2]) / (pt2[1]-pt1[1])
  b <- pt1[2]-pt1[1]*m

  # Derive new long
  long <- (lat - b) / m
  long

}



# Plot
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot() +
  # Plot world
  geom_sf(data=world, fill="grey90", col="white", lwd=0.2) +
  # Plot blocks
  geom_sf(data=blocks, fill=NA, color="grey40", lwd=0.2) +
  # Plot block labels
  geom_sf_text(data=blocks, mapping=aes(label=block_id), size=2.8) +
  # Scales
  scale_x_continuous(breaks=seq(-120,-70,5)) +
  scale_y_continuous(breaks=seq(-40,30,5)) +
  # Crop
  coord_sf(xlim=c(-120, -68), ylim=c(-40, 31)) + # all
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "CDFW_intl_blocks.pdf"),
       width=8.5, height=11, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "CDFW_intl_blocks.png"),
       width=8.5, height=11, units="in", dpi=600)


# Plot Central
################################################################################

# Plot
g <- ggplot() +
  # Plot world
  geom_sf(data=world, fill="grey90", col="white", lwd=0.2) +
  # Plot blocks
  geom_sf(data=blocks, fill=NA, color="grey30", lwd=0.2) +
  # Plot block labels
  geom_sf_text(data=blocks, mapping=aes(label=block_id), size=2, color="grey50") +
  # Plot vertices
  geom_point(data=verts_use, mapping=aes(x=long_dd, y=lat_dd)) +
  geom_text(data=verts_use, mapping=aes(x=long_dd, lat_dd, label=label), hjust=1.1, vjust=1.3, size=2) +
  # Crop
  coord_sf(xlim=c(-120, -70), ylim=c(5, 32)) + # Central America
  # coord_sf(xlim=c(-125, -70), ylim=c(-40, 7)) + # South America
  # coord_sf(xlim=c(-120, -68), ylim=c(-40, 31)) + # all
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "CDFW_intl_blocks_central_america.pdf"),
       width=11, height=8.5, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "CDFW_intl_blocks_central_america.png"),
       width=11, height=6.5, units="in", dpi=600)


# Plot South
################################################################################

# Plot
g <- ggplot() +
  # Plot world
  geom_sf(data=world, fill="grey90", col="white", lwd=0.2) +
  # Plot blocks
  geom_sf(data=blocks, fill=NA, color="grey30", lwd=0.2) +
  # Plot block labels
  geom_sf_text(data=blocks, mapping=aes(label=block_id), size=2, color="grey50") +
  # Plot vertices
  geom_point(data=verts_use, mapping=aes(x=long_dd, y=lat_dd)) +
  geom_text(data=verts_use, mapping=aes(x=long_dd, lat_dd, label=label), hjust=1.1, vjust=1.3, size=2) +
  # Crop
  # coord_sf(xlim=c(-120, -70), ylim=c(5, 32)) + # Central America
  coord_sf(xlim=c(-125, -70), ylim=c(-40, 7)) + # South America
  # coord_sf(xlim=c(-120, -68), ylim=c(-40, 31)) + # all
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "CDFW_intl_blocks_south_america.pdf"),
       width=11, height=8.5, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "CDFW_intl_blocks_south_america.png"),
       width=9.75, height=8.5, units="in", dpi=600)
