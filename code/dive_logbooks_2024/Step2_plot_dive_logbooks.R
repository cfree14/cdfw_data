

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/dive_logbooks_2024/raw"
outdir <- "data/confidential/dive_logbooks_2024/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/dive_logbooks_2024"

# Read data
data <- readRDS(file=file.path(outdir, "CDFW_1980_2023_dive_logbooks.Rds"))
blocks_sf <- wcfish::blocks


# Themes
################################################################################

# Theme
bar_theme <- theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   plot.subtitle = element_text(size=6, face="italic"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Map theme
map_theme <- theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=8),
                   plot.subtitle = element_text(size=6, face="italic"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot completeness
################################################################################

# Percent NAs
n_nas <- freeR::complete(data)
p_nas <- 1-n_nas/nrow(data)

# Build data
p_nas_df <- tibble(column=colnames(data),
                   prop=p_nas) %>% 
  # Format variable
  mutate(column=column %>% gsub("_", " ", .) %>% stringr::str_to_sentence(),
         column=recode(column,
                       "Depth min ft"="Depth min (ft)",
                       "Depth max ft"="Depth max (ft)",
                       "Sci name"="Scientific name",
                       "Comm name"="Common name",
                       "Catch lbs"="Catch (lbs)",
                       "Lat dd"="Latitude (°N)",
                       "Long dd"="Longitude (°W)")) %>% 
  # Remove columns
  filter(!column %in% c("Dive id", "Block state", "Block type", "Port complex", "Vessel combo", 
                        "Species id lbs combo", "Scientific name", "Location orig", "Block id gps", "Latlong good yn")) %>% 
  # Mark
  mutate(complete_yn=ifelse(prop==1, "Complete", "Incomplete")) %>% 
  # Arrange
  arrange(desc(prop)) %>% 
  mutate(column=factor(column, column))

# Plot data
g <- ggplot(p_nas_df, aes(y=column, x=prop, fill=complete_yn)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent complete", y="") +
  scale_x_continuous(labels=scales::percent) +
  scale_fill_discrete(name="") +
  # Themes
  theme_bw() + bar_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbook_completeness.png"), 
       width=4.5, height=4.5, units="in", dpi=600)



# Depth (ft)
################################################################################

# Data
stats <- data %>% 
  select(logbook_id, depth_min_ft, depth_max_ft) %>% 
  gather(key="type", value="depth_ft", 2:3) %>% 
  mutate(type=recode_factor(type, 
                            "depth_min_ft"="Minimum\ndepth",
                            "depth_max_ft"="Maximum\ndepth"))

# Plot
g <- ggplot(stats, aes(x=type, y=depth_ft)) +
  geom_boxplot(outlier.color=NA) +
  # Labels
  labs(x="", y="Depth (ft)",
       subtitle="Outliers hidden to comply with rule-of-three") +
  scale_y_continuous(trans="log2", breaks=c(1,2, 5,10,20,50,100,200,500)) +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbook_depth.png"), 
       width=3.5, height=3.5, units="in", dpi=600)


# Duration (hrs)
################################################################################

# Plot
g <- ggplot(data, aes(y=pmin(hours, 10))) +
  geom_boxplot(outlier.color=NA) +
  # Labels
  labs(x="", y="Dive time (hrs)",
       subtitle="Outliers hidden to comply with rule-of-three") +
  scale_y_continuous(breaks=seq(0,10,2), labels=c(seq(0,8,2), ">10")) +
  # Theme
  theme_bw() + bar_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank())
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbook_effort.png"), 
       width=3.5, height=3.5, units="in", dpi=600)


# Catch
################################################################################

# Plot 
g <- ggplot(data %>% filter(!is.na(comm_name)), aes(y=comm_name, x=catch_lbs)) +
  geom_boxplot(outlier.color=NA) +
  # Labels
  labs(x="Catch (lbs)", y="",
       subtitle="Outliers hidden to comply with rule-of-three") +
  scale_x_continuous(trans="log10",
                     breaks=c(1, 5, 10, 50, 100, 500, 1000, 5000 )) +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbook_catch.png"), 
       width=5.5, height=3, units="in", dpi=600)



# Block
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build stats
stats <- data %>% 
  group_by(comm_name, block_id) %>% 
  summarize(nfishers=unique(fisher_id),
            catch_lbs=sum(catch_lbs, na.rm=T)) %>% 
  ungroup() %>% 
  # RUler of three
  filter(nfishers>=3) %>% 
  # Remove unknown species
  filter(!is.na(comm_name)) %>% 
  # Spatialize
  filter(!is.na(block_id)) %>% 
  left_join(blocks_sf %>% select(block_id, geometry), by="block_id") %>% 
  sf::st_as_sf()

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~comm_name, ncol=4) +
  # Plot blocks
  geom_sf(data=stats, mapping=aes(fill=catch_lbs),
          color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="A few rarely visited blocks are hidden to comply with rule-of-three") +
  # Legend
  scale_fill_gradientn(name="Catch (lbs)", 
                       trans="log10",
                       breaks=10^c(0:6),
                       labels=parse(text=paste0("10^", 0:6)),
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  scale_x_continuous(breaks=seq(-124, -116, 4)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbooks_block_catch.png"), 
       width=6.5, height=5.5, units="in", dpi=600)


# Coordinates (generalized)
################################################################################

# Breaks and midpoints
lat_breaks <- seq(32, 42, 0.25)
lat_mids <- zoo::rollmean(lat_breaks, k=2)
long_breaks <- seq(-126,-114, 0.25)
long_mids <- zoo::rollmean(long_breaks, k=2)

# XY bins
xy_ras <- data %>% 
  # Reduce to logbooks with XY
  filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
  # Add bins for XY
  mutate(lat_dd_bin=cut(lat_dd, breaks=lat_breaks, labels=lat_mids) %>% as.character() %>%  as.numeric(),
         long_dd_bin=cut(long_dd, breaks=long_breaks, labels=long_mids) %>% as.character() %>% as.numeric()) %>% 
  # Summarize
  group_by(comm_name, lat_dd_bin, long_dd_bin) %>% 
  summarize(nfishers=n_distinct(fisher_id),
            nlogbooks=n_distinct(logbook_id)) %>% 
  ungroup() %>% 
  # Rule of three
  filter(nfishers>=3)


# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~comm_name, nrow=1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="Coordinates are summarized in bins to comply with the rule-of-three") +
  # Plot points
  geom_tile(data=xy_ras, mapping=aes(x=long_dd_bin, y=lat_dd_bin, fill=nlogbooks), alpha=0.5, color="black") +
  # Legend
  scale_fill_gradientn(name="Number of logbooks", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10",
                       breaks=c(1, 10, 100, 1000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbooks_coordinates_gen.png"), 
       width=6.5, height=3.25, units="in", dpi=600)


# Coordinates
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Plot points
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, color=comm_name), pch=1) +
  # Legend
  scale_color_discrete(name="Species") +
  # Crop
  coord_sf(xlim = range(data$long_dd, na.rm=T), 
           ylim = range(data$lat_dd, na.rm=T)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbooks_coordinates.png"), 
       width=5.5, height=5.5, units="in", dpi=600)


# Coordinate coverage
################################################################################

# % unknown coordinates
# % reliable coordinates
# % unreliable coordinates 


# Stats
stats <- data %>% 
  # Mark whether coordinates
  mutate(coord_yn=ifelse(!is.na(lat_dd) & !is.na(long_dd), "Coords", "No coords")) %>% 
  # Mark whether coordinates are reliabel
  mutate(reliable_yn=case_when(latlong_good_yn==T ~ "yes",
                               latlong_good_yn==F ~ "no",
                               T ~ "unknown")) %>% 
  # Create category
  mutate(catg=paste(coord_yn, reliable_yn, sep="-")) %>% 
  # Summarize
  group_by(year, catg) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Calculate prop
  group_by(year) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Format
  filter(catg!="No coords-unknown") %>% 
  mutate(catg=recode_factor(catg,
                     "Coords-unknown"="No block id reported",
                     "Coords-no"="No",
                     "Coords-yes"="Yes"))

# Plot
g <- ggplot(stats, aes(x=year, y=prop, fill=catg)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="% of logbooks\nwith GPS coordinates") +
  scale_y_continuous(labels=scales::percent, breaks=seq(0,1,0.1)) +
  scale_fill_manual(name="Reliable coordinates?", values=c("blue", "red", "grey80") %>% rev()) +
  # Theme
  theme_bw() + bar_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbooks_coordinates_over_time.png"), 
       width=4.5, height=3, units="in", dpi=600)



# Catch over time
################################################################################

# Stats
stats <- data %>% 
  group_by(year, comm_name) %>% 
  summarize(catch_lbs=sum(catch_lbs, na.rm=T)) %>% 
  ungroup()

# Plot data
g <- ggplot(stats, aes(x=year, y=catch_lbs/1e3, color=comm_name, group=comm_name)) +
  geom_line() +
  # Labels
  labs(y="Catch (thousands of lbs)", x="Year") +
  scale_color_discrete(name="Species") +
  # Theme
  theme_bw() + bar_theme + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbooks_catch_over_time.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



