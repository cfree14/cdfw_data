

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outdir <- "data/confidential/cucumber_trawl_logbooks/processed"
plotdir <- "figures/cucumber_trawl_logbooks"
keydir <- "data/public/cdfw_keys/processed"

# Read data
data <- readRDS(file=file.path(outdir, "1982_2020_cucumber_trawl_logbooks.Rds"))

# Get blocks
blocks_sf <- wcfish::blocks
blocks <- blocks_sf %>% 
  sf::st_drop_geometry()

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.subtitle = element_text(size=6, face="italic"),
                   plot.title=element_text(size=10),
                   plot.tag=element_text(size=9),
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
  # Remove uninteresting columns
  filter(!column %in% c("old_year", "year", "time_set_num", "time_up_num")) %>% 
  # Mark spatial/not-spatial
  mutate(type=ifelse(grepl("lat|long|loran", column), "Spatial", "Non-spatial")) %>% 
  # Format columns
  mutate(column=gsub("_", " ", column) %>% stringr::str_to_sentence()) %>%
  mutate(column=recode(column,
                       "Vessel"="Vessel name",
                       "Date tow"="Date of tow",
                       "Date landing"="Date of landing",
                       "Date depart"="Date of departure",
                       "Depth fa set"="Start depth (fa)",
                       "Depth fa up"="End depth (fa)",
                       "Duration min"="Duration (min)",
                       "Time set"="Start time",
                       "Time up"="End time",
                       "Headrope ft"="Head rope length (ft)",
                       "Catch lbs"="Catch (lbs)",
                       # Spatial
                       "Long dd set"="Start longitude (°W)",
                       "Lat dd set"="Start latitude (°N)",
                       "Long dd up"="End longitude (°W)",
                       "Lat dd up"="End latitude (°N)",
                       "Set loran cy"="Start LORAN-C Y-value",
                       "Set loran cx"="Start LORAN-C X-value",
                       "Set loran cw"="Start LORAN-C W-value",
                       "Up loran cy"="End LORAN-C Y-value",
                       "Up loran cx"="End LORAN-C X-value",
                       "Up loran cw"="End LORAN-C W-value",
                       "Set loran amin"="Start LORAN-A min",
                       "Set loran amax"="Start LORAN-A max",
                       "Up loran amin"="End LORAN-A min",
                       "Up loran amax"="End LORAN-A max",
                       )) %>%
  # Color
  mutate(complete_yn=prop==1)

# Plot data
g <- ggplot(p_nas_df, aes(x=prop, y=reorder(column, prop), fill=complete_yn)) +
  facet_grid(type~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="% complete", y="") +
  scale_x_continuous(labels=scales::percent) +
  scale_fill_discrete(name="100% complete?", guide=guide_legend(reverse=T)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "cucumber_logbook_completeness.png"), 
       width=5.5, height=5.5, units="in", dpi=600)


# Number of logbooks
################################################################################

# Stats
stats <- data %>% 
  group_by(year) %>% 
  summarize(nlogs=n_distinct(logbook_id)) %>% 
  ungroup()

# Plot
# 2008, 2011, 2012 no data
g <- ggplot(stats, aes(x=year, y=nlogs)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of logbooks") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "cucumber_prawn_logbook_nlogs.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


# Depth
################################################################################

# Depth, set
range(data$depth_fa_set, na.rm=T)
g1 <- ggplot(data, aes(x=depth_fa_set)) +
  geom_histogram(breaks=seq(0,910,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 300),
                     breaks=seq(0, 300, 25),
                     labels=c(breaks=seq(0, 275, 25), "≥300")) +
  # Labels
  labs(x="Start depth (fathoms)", y="# of logbook entries", tag="A") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Depth, haul
range(data$depth_fa_set, na.rm=T)
g2 <- ggplot(data, aes(x=depth_fa_up)) +
  geom_histogram(breaks=seq(0,910,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 300),
                     breaks=seq(0, 300, 25),
                     labels=c(breaks=seq(0, 275, 25), "≥300")) +
  # Labels
  labs(x="End depth (fathoms)", y="# of logbook entries", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "cucumber_logbook_depth_dists.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Duration
################################################################################

# Plot start/end times
tdata <- data %>% 
  select(logbook_id, time_set_num, time_up_num) %>% 
  gather(key="type", value="time", 2:ncol(.)) %>% 
  mutate(type=recode(type, 
                     "time_set_num"="Start",
                     "time_up_num"="End"))

# Plot times
g1 <- ggplot(tdata, aes(x=time, fill=type)) +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="Time of day", y="Density", tag="A") +
  scale_x_continuous(breaks=seq(0,24,4)) +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.8, 0.8),
        legend.key.size=unit(0.3, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot duration
range(data$duration_min/60, na.rm=T)
g2 <- ggplot(data, aes(x=duration_min/60)) +
  geom_histogram(breaks=seq(0,20,0.25)) +
  # Labels
  labs(y="# of logbook entries", x="Duration (hours)", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

ggsave(g, filename=file.path(plotdir, "cucumber_logbook_duration_dists.png"), 
       width=6.5, height=3, units="in", dpi=600)

# Head rope length
################################################################################

# Plot head rope
range(data$headrope_ft, na.rm=T)
g <- ggplot(data, aes(x=headrope_ft)) +
  geom_histogram(breaks=seq(0,775,5)) +
  # Reference line
  geom_vline(xintercept=c(30, 100), linetype="dotted", linewidth=0.2, color="grey60") +
  # Labels
  labs(y="# of logbook entries", x="Head rope length (ft)") +
  scale_x_continuous(breaks=seq(0,800,50)) +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

ggsave(g, filename=file.path(plotdir, "cucumber_logbook_headrope_dists.png"), 
       width=3.5, height=3, units="in", dpi=600)


# Catch
################################################################################

# Plot catch
range(data$catch_lbs, na.rm=T)
g <- ggplot(data, aes(x=catch_lbs)) +
  geom_histogram(breaks=seq(0,2000,10)) +
  # Labels
  labs(y="# of logbook entries", x="Catch (lbs)") +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.75,0.8),
        legend.key.size = unit(0.3, "cm"))
g

ggsave(g, filename=file.path(plotdir, "cucumber_logbook_catch_dists.png"), 
       width=3.5, height=3, units="in", dpi=600)

# Catch
################################################################################

# Catch stats
stats <- data %>% 
  filter(!is.na(species) & !is.na(catch_lbs)) %>% 
  group_by(species) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            catch_lbs=median(catch_lbs)) %>% 
  ungroup() %>% 
  filter(nvessels>=3) %>% 
  arrange(desc(catch_lbs))

# Prep catch data
cdata <- data %>% 
  filter(species %in% stats$species) %>% 
  mutate(species=factor(species, levels=stats$species))

# Plot catch
range(data$catch_lbs, na.rm=T)
g <- ggplot(cdata, aes(x=catch_lbs, y=species)) +
  geom_boxplot(outlier.shape = NA) +
  # Labels
  labs(x="Catch (lbs)", y="", subtitle = "Outliers and a few rare species are not shown to comply with the rule-of-three") +
  scale_x_continuous(trans="log10") +
  # Scale
  theme_bw() + my_theme 
g


ggsave(g, filename=file.path(plotdir, "cucumber_logbook_catch_dists.png"), 
       width=5.5, height=5, units="in", dpi=600)



# Plot locations
################################################################################

# Get midpoint
get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}

# Breaks and midpoints
lat_breaks <- seq(32, 42, 0.25)
lat_mids <- zoo::rollmean(lat_breaks, k=2)
long_breaks <- seq(-126,-114, 0.25)
long_mids <- zoo::rollmean(long_breaks, k=2)

# XY bins
xy_ras_set <- data %>% 
  # Reduce to logboooks with XY
  filter(!is.na(lat_dd_set) & !is.na(long_dd_set)) %>% 
  # Add bins for XY
  mutate(lat_dd_bin=cut(lat_dd_set, breaks=lat_breaks, labels=lat_mids) %>% as.character() %>%  as.numeric(),
         long_dd_bin=cut(long_dd_set, breaks=long_breaks, labels=long_mids) %>% as.character() %>% as.numeric()) %>% 
  # Summarize
  group_by(lat_dd_bin, long_dd_bin) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nlogbooks=n_distinct(logbook_id)) %>% 
  ungroup() %>% 
  # Rule of three
  filter(nvessels>=3)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot
g1 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="Coordinates are summarized in bins to comply with the rule-of-three") +
  # Plot points
  geom_tile(data=xy_ras_set, mapping=aes(x=long_dd_bin, y=lat_dd_bin, fill=nlogbooks), alpha=0.9) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
  # Legend
  scale_fill_gradientn(name="Number of logbooks", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10",
                       breaks=c(1, 5, 10, 25, 50, 100, 250, 500, 1000,  5000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.25, 0.2),
        legend.key.size = unit(0.4, "cm"))
g1

# Export
ggsave(g, filename=file.path(plotdir, "cucumber_logbook_map.png"),
       width=3.5, height=4.75, units="in", dpi=600)

# GPS data coverage
################################################################################

# GPS stats
gps_stats <- data %>% 
  # Summarize
  group_by(year) %>% 
  summarise(n=n(),
            n_with_gps=sum( ( !is.na(lat_dd_set) & !is.na(long_dd_set) )  |  ( !is.na(lat_dd_up) & !is.na(long_dd_up) ) ) )%>% 
  ungroup() %>% 
  # Calc prop
  mutate(prop=n_with_gps/n)

# Plot data
g <- ggplot(gps_stats, aes(x=year, y=prop)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Percent of logbook entries\nwith GPS coordinates") +
  scale_x_continuous(breaks=seq(1980, 2025,5)) +
  scale_y_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "cucumber_logbook_gps_coverage.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


# Block
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build stats
stats <- data %>% 
  # Species of interest
  filter(grepl("cucumber", species)) %>% 
  # Summarize
  group_by(species, block_id) %>% 
  summarize(nfishers=unique(vessel_id),
            catch_lbs=sum(catch_lbs, na.rm=T)) %>% 
  ungroup() %>% 
  # RUler of three
  filter(nfishers>=3) %>% 
  # Remove unknown species
  filter(!is.na(species)) %>% 
  # Spatialize
  filter(!is.na(block_id)) %>% 
  left_join(blocks_sf %>% select(block_id, geometry), by="block_id") %>% 
  sf::st_as_sf()

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~species, ncol=4) +
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
ggsave(g, filename=file.path(plotdir, "cucumber_logbooks_block_catch.png"), 
       width=6.5, height=4, units="in", dpi=600)


