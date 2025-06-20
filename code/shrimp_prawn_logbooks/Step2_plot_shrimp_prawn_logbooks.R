

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/shrimp_prawn_logbooks/raw"
outdir <- "data/confidential/shrimp_prawn_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/shrimp_prawn_logbooks"

# Read data
data <- readRDS(file=file.path(outdir, "CDFW_1982_2023_shrimp_prawn_logbook_data.Rds"))
blocks_sf <- wcfish::blocks

# Read landings data
landings_orig <- readRDS("data/confidential/landing_receipts_2023/processed/1980_2022_landings_receipts.Rds")



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


# Plot completeness
################################################################################

# Percent NAs
n_nas <- freeR::complete(data)
p_nas <- 1-n_nas/nrow(data)

# Build data
p_nas_df <- tibble(column=colnames(data),
                   prop=p_nas) %>% 
  # Remove uninteresting columns
  filter(!column %in% c("year_orig", "year", "time_set_num", "time_up_num")) %>% 
  # Mark spatial/not-spatial
  mutate(type=ifelse(grepl("lat|long|loran", column), "Spatial", "Non-spatial")) %>% 
  # Format columns
  mutate(column=gsub("_", " ", column) %>% stringr::str_to_sentence()) %>%
  mutate(column=recode(column,
                       "Vessel"="Vessel name",
                       "Spp code"="Species id",
                       "Port code"="Port id",
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
                       "Comments1"="Comments 1",
                       "Comments2"="Comments 2",
                       # Spatial
                       "Long dd set"="Start longitude (°W)",
                       "Lat dd set"="Start latitude (°N)",
                       "Long dd up"="End longitude (°W)",
                       "Lat dd up"="End latitude (°N)",
                       "Set loran y"="Start LORAN-C Y-value",
                       "Set loran x"="Start LORAN-C X-value",
                       "Set loran w"="Start LORAN-C W-value",
                       # "Up loran y"="End LORAN-C Y-value",
                       "Up loran x"="End LORAN-C X-value",
                       "Up loran w"="End LORAN-C W-value",
                       # "Up loran cy"="End LORAN-C Y-value",
                       "Up loran cx"="End LORAN-C X-value",
                       "Up loran cw"="End LORAN-C W-value",
                       # "Set loran amin"="Start LORAN-A min",
                       # "Set loran amax"="Start LORAN-A max",
                       # "Up loran amin"="End LORAN-A min",
                       # "Up loran amax"="End LORAN-A max",
  )) %>%
  # Color
  mutate(complete_yn=prop==1)

# Plot data
g <- ggplot(p_nas_df, aes(x=prop, y=reorder(column, desc(prop)), fill=complete_yn)) +
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
ggsave(g, filename=file.path(plotdir, "shrimp_prawn_logbook_completeness.png"), 
       width=5.5, height=5.5, units="in", dpi=600)


# Number of logbooks through time
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
ggsave(g, filename=file.path(plotdir, "shrimp_prawn_logbook_nlogs.png"), 
       width=5.5, height=3.5, units="in", dpi=600)

# Completeness through time
################################################################################

# Build stats
stats <- data %>% 
  mutate(row_id=1:n()) %>% 
  select(year, row_id, everything()) %>% 
  gather(key="variable", value="value", 3:ncol(.)) %>% 
  # Summarize availability
  group_by(year, variable) %>% 
  summarize(n=n(),
            ndata=sum(!is.na(value))) %>% 
  ungroup() %>% 
  # Calculate prop
  mutate(prop=ndata/n)

# Format stats
stats1 <- stats %>% 
  # Format variable name
  mutate(variable=gsub("_", " ", variable) %>% stringr::str_to_sentence(),
         variable=recode(variable,
                         "Vessel"="Vessel name",
                         "Spp code"="Species id",
                         "Port code"="Port id",
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
                         "Comments1"="Comments 1",
                         "Comments2"="Comments 2",
                         # Spatial
                         "Long dd set"="Start longitude (°W)",
                         "Lat dd set"="Start latitude (°N)",
                         "Long dd up"="End longitude (°W)",
                         "Lat dd up"="End latitude (°N)",
                         "Set loran y"="Start LORAN-C Y-value",
                         "Set loran x"="Start LORAN-C X-value",
                         "Set loran w"="Start LORAN-C W-value",
                         # "Up loran y"="End LORAN-C Y-value",
                         "Up loran x"="End LORAN-C X-value",
                         "Up loran w"="End LORAN-C W-value",
                         # "Up loran cy"="End LORAN-C Y-value",
                         "Up loran cx"="End LORAN-C X-value",
                         "Up loran cw"="End LORAN-C W-value")) %>% 
  # Remove
  filter(!variable %in% c("Old year", "Time set num", "Time up num"))

# Plot data
g <- ggplot(stats1, aes(x=year, y=prop)) +
  facet_wrap(~variable, ncol=6) +
  geom_vline(xintercept=2002, linetype="dotted", color="grey60") +
  geom_line() +
  # Labels
  labs(x="Year", y="Percent complete") +
  scale_y_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + my_theme +
  theme(strip.text=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))
g

# Export
ggsave(g, filename=file.path(plotdir, "shrimp_prawn_trawl_logbook_completeness_over_time.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# Catch: logbooks vs. receipts
################################################################################

# Summarize landings
landings <- landings_orig %>% 
  # Trawl fishery for shrimps/prawns
  filter(comm_name %in% c("Pacific pink shrimp", "Ridgeback prawn", "Spot prawn")) %>% 
  filter(grepl("trawl", tolower(gear))) %>% 
  # Summarize
  group_by(year, comm_name) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)) %>% 
  ungroup()

# Summarize logged landings
logged_lbs <- data %>% 
  filter(species %in% c("Pacific pink shrimp", "Ridgeback prawn", "Spot prawn")) %>% 
  group_by(year) %>% 
  summarize(landings_lbs=sum(catch_lbs, na.rm=T)) %>% 
  ungroup()

# Plot data
g <- ggplot(landings, aes(x=year, y=landings_lbs/1000)) +
  facet_wrap(~comm_name, scale="free_y") +
  # Plot landings
  geom_bar(stat="identity", fill="grey80") +
  # Plot logbooks
  geom_line(data=logged_lbs) +
  geom_point(data=logged_lbs, size=0.8) +
  # Labels
  labs(x="Year", y="Landings (1000s lbs)") +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbook_vs_reciepts_catch.png"), 
       width=6.5, height=3, units="in", dpi=600)

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
ggsave(g, filename=file.path(plotdir, "shrimp_prawn_logbook_depth_dists.png"), 
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

ggsave(g, filename=file.path(plotdir, "shrimp_prawn_logbook_duration_dists.png"),
       width=6.5, height=3, units="in", dpi=600)


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


ggsave(g, filename=file.path(plotdir, "shrimp_prawn_logbook_catch_dists.png"), 
       width=5.5, height=5, units="in", dpi=600)

# Block
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build stats
stats <- data %>% 
  # Species of interest
  filter(species %in% c("Ridgeback prawn", "Spot prawn", "Pacific pink shrimp")) %>% 
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
  facet_wrap(~species, nrow=1) +
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
ggsave(g, filename=file.path(plotdir, "shrimp_prawn_logbooks_block_catch.png"), 
       width=6.5, height=4, units="in", dpi=600)


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
ggsave(g, filename=file.path(plotdir, "shrimp_prawn_logbook_gps_coverage.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


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
ggsave(g1, filename=file.path(plotdir, "shrimp_prawn_logbook_map.png"),
       width=3.5, height=4.75, units="in", dpi=600)

