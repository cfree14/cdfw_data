

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outdir <- "data/confidential/halibut_trawl_logbooks_2024/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/halibut_trawl_logbooks_2024"

# Read data
data <- readRDS(file=file.path(outdir, "CDFW_1981_2022_halibut_trawl_data.Rds"))
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
  # Format columns
  mutate(column=gsub("_", " ", column) %>% stringr::str_to_sentence()) %>%
  mutate(column=recode(column,
                       "Vessel"="Vessel name",
                       # Date
                       "Date set"="Date of tow",
                       "Date return"="Date of return",
                       "Date depart"="Date of departure",
                       # Port
                       "Port depart"="Departure port",
                       "Port code depart"="Departure port code",
                       "Port return"="Return port",
                       "Port code return"="Return port code",
                       # Depth
                       "Depth fa avg"="Average depth (fa)",
                       # Time
                       "Time return"="Return time (HH:MM)",
                       "Time depart"="Depart time (HH:MM)",
                       "Time set"="Tow start time (HH:MM)",
                       "Time up"="Tow end time (HH:MM)",
                       "Duration hrs"="Duration (hrs)",
                       # Species
                       "Spp code"="Species id",
                       "Target spp"="Target species",
                       "Target spp code"="Target species code",
                       # Catch
                       "Revenue usd"="Revenues (USD)",
                       "Catch lbs est"="Estimated catch (lbs)",
                       "Catch lbs receipt"="Catch on receipt (lbs)",
                       # Trip info
                       "Em trip yn"="EM trip (Y/N)?",
                       "Efp trip yn"="EFP trip (Y/N)?",
                       "Observed trip yn"="Observed trip (Y/N)?",
                       "Void yn"="Void trip (Y/N)?",
                       "Signed yn"="Signed trip (Y/N)?",
                       # Spatial
                       "Long dd set"="Start longitude (°W)",
                       "Lat dd set"="Start latitude (°N)",
                       "Long dd up"="End longitude (°W)",
                       "Lat dd up"="End latitude (°N)",
 

  )) %>%
  # Color
  mutate(complete_yn=prop==1)

# Plot data
g <- ggplot(p_nas_df, aes(x=prop, y=reorder(column, desc(prop)), fill=complete_yn)) +
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
ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_completeness.png"), 
       width=5.5, height=6, units="in", dpi=600)


# Number of logbooks through time
################################################################################

# Stats
stats <- data %>% 
  mutate(year=lubridate::year(date_return)) %>% 
  group_by(year) %>% 
  summarize(nlogs=n_distinct(logbook_id)) %>% 
  ungroup()

# Plot logs through time
g <- ggplot(stats, aes(x=year, y=nlogs/1000)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Thousands of logbooks") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_nlogs.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


# Catch: logs vs. receipts
################################################################################

# Summarize landings
landings <- landings_orig %>% 
  # Trawl fishery for halibut
  filter(comm_name %in% c("California halibut")) %>% 
  filter(grepl("trawl", tolower(gear))) %>% 
  # Summarize
  group_by(year, comm_name) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)) %>% 
  ungroup()

# Summarize logged landings
logged_lbs <- data %>% 
  filter(species %in% c("California halibut")) %>% 
  mutate(year=lubridate::year(date_set)) %>% 
  group_by(year) %>% 
  summarize(landings_lbs=sum(catch_lbs_est, na.rm=T)) %>% 
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
ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_vs_reciepts_catch.png"), 
       width=6.5, height=3, units="in", dpi=600)

# Depth
################################################################################

# Depth
range(data$depth_fa_avg, na.rm=T)
g <- ggplot(data, aes(x=depth_fa_avg)) +
  geom_histogram(breaks=seq(0,802,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 300),
                     breaks=seq(0, 300, 25),
                     labels=c(breaks=seq(0, 275, 25), "≥300")) +
  # Labels
  labs(x="Start depth (fathoms)", y="# of logbook entries") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_depth_dists.png"), 
       width=3.5, height=3, units="in", dpi=600)



# Target species
################################################################################

# 
stats <- data %>% 
  group_by(target_spp) %>% 
  summarize(nlogs=n_distinct(logbook_id),
            nvessels=n_distinct(vessel_id)) %>% 
  ungroup() %>% 
  filter(!is.na(target_spp) & nvessels>=3)

# Plot data
g <- ggplot(stats, aes(y=reorder(target_spp, desc(nlogs)), x=nlogs)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="# of logbook entries", y="") +
  scale_x_continuous(trans="log10") +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_target_spp.png"), 
       width=5.5, height=4.5, units="in", dpi=600)
  

# Crew size
################################################################################

# Depth
range(data$crew_size, na.rm=T)
g <- ggplot(data, aes(x=crew_size)) +
  geom_histogram(breaks=seq(0,84,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 16),
                     breaks=seq(0, 16, 1),
                     labels=c(breaks=seq(0, 15, 1), "≥16")) +
  # Labels
  labs(x="Crew size", y="# of logbook entries") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_crew_dists.png"), 
       width=3.5, height=3, units="in", dpi=600)


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
range(data$duration_hrs, na.rm=T)
g2 <- ggplot(data, aes(x=duration_hrs)) +
  geom_histogram(breaks=seq(0,24,0.25)) +
  # Labels
  labs(y="# of logbook entries", x="Duration (hours)", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_duration_dists.png"),
       width=6.5, height=3, units="in", dpi=600)


# Catch
################################################################################

# Catch stats
stats <- data %>% 
  filter(!is.na(species) & !is.na(catch_lbs_receipt)) %>% 
  group_by(species) %>% 
  summarize(nvessels=n_distinct(vessel_id),
           catch_lbs=median(catch_lbs_receipt, na.rm=T)) %>% 
  ungroup() %>% 
  filter(nvessels>=3) %>% 
  arrange(desc(catch_lbs))

# Prep catch data
cdata <- data %>% 
  filter(species %in% stats$species) %>% 
  mutate(species=factor(species, levels=stats$species))

# Plot catch
range(data$catch_lbs_receipt, na.rm=T)
g <- ggplot(cdata, aes(x=catch_lbs_receipt, y=species)) +
  geom_boxplot(outlier.shape = NA, linewidth=0.2, fill="grey90") +
  # Labels
  labs(x="Catch on receipt (lbs)", y="", subtitle = "Outliers and a few rare species are not shown to comply with the rule-of-three") +
  scale_x_continuous(trans="log10",
                     breaks=c(1,10,100,1000, 10000, 100000),
                     labels=c("1","10","100","1,000", "10,000", "100,000")) +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y=element_text(size=4))
g


ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_catch_dists.png"), 
       width=5.5, height=10, units="in", dpi=600)


# Block
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build stats
stats <- data %>% 
  # Add data
  mutate(year=lubridate::year(date_return)) %>% 
  # Summarize
  group_by(year, block_id) %>% 
  summarize(nlogs=n_distinct(logbook_id),
            nvessels=n_distinct(vessel_id)) %>% 
  ungroup() %>% 
  # Rule of three
  filter(nvessels>=3) %>% 
  # Spatialize
  filter(!is.na(block_id)) %>% 
  left_join(blocks_sf %>% select(block_id, geometry), by="block_id") %>% 
  sf::st_as_sf()

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~year, ncol=10) +
  # Plot blocks
  geom_sf(data=stats, mapping=aes(fill=nlogs),
          color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="A few rarely visited blocks are hidden to comply with rule-of-three") +
  # Legend
  scale_fill_gradientn(name="Number of logs", 
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
        legend.key.size = unit(0.5, "cm"),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.text = element_text(size=6))
g

# Export
ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbooks_block_catch.png"), 
       width=6.5, height=5.25, units="in", dpi=600)



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
ggsave(g1, filename=file.path(plotdir, "halibut_trawl_logbook_map.png"),
       width=3.5, height=4.75, units="in", dpi=600)

