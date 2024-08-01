

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/lobster_logbooks_2024/raw"
outdir <- "data/confidential/lobster_logbooks_2024/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/lobster_logbooks_2024"

# Read data
blocks_sf <- wcfish::blocks
data <- readRDS(file=file.path(outdir, "CDFW_1980_2022_lobster_logbook_data.Rds"))


# Themes
################################################################################

box_theme <-  theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    plot.tag=element_text(size=8),
                    plot.subtitle = element_text(size=6, face="italic"),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


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


# Completeness
################################################################################

# Percent NAs
n_nas <- freeR::complete(data)
p_nas <- 1-n_nas/nrow(data)

# Build data
p_nas_df <- tibble(column=colnames(data),
                   prop=p_nas) %>% 
  # Format columns
  mutate(column=gsub("_", " ", column) %>% stringr::str_to_sentence()) %>%
  mutate(column=recode(column,
                       "Vessel"="Vessel name",
                       "Fisher last"="Fisher last name",
                       "Fisher first"="Fisher first name",
                       "Depth ft"="Depth (ft)",
                       "Multi day yn"="Multi-day trip? (y/n)",
                       "N traps pulled"="Number of traps pulled",
                       "N nights"="Number of nights",
                       "N kept"="Number of lobsters kept",
                       "N released"="Number of shorts released", 
                       "N traps set"="Number of traps set",
                       "Ncrew"="Number of crew",
                       "Long dd"="Longitude (°W)",
                       "Lat dd"="Latitude (°N)")) %>% 
  # Remove uninteresting columns
  filter(!column %in% c("Location type", "Block state", "Block type", "Location orig", "Block id gps", "Gps reliable yn")) %>% 
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
  theme_bw() + bar_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_completeness.png"), 
       width=5.5, height=5.5, units="in", dpi=600)


# Number of log entries
################################################################################

# Stats
stats <- data %>% 
  group_by(year) %>% 
  summarize(nlogs=n()) %>% 
  ungroup()

# Plot data
g <- ggplot(stats, aes(x=year, y=nlogs/1000)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Thousands of logbook entries") +
  scale_x_continuous(breaks=seq(1980,2025, 5)) +
  # Theme
  theme_bw() + bar_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_nlogs.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


# Effort
################################################################################

# Plot nights on water
range(data$n_nights, na.rm=T)
g1 <- ggplot(data, aes(y=n_nights)) +
  geom_boxplot(outlier.color = NA) +
  # Scale
  scale_y_continuous(trans="log10", breaks=c(1,2,5,10, 20, 50, 100, 200)) +
  # Labels
  labs(y="Number of nights", tag="A",
       subtitle="Outliers hidden to comply with rule-of-three") +
  # Thene
  theme_bw() + box_theme
g1


# Plot of traps pulled
g2 <- ggplot(data, aes(y=n_traps_pulled)) +
  geom_boxplot(outlier.color = NA) +
  # Scale
  scale_y_continuous(trans="log10", breaks=c(1, 5, 10, 50, 100, 500, 1000, 5000)) +
  # Labels
  labs(y="Number of traps pulled", tag="B",
       subtitle="Outliers hidden to comply with rule-of-three") +
  # Theme
  theme_bw() + box_theme
g2

# Plot of traps current deployed
g3 <- ggplot(data, aes(y=n_traps_set)) +
  geom_boxplot(outlier.color = NA) +
  # Labels
  labs(y="Number of traps currently deployed", tag="C",
       subtitle="Outliers hidden to comply with rule-of-three") +
  # Thene
  theme_bw() + box_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_effort.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


# Catch
################################################################################

# Plot kept
g1 <- ggplot(data, aes(y=n_kept)) +
  geom_boxplot(outlier.color = NA) +
  # Scale
  scale_y_continuous(trans="log10", breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000)) +
  # Labels
  labs(y="Number of lobsters retained", tag="A",
       subtitle="Outliers hidden to comply with rule-of-three") +
  # Thene
  theme_bw() + box_theme
g1

# Plot of traps pulled
g2 <- ggplot(data, aes(y=n_released)) +
  geom_boxplot(outlier.color = NA) +
  # Scale
  scale_y_continuous(trans="log2", breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  # Labels
  labs(y="Number of lobsters released", tag="B",
       subtitle="Outliers hidden to comply with rule-of-three") +
  # Thene
  theme_bw() + box_theme
g2


# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_catch.png"), 
       width=4.5, height=2.5, units="in", dpi=600)


# Catch over time
################################################################################

# Summarize over time
stats <- data %>%
  group_by(year) %>%
  summarize(n_kept=sum(n_kept, na.rm=T),
            n_released=sum(n_released, na.rm=T)) %>%
  ungroup() %>%
  gather(key="type", value="catch_n", 2:ncol(.)) %>%
  mutate(type=recode_factor(type,
                            "n_released"="Shorts released",
                            "n_kept"="Legals kept"))

# Plot
g <- ggplot(stats, aes(x=year, y=catch_n/1e6, fill=type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Millions of lobsters caught") +
  # Legend
  scale_fill_discrete(name="", guide=guide_legend(reverse=T)) +
  scale_y_continuous(breaks=seq(0,2.5, 0.5)) +
  # Theme
  theme_bw() + bar_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_catch_over_time.png"), 
       width=4.5, height=3.5, units="in", dpi=600)


# Depth
################################################################################

# Depth
g <- ggplot(data, aes(y=depth_ft)) +
  geom_boxplot(outlier.color = NA) +
  # Reference line
  geom_hline(yintercept = 300, linetype="dotted", color="grey30") +
  annotate(geom="text", x=-0.3, y=300, label="300 ft", color="grey30", vjust=-0.5, size=2.4) +
  # Labels
  labs(y="Depth (ft)",
       subtitle="Outliers hidden to comply with rule-of-three") +
  scale_y_continuous(trans="log10", breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  # Thene
  theme_bw() + box_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_depth.png"), 
       width=3.5, height=3.5, units="in", dpi=600)


# Location type
################################################################################

# Stats
stats <- data %>% 
  count(year, location_type) %>% 
  group_by(year) %>% 
  mutate(prop=n/sum(n))

# Plot
g <- ggplot(stats, aes(x=year, y=prop, fill=location_type)) +
  geom_bar(stat="identity", color="grey30", lwd=0.1) +
  # Labels
  labs(x="Year", y='Percent of logbooks') +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_discrete(name="Location type", guide=guide_legend(reverse=T)) +
  # Theme
  theme_bw() + bar_theme +
  theme(legend.position = "top")
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_location_type.png"), 
       width=4, height=3.5, units="in", dpi=600)


# Block map
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build stats
stats <- data %>% 
  # Number of logs by block 
  group_by(block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nlogs=n_distinct(logbook_id)) %>% 
  ungroup() %>% 
  # Remove rule-of-three
  filter(nvessels>=3) %>% 
  # Reduce to valid blocks
  filter(block_id %in% blocks_sf$block_id) %>% 
  # Spatialize
  left_join(blocks_sf %>% select(block_id)) %>% 
  sf::st_as_sf()

# Plot
g <- ggplot() +
  # Plot blocks
  geom_sf(data=stats, mapping=aes(fill=nlogs), lwd=0.1, color="grey30") +
  # Plot reference line
  geom_hline(yintercept=34.5, linetype="dotted") + # most fishing is south Point Conception
  geom_hline(yintercept=36.492225) + # fishing is not allowed north of Yankee Point
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="A few extremely rarely visited blocks are hidden to comply with the rule-of-three") +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
  # Legend
  scale_fill_gradientn(name="Number of logbooks", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10",
                       breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth=0.2)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.4, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_blocks.png"), 
       width=3.5, height=4.75, units="in", dpi=600)


# Locations (generalized)
################################################################################

get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}

# Breaks and midpoints
lat_breaks <- seq(32, 42, 0.25)
lat_mids <- zoo::rollmean(lat_breaks, k=2)
long_breaks <- seq(-126,-114, 0.25)
long_mids <- zoo::rollmean(long_breaks, k=2)

# XY bins
xy_ras <- data %>% 
  # Reduce to logboooks with XY
  filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
  # Add bins for XY
  mutate(lat_dd_bin=cut(lat_dd, breaks=lat_breaks, labels=lat_mids) %>% as.character() %>%  as.numeric(),
         long_dd_bin=cut(long_dd, breaks=long_breaks, labels=long_mids) %>% as.character() %>% as.numeric()) %>% 
  # Summarize
  group_by(lat_dd_bin, long_dd_bin) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nlogbooks=n_distinct(logbook_id)) %>% 
  ungroup() %>% 
  # Rule of three
  filter(nvessels>=3)

# Plot
g <- ggplot() +
  # Plot reference line
  geom_hline(yintercept=34.5) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="Coordinates are summarized in bins to comply with the rule-of-three") +
  # Plot points
  geom_tile(data=xy_ras, mapping=aes(x=long_dd_bin, y=lat_dd_bin, fill=nlogbooks), alpha=0.5) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
  # Legend
  scale_fill_gradientn(name="Number of logbooks", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10",
                       breaks=c(1, 5, 10, 50, 100, 500, 1000,  5000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth=0.2)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.4, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_coordinates_gen.png"), 
       width=3.5, height=4.75, units="in", dpi=600)


# Locations
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot
g <- ggplot() +
  # Plot blocks
  # geom_sf(data=stats, mapping=aes(fill=nlogs), lwd=0.1, color="grey30") +
  # Plot reference line
  geom_hline(yintercept=34.5) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="A few extremely rarely visited blocks are hidden to comply with the rule-of-three") +
  # Plot points
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd), pch=1, alpha=0.5, size=0.7, color="grey30") +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.4, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_coordinates.png"), 
       width=3.5, height=4.75, units="in", dpi=600)







