

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/lobster_logbooks/raw"
outdir <- "data/confidential/lobster_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/lobster_logbooks"

# Read data
blocks_sf <- wcfish::blocks
data <- readRDS(file=file.path(outdir, "CDFW_2000_2020_logbook_data.Rds"))


# Themes
################################################################################

box_theme <-  theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    plot.tag=element_text(size=8),
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
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Effort
################################################################################

# Plot nights on water
g1 <- ggplot(data, aes(y=n_nights)) +
  geom_boxplot() +
  # Scale
  scale_y_continuous(trans="log2", breaks=c(1,2,5,10, 20, 50, 100)) +
  # Labels
  labs(y="Number of nights", tag="A") +
  # Thene
  theme_bw() + box_theme
g1


# Plot of traps pulled
g2 <- ggplot(data, aes(y=n_traps_pulled)) +
  geom_boxplot() +
  # Labels
  labs(y="Number of traps pulled", tag="B") +
  # Thene
  theme_bw() + box_theme
g2

# Plot of traps current deployed
g3 <- ggplot(data, aes(y=n_traps_set)) +
  geom_boxplot() +
  # Labels
  labs(y="Number of traps currently deployed", tag="C") +
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
  geom_boxplot() +
  # Scale
  scale_y_continuous(trans="log2", breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)) +
  # Labels
  labs(y="Number of lobsters retained", tag="A") +
  # Thene
  theme_bw() + box_theme
g1

# Plot of traps pulled
g2 <- ggplot(data, aes(y=n_released)) +
  geom_boxplot() +
  # Scale
  scale_y_continuous(trans="log2", breaks=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  # Labels
  labs(y="Number of lobsters released", tag="B") +
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
  geom_boxplot() +
  # Reference line
  geom_hline(yintercept = 300, linetype="dotted", color="grey30") +
  annotate(geom="text", x=-0.3, y=300, label="300 ft", color="grey30", vjust=-0.5, size=2.4) +
  # Labels
  labs(y="Depth (ft)") +
  scale_y_continuous(trans="log2", breaks=c(1, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
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
  summarize(nlogs=n_distinct(logbook_id)) %>% 
  ungroup() %>% 
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
  geom_hline(yintercept=34.5) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32, 42)) +
  # Legend
  scale_fill_gradientn(name="Number of logbooks", 
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10",
                       breaks=c(1, 5, 10, 50, 100, 500, 1000,  5000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.4, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "lobster_logbook_blocks.png"), 
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







