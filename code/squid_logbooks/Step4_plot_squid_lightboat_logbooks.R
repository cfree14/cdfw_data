

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outdir <- "data/confidential/squid_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/squid_logbooks"

# Read data
data <- readRDS(file=file.path(outdir, "CDFW_2000_2022_squid_lightboat_logbook_data.Rds"))

# Get blocks
blocks <- wcfish::blocks

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
  # filter(!grepl("used|target|method", column)) %>% 
  # filter(!column %in% c("logbook_id_use", "year", "month", "day", "filename", "comm_name_orig", "geometry")) %>% 
  # Format columns
  mutate(column=gsub("_", " ", column) %>% stringr::str_to_sentence()) %>%
  mutate(column=recode(column,
                       "Bycatch lbs"="Bycatch (species, lbs)",
                       "Bait lbs"="Catch as live bait (lbs)",
                       "Sold t"="Catch sold (short tons)",
                       "Remaining t"="Biomass remaining (short tons)",
                       "Duration min"="Duration (min)",
                       "Long dd"="Longitude (°W)",
                       "Lat dd"="Latitude (°N)",
                       "Catch t"="Catch (tons)",
                       "Vessel"= "Vessel name",
                       "Mammals yn"="Mammals present? (y/n)",
                       "Birds yn"="Birds present? (y/n)",
                       "Limited yn"="Limited by order? (yes/no)",
                       "Depth fa"="Depth (fa)",
                       "Captain"="Captain name",
                       "Gps position"="GPS position",
                       "Sst f"="Temperature (SST, °F)")) %>%
  # Order columns
  arrange(desc(prop)) %>% 
  mutate(column=factor(column, levels=c(column))) %>% 
  # Color
  mutate(complete_yn=prop==1)

# Plot data
g <- ggplot(p_nas_df, aes(x=prop, y=column, fill=complete_yn)) +
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
ggsave(g, filename=file.path(plotdir, "squid_lightboat_logbook_completeness.png"), 
       width=5.5, height=4.5, units="in", dpi=600)


# Depth
################################################################################

# Depth
range(data$depth_fa, na.rm=T)
g1 <- ggplot(data, aes(x=depth_fa)) +
  geom_histogram(breaks=seq(0,47000,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 200), 
                     breaks=seq(0,200, 25),
                     labels=c(breaks=seq(0, 175, 25), "≥200")) +
  # Labels
  labs(x="Depth (fathoms)", y="# of logbook entries", tag="A") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g1


# Export
ggsave(g1, filename=file.path(plotdir, "squid_lightboat_logbook_depth_dist.png"),
       width=3.5, height=3, units="in", dpi=600)


# Fishing effort
################################################################################

# Plot searching
range(data$hours_searching, na.rm=T)
g1 <- ggplot(data, aes(x=hours_searching)) +
  geom_histogram(breaks=seq(0,42613,1)) +
  # Labels
  labs(y="# of logbook entries", x="Hours spent searching", tag="A") +
  scale_x_continuous(lim=c(0,40),
                     breaks=seq(0,40,5),
                     labels=c(seq(0,35,5), "≥40")) +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot lighting
range(data$hours_lighting, na.rm=T)
g2 <- ggplot(data, aes(x=hours_lighting)) +
  geom_histogram(breaks=seq(0,124,1)) +
  # Labels
  labs(y="# of logbook entries", x="Hours spent lighting", tag="B") +
  scale_x_continuous(lim=c(0,40),
                     breaks=seq(0,40,5),
                     labels=c(seq(0,35,5), "≥40")) +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot duration
range(data$duration_min, na.rm=T)
g3 <- ggplot(data, aes(x=duration_min/60)) +
  geom_histogram(breaks=seq(0,1439,0.5)) +
  # Labels
  labs(y="# of logbook entries", x="Hours spent brailling", tag="C") +
  scale_x_continuous(lim=c(0,24),
                     breaks=seq(0,24,4),
                     labels=c(seq(0,20,4), "≥24")) +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "squid_lightboat_logbook_time_dists.png"),
       width=6.5, height=2.5, units="in", dpi=600)


# Catch
################################################################################

# Plot remaining
range(data$remaining_t, na.rm=T)
xmax <- max(data$remaining_t, na.rm=T)
cutoff <- 1000
g1 <- ggplot(data %>% filter(remaining_t<=cutoff), aes(x=remaining_t)) +
  geom_histogram(breaks=seq(0, xmax, 5)) +
  scale_x_continuous(lim=c(0, cutoff)) +
  # Labels
  labs(y="# of logbook entries", x="Remaining biomass (short tons)", tag="A") +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot sold
range(data$sold_t, na.rm=T)
xmax <- max(data$sold_t, na.rm=T)
cutoff <- 250
g2 <- ggplot(data %>% filter(sold_t<=cutoff), aes(x=sold_t)) +
  geom_histogram(breaks=seq(0, cutoff, 2.5)) +
  scale_x_continuous(lim=c(0, cutoff)) +
  # Labels
  labs(y="# of logbook entries", x="Sold catch (short tons)", tag="B") +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot bait
range(data$bait_lbs, na.rm=T)
xmax <- max(data$bait_lbs, na.rm=T)
cutoff <- 4100
g3 <- ggplot(data %>% filter(bait_lbs<=cutoff), aes(x=bait_lbs)) +
  geom_histogram(breaks=seq(0, cutoff, 50)) +
  scale_x_continuous(lim=c(0, cutoff)) +
  # Labels
  labs(y="# of logbook entries", x="Live bait catch (lbs)", tag="C") +
  # Scale
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "squid_lightboat_logbook_catch_dists.png"),
       width=6.5, height=2.5, units="in", dpi=600)



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

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="Coordinates are summarized in bins to comply with the rule-of-three") +
  # Plot points
  geom_tile(data=xy_ras, mapping=aes(x=long_dd_bin, y=lat_dd_bin, fill=nlogbooks), alpha=0.9) +
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
g

# Export
ggsave(g, filename=file.path(plotdir, "squid_lightboat_logbook_map.png"),
       width=3.5, height=4.75, units="in", dpi=600)
