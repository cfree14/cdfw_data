

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/cpfv_logbooks/raw/CPFV logs"
outdir <- "data/confidential/cpfv_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/cpfv_logbooks"

# Read data
data <- readRDS(file=file.path(outdir, "CDFW_2000_2020_cpfv_logbook_data.Rds"))

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
  filter(!grepl("used|target|method", column)) %>% 
  filter(!column %in% c("logbook_id_use", "year", "month", "day", "filename", "comm_name_orig", "geometry")) %>% 
  # Format columns
  mutate(column=gsub("_", " ", column) %>% stringr::str_to_sentence()) %>% 
  mutate(column=recode(column,
                       "No activity month"="No activity month?",
                       "Non paying"="Non-paying customers?",
                       "N crew fished"="Number of crew who fished",
                       "N kept"="Number of fish kept",
                       "N released"="Number of fish released",
                       "N lost to sea lions"="Number of fish lost to sea lions",
                       "N caught by crew"="Number of fish caught by crew",
                       "N fishers"="Number of fishers",
                       "Hrs fished"="Hours fished (numeric)",
                       "Hm fished"="Hours fished (HH:MM)",
                       "Comm name"="Common name",
                       "Sci name"="Scientific name",
                       "Temp f"="Temperature (SST, °F)",
                       "Depth ft"="Depth (ft)",
                       "Bird interaction"="Bird interaction?",
                       "Descending device"="Descending device?")) %>% 
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
ggsave(g, filename=file.path(plotdir, "cpfv_logbook_completeness.png"), 
       width=5.5, height=4.5, units="in", dpi=600)


# Depth and temperature
################################################################################

# Depth
g1 <- ggplot(data, aes(y=pmin(depth_ft, 500))) +
  geom_boxplot(outlier.color = NA) +
  # Limits
  scale_y_continuous(lim=c(NA, 500),
                     breaks=seq(0,500,100),
                     labels=c(seq(0,400, 100), "≥500")) +
  # Labels
  labs(y="Depth (feet)", tag="A", 
       subtitle="Outliers hidden to comply with rule-of-three") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g1

# Temperature
g2 <- ggplot(data, aes(y=temp_f)) +
  geom_boxplot(outlier.color = NA) +
  # Limits
  scale_y_continuous(lim=c(NA, 100),
                     breaks=seq(0,100,20),
                     labels=c(seq(0,80, 20), "≥100")) +
  # Labels
  labs(y="Temperature (°F)", tag="B",
       subtitle="Outliers hidden to comply with rule-of-three") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_logbook_depth_temp_dists.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Fishing effort
################################################################################

# Hours of fishing
g1 <- ggplot(data, aes(y=pmin(hrs_fished, 24))) +
  geom_boxplot(outlier.color = NA) +
  # Limits
  scale_y_continuous(lim=c(NA, 24),
                     breaks=seq(0, 24, 4),
                     labels=c(seq(0, 20, 4), "≥24")) +
  # Labels
  labs(y="Hours of fishing", tag="A",
       subtitle="Outliers hidden for rule-of-three") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g1

# Number of fishers
g2 <- ggplot(data, aes(y=pmin(n_fishers,100))) +
  geom_boxplot(outlier.color = NA) +
  # Limits
  scale_y_continuous(lim=c(NA, 100),
                     breaks=seq(0, 100, 20),
                     labels=c(seq(0, 80, 20), "≥100")) +
  # Labels
  labs(y="Number of fishers\n(includes passengers, operators,\ncrew, and non-paying guests)", tag="B",
       subtitle="Outliers hidden for rule-of-three") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g2

# Number of fishers
g3 <- ggplot(data, aes(y=n_crew_fished)) +
  geom_boxplot(outlier.color = NA) +
  # Limits
  scale_y_continuous(lim=c(NA, 20),
                     breaks=seq(0,20,5),
                     labels=c(seq(0,15,5), "≥20")) +
  # Labels
  labs(y="Number of crew who fished", tag="C",
       subtitle="Outliers hidden for rule-of-three") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_logbook_fishing_effort_dists.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


boxplot(data$n_fishers-data$n_crew_fished)



# Target species
################################################################################

# Target species data
stats <- data %>% 
  group_by(target_species) %>% 
  summarize(n=n_distinct(logbook_id_use)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>% 
  mutate(target_species=factor(target_species, levels=target_species))

# Plot data
g <- ggplot(stats, aes(x=n/1e3, y=target_species)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Thousands of trips", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_target_species.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Fishing method
################################################################################

# Fishing method data
stats <- data %>% 
  group_by(fishing_method) %>% 
  summarize(n=n_distinct(logbook_id_use)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>% 
  mutate(fishing_method=factor(fishing_method, levels=fishing_method))

# Plot data
g <- ggplot(stats, aes(x=n/1e3, y=fishing_method)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Thousands of trips", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_fishing_method.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Bait used
################################################################################

# TBait used data
stats <- data %>% 
  group_by(bait_used) %>% 
  summarize(n=n_distinct(logbook_id_use)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>% 
  mutate(bait_used=factor(bait_used, levels=bait_used))

# Plot data
g <- ggplot(stats, aes(x=n/1e3, y=bait_used)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Thousands of trips", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_bait_used.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Catch over time
################################################################################

cpfv1 <- wcfish::cdfw_cpfv
cpfv2 <- cpfv1 %>% 
  group_by(year) %>% 
  summarise(catch_n=sum(landings_n)) %>% 
  ungroup()

# Caluclate annual catch
stats <- data %>% 
  group_by(year) %>% 
  summarize(n_kept=sum(n_kept, na.rm=T),
            n_released=sum(n_released, na.rm=T)) %>% 
  ungroup() %>% 
  gather(key="catch_type", value="catch_n", 2:ncol(.)) %>% 
  mutate(catch_type=recode_factor(catch_type,
                                 "n_released"="Released",
                                 "n_kept"="Kept"))

# Plot
g <- ggplot(stats, aes(x=year, y=catch_n/1e6, fill=catch_type)) +
  geom_bar(stat="identity") +
  # Line
  geom_point(data=cpfv2, mapping=aes(x=year, y=catch_n/1e6), inherit.aes = F) +
  geom_line(data=cpfv2, mapping=aes(x=year, y=catch_n/1e6), inherit.aes = F) +
  # Labels
  labs(x="Year", y="Millions of fish") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.margin = margin(0, 0, -3, 0),
        legend.key.size = unit(0.3, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_logbook_catch_over_time.png"), 
       width=4, height=3, units="in", dpi=600)


# Catch by port complex
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Port complexes
port_complexes <- c("Eureka", "Fort Bragg", "Bodega Bay", "San Francisco",
                    "Sacramento Delta", "Monterey", "Morro Bay", 
                    "Santa Barbara", "Los Angeles", "San Diego")

# Build data
stats <- data %>% 
  # Reduce to valid blocks
  filter(!is.na(block_type)) %>% 
  # Summarize # of trips by block id
  group_by(port_complex, block_id, block_type) %>% 
  summarize(nvessels=n_distinct(vessel_id), 
            n_trips=n_distinct(logbook_id)) %>% 
  ungroup() %>% 
  # Remove blocks with fewer than three vessels
  filter(nvessels>=3) %>% 
  # Remove offshore blocks
  filter(block_type!="Offshore") %>% 
  # Reduce to ports of interest
  filter(!port_complex %in% c("Invalid", "Oregon")) %>% 
  # Order ports
  mutate(port_complex=factor(port_complex, levels=port_complexes)) %>% 
  # Spatialize
  left_join(blocks %>% select(block_id, geometry), by="block_id") %>% 
  sf::st_as_sf()

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~port_complex, ncol=5) +
  # Plot blocks
  geom_sf(data=stats, mapping=aes(fill=n_trips),
          color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="A few extremely rarely visited blocks are hidden to comply with the rule-of-three") +
  # Legend
  scale_fill_gradientn(name="# of CPFV trips\n(from 2000-2020)", trans="log10",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=c(1, 10, 100, 1000, 10000)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  scale_x_continuous(breaks=seq(-124, -116, 4)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_ntrips_by_port_complex.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Catch by port complex
################################################################################

# Top 10 species
top10 <- data %>% 
  group_by(comm_name) %>% 
  summarize(n_kept=sum(n_kept)) %>% 
  ungroup() %>% 
  arrange(desc(n_kept)) %>% 
  slice(1:10)

# Build data
stats1 <- data %>% 
  # Reduce to species of interest
  filter(comm_name %in% top10$comm_name) %>% 
  # Reduce to valid blocks
  filter(!is.na(block_type)) %>% 
  # Summarize # of fish by block id
  group_by(comm_name, block_id, block_type) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            n_kept=sum(n_kept)) %>% 
  ungroup() %>% 
  # Remove blocks with fewer than three vessels
  filter(nvessels>=3) %>% 
  # Remove offshore blocks
  filter(block_type!="Offshore") %>% 
  # Remove zeros
  filter(n_kept>0) %>% 
  # Order species
  mutate(comm_name=factor(comm_name, levels=top10$comm_name)) %>% 
  # Spatialize
  left_join(blocks %>% select(block_id, geometry), by="block_id") %>% 
  sf::st_as_sf()

# Plot data
g <- ggplot() +
  # Facet
  facet_wrap(~comm_name, ncol=5) +
  # Plot blocks
  geom_sf(data=stats1, mapping=aes(fill=n_kept),
          color="grey30", lwd=0.1) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  # Labels
  labs(subtitle="A few extremely rarely visited blocks are hidden to comply with the rule-of-three") +
  # Legend
  scale_fill_gradientn(name="Number of fish caught\non CPFV trips from 2000-2020", trans="log10",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=10^c(0:6),
                       labels=parse(text=paste0("10^", 0:6))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-126, -117), ylim = c(32.5, 42)) +
  scale_x_continuous(breaks=seq(-124, -116, 4)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "cpfv_nkept_by_top10_species.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
