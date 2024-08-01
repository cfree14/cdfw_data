

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outdir <- "data/confidential/gillnet_logbooks_2023/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/gillnet_logbooks_2023"

# Read data
data <- readRDS(file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data.Rds")) %>% 
  mutate(net_type=ifelse(net_type=="Unknown", NA, net_type))
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
  filter(!column %in% c("net_length_fa_num", "mesh_size_in_num", "soak_hr_num", 
                        "depth_fa_num", "buoy_line_depth_ft_num",
                        "comm_name_orig1", "comm_name_orig2",
                        "target_spp1", "target_spp2",
                        "net_type_orig1", "net_type_orig2",
                        "vessel_id_use", "vessel_id_use_type")) %>% 
  # Format columns
  mutate(column=gsub("_", " ", column) %>% stringr::str_to_sentence()) %>%
  mutate(column=recode(column,
                       "Vessel"="Vessel name",
                       "Vessel id"="Vessel id (CDFW)",
                       "Boat num"="Vessel id (Coast Guard)",
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
                       "Comm name"="Species",
                       "Target spp"="Target species",
                       "Target spp code"="Target species code",
                       "Predator"="Predators",
                       # Catch
                       "Catch lb"="Catch (lbs)",
                       "Catch n"="Catch (# of fish)",
                       # Gear traits
                       "Net length fa"="Net length (fathoms)",
                       "Mesh size in"="Mesh size (in)",
                       "Soak hr"="Soak time (hr)",
                       "Depth fa"="Depth (fathoms)",
                       "Buoy line depth ft"="Buoy line depth (ft)")) %>%
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
ggsave(g, filename=file.path(plotdir, "gillnet_logbook_completeness.png"), 
       width=5.5, height=4.5, units="in", dpi=600)


# Number of logbooks through time
################################################################################

# Stats
stats <- data %>% 
  group_by(year) %>% 
  summarize(nlogs=n()) %>% 
  ungroup()

# Plot
# 2008, 2011, 2012 no data
g <- ggplot(stats, aes(x=year, y=nlogs/1000)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Thousands of logbook entries") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "gillnet_logbook_nlogs.png"), 
       width=5.5, height=3.5, units="in", dpi=600)


# Catch: logs vs. receipts
################################################################################

# Summarize landings
landings <- landings_orig %>% 
  # Trawl fishery for halibut
  filter(comm_name %in% c("California halibut")) %>% 
  filter(grepl("gn|gill net", tolower(gear))) %>% 
  # Summarize
  group_by(year, comm_name) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T)) %>% 
  ungroup()

# Summarize logged landings
logged_lbs <- data %>% 
  filter(comm_name %in% c("California halibut")) %>% 
  group_by(year) %>% 
  summarize(landings_lbs=sum(catch_lb, na.rm=T)) %>% 
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
range(data$depth_fa_num, na.rm=T)
g1 <- ggplot(data, aes(x=depth_fa_num)) +
  geom_histogram(breaks=seq(0,9000,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 300),
                     breaks=seq(0, 300, 25),
                     labels=c(breaks=seq(0, 275, 25), "≥300")) +
  # Labels
  labs(x="Depth (fathoms)", y="# of logbook entries") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Depth
range(data$soak_hr_num, na.rm=T)
g2 <- ggplot(data, aes(x=soak_hr_num)) +
  geom_histogram(breaks=seq(0,1200,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 100),
                     breaks=seq(0, 100, 10),
                     labels=c(breaks=seq(0, 90, 10), "≥100")) +
  # Labels
  labs(x="Soak time (hrs)", y="# of logbook entries") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "gillnet_logbook_depth_soak_dists.png"), 
       width=6.5, height=3, units="in", dpi=600)



# Target species
################################################################################

# Target sepecies
stats <- data %>% 
  group_by(target_spp) %>% 
  summarize(nlogs=n(),
            nvessels=n_distinct(vessel_id_use)) %>% 
  ungroup() %>% 
  filter(!is.na(target_spp) & nvessels>=3)

# Plot data
g <- ggplot(stats, aes(y=reorder(target_spp, desc(nlogs)), x=nlogs)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="# of logbook entries", y="", 
       subtitle="A few rare target species are not shown") +
  scale_x_continuous(trans="log10",
                     breaks = c(1,10,100,1000, 10000, 100000),
                     labels= c("1","10","100","1,000", "10,000", "100,000")) +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "gillnet_logbook_target_spp.png"), 
       width=5.5, height=4.5, units="in", dpi=600)

# Predators
################################################################################

# Predators
stats <- data %>% 
  group_by(predator) %>% 
  summarize(nlogs=n(),
            nvessels=n_distinct(vessel_id_use)) %>% 
  ungroup() %>% 
  filter(!is.na(predator))

# Plot data
g <- ggplot(stats, aes(y=reorder(predator, desc(nlogs)), x=nlogs)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="# of logbook entries", y="") +
  scale_x_continuous(trans="log10",
                     breaks = c(1,10,100,1000, 10000, 100000),
                     labels= c("1","10","100","1,000", "10,000", "100,000")) +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "gillnet_logbook_predator_spp.png"), 
       width=5.5, height=4.5, units="in", dpi=600)
  

# Net characteristis
################################################################################

# Mesh size
range(data$mesh_size_in_num, na.rm=T)
g1 <- ggplot(data, aes(x=mesh_size_in_num)) +
  geom_histogram(breaks=seq(0,900,1)) + 
  # Limits
  scale_x_continuous(lim=c(0, 30),
                     breaks=seq(0, 30, 5),
                     labels=c(breaks=seq(0, 25, 5), "≥30")) +
  # Labels
  labs(x="Mesh size (in)", y="# of logbook entries", tag="A") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Net length
range(data$net_length_fa_num, na.rm=T)
g2 <- ggplot(data, aes(x=net_length_fa_num)) +
  geom_histogram(breaks=seq(0,10000,100)) + 
  # Limits
  scale_x_continuous(lim=c(0, 5000),
                     breaks=seq(0, 5000, 1000),
                     labels=c(seq(0, 4000, 1000), "≥5000")) +
  # Labels
  labs(x="Net length (fathoms)", y="# of logbook entries", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Net length
range(data$buoy_line_depth_ft_num, na.rm=T)
g3 <- ggplot(data, aes(x=buoy_line_depth_ft_num)) +
  geom_histogram(breaks=seq(0,1000,5)) + 
  # Limits
  scale_x_continuous(lim=c(0, 300),
                     breaks=seq(0, 300, 50),
                     labels=c(seq(0, 250, 50), "≥300")) +

  # Labels
  labs(x="Buoy line depth (ft)", y="# of logbook entries", tag="C") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "gillnet_logbook_net_char_dists.png"), 
       width=6.5, height=2.75, units="in", dpi=600)


# Catch
################################################################################
# 
# # Catch stats
# stats <- data %>% 
#   filter(!is.na(species) & !is.na(catch_lbs_receipt)) %>% 
#   group_by(species) %>% 
#   summarize(nvessels=n_distinct(vessel_id),
#            catch_lbs=median(catch_lbs_receipt, na.rm=T)) %>% 
#   ungroup() %>% 
#   filter(nvessels>=3) %>% 
#   arrange(desc(catch_lbs))
# 
# # Prep catch data
# cdata <- data %>% 
#   filter(species %in% stats$species) %>% 
#   mutate(species=factor(species, levels=stats$species))
# 
# # Plot catch
# range(data$catch_lbs_receipt, na.rm=T)
# g <- ggplot(cdata, aes(x=catch_lbs_receipt, y=species)) +
#   geom_boxplot(outlier.shape = NA, linewidth=0.2, fill="grey90") +
#   # Labels
#   labs(x="Catch on receipt (lbs)", y="", subtitle = "Outliers and a few rare species are not shown to comply with the rule-of-three") +
#   scale_x_continuous(trans="log10",
#                      breaks=c(1,10,100,1000, 10000, 100000),
#                      labels=c("1","10","100","1,000", "10,000", "100,000")) +
#   # Scale
#   theme_bw() + my_theme +
#   theme(axis.text.y=element_text(size=4))
# g
# 
# 
# ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbook_catch_dists.png"), 
#        width=5.5, height=10, units="in", dpi=600)


# Block
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build stats
stats <- data %>% 
  # Summarize
  group_by(year, block_id_num) %>% 
  summarize(nlogs=n(),
            nvessels=n_distinct(vessel_id)) %>% 
  ungroup() %>% 
  rename(block_id=block_id_num) %>% 
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
# ggsave(g, filename=file.path(plotdir, "halibut_trawl_logbooks_block_catch.png"), 
#        width=6.5, height=5.25, units="in", dpi=600)



