

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
                       "Duration min"="Duration (min)",
                       "Long dd"="Longitude (°W)",
                       "Lat dd"="Latitude (°N)",
                       "Catch t"="Catch (tons)",
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


# Depth and temperature
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
  theme_bw() + my_theme
g1

# Temperature
range(data$sst_f, na.rm=T)
g2 <- ggplot(data, aes(x=sst_f)) +
  geom_histogram(breaks=seq(10,80,1)) + 
  # Limits
  scale_x_continuous(breaks=seq(0,100,5)) +
  # Labels
  labs(x="Temperature (°F)", y="# of logbook entries", tag="B") +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "squid_logbook_depth_temp_dists.png"), 
       width=6.5, height=3, units="in", dpi=600)


# Fishing effort
################################################################################

# Plot duration
range(data$duration_min/60, na.rm=T)
g1 <- ggplot(data, aes(x=duration_min/60)) +
  geom_histogram(breaks=seq(0,24,0.25)) +
  # Labels
  labs(y="# of logbook entries", x="Duration (hours)") +
  # Scale
  theme_bw() + my_theme
g1

# # Merge
# g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
# g
# 
# # Export
# ggsave(g, filename=file.path(plotdir, "squid_logbook_fishing_time_dists.png"), 
#        width=6.5, height=2.5, units="in", dpi=600)



# Catch
################################################################################

# Plot catch
range(data$catch_t, na.rm=T)
g1 <- ggplot(data, aes(x=catch_t, fill=limited_yn)) +
  geom_histogram(breaks=seq(0,150,5)) +
  # Labels
  labs(y="# of logbook entries", x="Catch (short tons)") +
  # Legend
  scale_fill_ordinal(name="Limited by market order?", na.value="grey80") +
  # Scale
  theme_bw() + my_theme
g1




