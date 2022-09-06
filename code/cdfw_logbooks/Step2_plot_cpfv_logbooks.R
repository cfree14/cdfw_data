

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
data <- readRDS(file=file.path(outdir, "CDWF_2000_2020_cpfv_logbook_data.Rds"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Depth and temperature
################################################################################

# Depth
g1 <- ggplot(data, aes(y=pmin(depth_ft, 500))) +
  geom_boxplot() +
  # Limits
  scale_y_continuous(lim=c(NA, 500),
                     breaks=seq(0,500,100),
                     labels=c(seq(0,400, 100), "≥500")) +
  # Labels
  labs(y="Depth (feet)", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g1

# Temperature
g2 <- ggplot(data, aes(y=temp_f)) +
  geom_boxplot() +
  # Limits
  scale_y_continuous(lim=c(NA, 100),
                     breaks=seq(0,100,20),
                     labels=c(seq(0,80, 20), "≥100")) +
  # Labels
  labs(y="Temperature (°F)", tag="A") +
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
  geom_boxplot() +
  # Limits
  scale_y_continuous(lim=c(NA, 24),
                     breaks=seq(0, 24, 4),
                     labels=c(seq(0, 20, 4), "≥24")) +
  # Labels
  labs(y="Hours of fishing", tag="A") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g1

# Number of fishers
g2 <- ggplot(data, aes(y=pmin(n_fishers,100))) +
  geom_boxplot() +
  # Limits
  scale_y_continuous(lim=c(NA, 100),
                     breaks=seq(0, 100, 20),
                     labels=c(seq(0, 80, 20), "≥100")) +
  # Labels
  labs(y="Number of fishers\n(includes passengers, operators,\ncrew, and non-paying guests)", tag="B") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank())
g2

# Number of fishers
g3 <- ggplot(data, aes(y=n_crew_fished)) +
  geom_boxplot() +
  # Limits
  scale_y_continuous(lim=c(NA, 20),
                     breaks=seq(0,20,5),
                     labels=c(seq(0,15,5), "≥20")) +
  # Labels
  labs(y="Number of crew who fished", tag="C") +
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


# Fishing effort
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





