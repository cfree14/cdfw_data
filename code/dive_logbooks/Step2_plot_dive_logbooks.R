

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/dive_logbooks/raw"
outdir <- "data/confidential/dive_logbooks/processed"
keydir <- "data/public/cdfw_keys/processed"
plotdir <- "figures/dive_logbooks"

# Read data
data <- readRDS(file=file.path(outdir, "CDFW_2000_2020_dive_logbooks.Rds"))


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

# Depth (ft)
################################################################################

# Data
stats <- data %>% 
  select(logbook_id, depth_min_ft, depth_max_ft) %>% 
  gather(key="type", value="depth_ft", 2:3) %>% 
  mutate(type=recode_factor(type, 
                            "depth_min_ft"="Minimum\ndepth",
                            "depth_max_ft"="Maximum\ndepth"))

# Plot
g <- ggplot(stats, aes(x=type, y=depth_ft)) +
  geom_boxplot() +
  # Labels
  labs(x="", y="Depth (ft)") +
  scale_y_continuous(trans="log2", breaks=c(1,2, 5,10,20,50,100,200,500)) +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbook_depth.png"), 
       width=3.5, height=3.5, units="in", dpi=600)


# Duration (hrs)
################################################################################


# Plot
g <- ggplot(data, aes(y=pmin(hours, 10))) +
  geom_boxplot() +
  # Labels
  labs(x="", y="Dive time (hrs)") +
  scale_y_continuous(breaks=seq(0,10,2), labels=c(seq(0,8,2), ">10")) +
  # Theme
  theme_bw() + bar_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "dive_logbook_effort.png"), 
       width=3.5, height=3.5, units="in", dpi=600)






