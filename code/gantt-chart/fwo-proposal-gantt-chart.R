#'
#' @title Gantt chart FWO proposal
#' 
#' @description Create a Gantt chart for the 2023 FWO post-doctoral
#' fellowship proposal
#'

# load plotting theme
source("code/helper-plotting-theme.R")

# load relevant libraries
library(dplyr)
library(readr)
library(ganttrify)

# load the project data
gantt_dat <- readr::read_csv("data/gantt-chart-data1.csv")
head(gantt_dat)

# sort out the levels of the work packages
gantt_dat$wp <- factor(gantt_dat$wp, levels = c("WP1", "WP2", "WP3", "Other"))

# load project spot data
spot_dat <- readr::read_csv("data/gantt-chart-data2.csv")

# produce a gant chart
p1 <- 
  ganttrify::ganttrify(project = gantt_dat,
                       spots = spot_dat,
                       project_start_date = "2024-10",
                       month_breaks = 3,
                       size_wp = 4,
                       month_number_label = FALSE,
                       mark_years = TRUE,
                       alpha_wp = 0.9,
                       alpha_activity = 0.6,
                       colour_stripe = "white",
                       show_vertical_lines = FALSE,
                       spot_fontface = "plain",
                       colour_palette =  MetBrewer::met.brewer("Lakota"),
                       spot_fill = ggplot2::alpha(c("white"), 0.7),
                       spot_padding = ggplot2::unit(0.2, "lines")) +
  theme(axis.text = element_text(colour="black",size=9.5))
plot(p1)

# export the plot
ggsave("figures-tables/gantt-chart.pdf", p1,
       units = "cm", width = 22, height = 12)

