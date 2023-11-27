#'
#' @title Combine the figures from the model and jena data
#' 
#' @description Use cowplot to combine the .rds files
#'

# load relevant libraries
library(ggplot2)

# load plotting theme
source("code/helper-plotting-theme.R")

# get a list of files
files <- list.files("figures-tables/")
print(files)

# load the model files
mod_names <- files[grepl("model-plot", files)]
mod_plots <- vector("list", length = length(mod_names))
for(i in 1:length(mod_names)) {
  
  mod_plots[[i]] <- readRDS(file = paste0("figures-tables/", mod_names[i]))
  
}

# get the legend from the first plot
mod_leg <- cowplot::get_legend(mod_plots[[1]])
plot(mod_leg)

# remove the legend from the first plot
mod_plots[[1]] <- mod_plots[[1]] + theme(legend.position = "none")

# combine into four panel figure
p1 <- 
  cowplot::plot_grid(mod_plots[[2]], mod_plots[[3]], mod_plots[[1]], mod_plots[[4]], 
                     labels = c("a", "b", "c", "d"),
                     label_fontface = "plain", label_size = 10,
                     nrow = 2, ncol = 2, align = "hv", axis = "b", vjust = 1)
plot(p1)

# combine with the legend
p2 <- cowplot::plot_grid(p1, mod_leg, nrow = 2, ncol = 1,
                         rel_heights = c(1, 0.05))
plot(p2)

# export the plot
ggsave(filename = "figures-tables/fig_pre1.pdf", p2, units = "cm",
       width = 11, height = 12.5)

# load the jena files
jena_names <- files[grepl("jena", files)]
jena_plots <- vector("list", length = length(jena_names))
for(i in 1:length(jena_names)) {
  
  jena_plots[[i]] <- readRDS(file = paste0("figures-tables/", jena_names[i]))
  
}

# get the legend from the first plot
jena_leg <- cowplot::get_legend(jena_plots[[1]])
plot(jena_leg)

# remove the legend from the first plot
jena_plots[[1]] <- jena_plots[[1]] + theme(legend.position = "none")

# combine into four panel figure
p3 <- 
  cowplot::plot_grid(jena_plots[[2]], jena_plots[[3]], jena_plots[[1]], jena_plots[[4]], 
                     labels = c("e", "f", "g", "h"),
                     label_fontface = "plain",
                     label_size = 10,
                     nrow = 2, ncol = 2, align = "hv", axis = "b", vjust = 1)
plot(p3)

# combine with the legend
p4 <- cowplot::plot_grid(p3, jena_leg, nrow = 2, ncol = 1,
                         rel_heights = c(1, 0.05))
plot(p4)

# export the plot
ggsave(filename = "figures-tables/fig_pre2.pdf", p4, units = "cm",
       width = 11, height = 12.5)

### END
