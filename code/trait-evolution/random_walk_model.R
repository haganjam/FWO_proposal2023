#'
#' @title Trait evolution model
#' 
#' @description Simulate a model of trait evolution using a random walk
#'

# load relevant libraries
library(dplyr)
library(ggplot2)

# get the plotting theme
source("code/helper-plotting-theme.R")

# simulate a random walk

# how many species? 
S <- 5

# timesteps
steps <- 200

# how much sd
sd <- 0.1

# simulate starting trait values
trait_start <- runif(n = S, min = 0, max = 5)

# sort from max to min
trait_start <- sort(trait_start, decreasing = TRUE)

trait_sp <- vector("list", length = S)
for(i in 1:S) {
  
  trait_vec <- vector(length = steps)
  trait_vec[1] <- trait_start[i]
  for(j in 2:steps) {
    
    trait_vec[j] <- trait_vec[j-1] + rnorm(n = 1, mean = 0, sd = sd)
    
  }
  
  # pull into a data.frame
  trait_vec <- dplyr::tibble(sp = paste0("sp_", i),
                             time = 1:steps,
                             trait = trait_vec)
  
  trait_sp[[i]] <- trait_vec
  
}

# bind into a data.frame
trait_df <- dplyr::bind_rows(trait_sp)

# plot the results
p1 <- 
  ggplot(data = trait_df, 
       mapping = aes(x = time, y = trait, colour = sp)) +
  geom_line() +
  scale_colour_manual(values = MetBrewer::met.brewer("Lakota", n = 5)) +
  theme_meta() +
  theme(legend.position = "none", 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank())
plot(p1)

# save the plot
ggsave(filename = "figures-tables/random-walk-model.pdf", p1,
       units = "cm", width = 4.5, height = 4.5)









