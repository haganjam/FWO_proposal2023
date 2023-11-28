#'
#' @title Analyse the Jena data
#' 
#' @description Use the Jena data to see if the net biodiversity effect
#' has changed over time
#'

# load relevant libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# load the Jena biomass data
jena_bio <- read_delim(url("https://ndownloader.figshare.com/files/5608847"), delim = ",")
head(jena_bio)
names(jena_bio)

# create a vector of species names
sp_names <- names(jena_bio[, 85:144])

# remove the first plots that were not sown with any species
jena_bio <- filter(jena_bio, !(sowndiv %in% c(0)) )

# remove species presence columns
jena_bio <- select(jena_bio, -all_of(paste0("p", sp_names)))

# create a season variable for jena_bio
unique(jena_bio$month)

jena_bio <- 
  jena_bio |>
  mutate(season = if_else(month %in% c("May", "Jun"), "spring", "summer"))

# subset out the spring data only
jena_bio <- 
  jena_bio |>
  filter(season == "spring")

# replace the NAs with zeros
jena_bio <- 
  jena_bio |>
  mutate(across(.cols = all_of(sp_names), ~replace(., is.na(.), 0)))

# remove rows of the data where there are missing values i.e. -9999 values in the sp_names
jena_bio <- 
  jena_bio |>
  filter_at(all_of(sp_names), all_vars(. >= 0 ) )

# take the first three sub-samples as not all plots have four sub-samples
unique(jena_bio$subsample)

jena_bio <- 
  jena_bio |>
  filter(subsample %in% c(1, 2, 3))

# select out the relevant columns
jena_bio <- 
  jena_bio |>
  select(plotcode, season, time, subsample, sowndiv, target.biomass, all_of(sp_names))

# for each species and for target biomass, take the average value
jena_bio <- 
  jena_bio |>
  group_by(plotcode, time) |>
  summarise(across(.cols = all_of(c("sowndiv", "target.biomass", sp_names)), ~mean(., na.rm = TRUE) ), .groups = "drop")

# get the monocultures
jena_mono <-
  jena_bio |>
  dplyr::filter(sowndiv == 1)
head(jena_mono)

# get a list of species that are present in monoculture
m_names <- sp_names[colSums(jena_mono[, sp_names]) > 0]
length(m_names)

# only use those columns
jena_mono <- jena_mono[, c("plotcode", "time", "sowndiv", "target.biomass", m_names)]

# get the two-species mixtures
jena_mix <-
  jena_bio |>
  dplyr::filter(sowndiv == 2)
head(jena_mix)

# get the species that are present in monoculture
jena_mix <- jena_mix[, c("plotcode", "time", "sowndiv", "target.biomass", m_names)]

# only keep rows where biomass is positive for both at least two species
jena_mix <- jena_mix[apply(jena_mix[, m_names], 1, function(x) sum(x > 0)) == 2, ]
head(jena_mix)

# there are only three species that match these criteria
jena_mix <- jena_mix[, c("plotcode", "time", "sowndiv", "target.biomass", m_names[colSums(jena_mix[, m_names]) > 0] )]

# get the relevant monocultures
jena_mono[, names(jena_mix)]

# mixture plot1
mix1 <- dplyr::filter(jena_mix, plotcode == "B1A16")

# get relevant columns
mix1 <- dplyr::select(mix1, -plotcode, -Bel.per, -target.biomass, -sowndiv)
head(mix1)

# pull into the long-format
mix1 <- 
  mix1 |>
  tidyr::pivot_longer(cols = c("Poa.pra", "Pla.lan"),
                      names_to = "species",
                      values_to = "Y")

# get relevant monoculture data
m1 <- 
  jena_mono[, c("time", "Poa.pra")] |>
  dplyr::filter(Poa.pra > 0)

m2 <- 
  jena_mono[, c("time", "Pla.lan")] |>
  dplyr::filter(Pla.lan > 0)

m3 <- dplyr::full_join(m1, m2, by = "time")

# pull monocultures into the long-format
mono1 <- 
  m3 |>
  tidyr::pivot_longer(cols = c("Poa.pra", "Pla.lan"),
                      names_to = "species",
                      values_to = "M")

# bind mono1 and mix1
mix1 <- dplyr::full_join(mono1, mix1, by = c("time", "species"))

# get maximum abundance
max_a1 <- max( c(max(mix1$Y), max(mix1$M)) )

mix1 <- 
  mix1 |> 
  dplyr::mutate(Species = as.character(species)) |>
  dplyr::mutate(Species = ifelse(Species == "Poa.pra", "P.pratensis", "P.lanceolata"))

p1 <- 
  ggplot(data = mix1,
         mapping = aes(x = time, y = Y, colour = Species)) +
  geom_line(linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a1+5)) +
  scale_colour_manual(values = c("#2c6ca4", "#d49404")) +
  xlab("Months") +
  ylab("Biomass in mixture") +
  theme_meta() +
  theme(legend.position = "bottom")
plot(p1)

p2 <- 
  ggplot(data = dplyr::filter(mix1, species == "Poa.pra") |> dplyr::mutate(Species = as.character(species)),
         mapping = aes(x = time, y = M)) +
  geom_line(colour = "#2c6ca4", linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a1+5) ) +
  xlab("Months") +
  ylab("Biomass in monoculture") +
  theme_meta()
plot(p2)

p3 <- 
  ggplot(data = dplyr::filter(mix1, species == "Pla.lan") |> dplyr::mutate(Species = as.character(species)),
         mapping = aes(x = time, y = M)) +
  geom_line(colour = "#d49404", linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a1+5)) +
  xlab("Months") +
  ylab("") +
  theme_meta()
plot(p3)

# calculate the net biodiversity effect
nbe1 <- 
  mix1 |>
  dplyr::group_by(time) |>
  dplyr::summarise(M = mean(M),
                   Y = sum(Y)) |>
  dplyr::mutate(NBE = Y - M)

# plot the results
p4 <- 
  ggplot(data = nbe1,
         mapping = aes(x = time, y = NBE)) +
  geom_line(linewidth = 0.75) +
  xlab("Months") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Net biodiversity effect") +
  scale_y_continuous() +
  theme_meta()
plot(p4)

# export the plots
plot_list <- list(p1, p2, p3, p4)
for(i in 1:length(plot_list)) {
  saveRDS(object = plot_list[[i]], file = paste0("figures-tables/jena-plot-", i, ".rds") )
}

# mixture plot2
mix2 <- dplyr::filter(jena_mix, plotcode == "B4A14")
head(mix2)

# get relevant columns
mix2 <- dplyr::select(mix2, -plotcode, -Poa.pra, -target.biomass, -sowndiv)
head(mix2)

# pull into the long-format
mix2 <- 
  mix2 |>
  tidyr::pivot_longer(cols = c("Bel.per", "Pla.lan"),
                      names_to = "species",
                      values_to = "Y")

# get relevant monoculture data
m1 <- 
  jena_mono[, c("time", "Bel.per")] |>
  dplyr::filter(Bel.per > 0)

m2 <- 
  jena_mono[, c("time", "Pla.lan")] |>
  dplyr::filter(Pla.lan > 0)

m3 <- dplyr::left_join(m1, m2, by = "time")

# pull monocultures into the long-format
mono2 <- 
  m3 |>
  tidyr::pivot_longer(cols = c("Bel.per", "Pla.lan"),
                      names_to = "species",
                      values_to = "M")

# bind mono1 and mix1
mix2 <- dplyr::full_join(mono2, mix2, by = c("time", "species"))

# complete cases
mix2 <- mix2[complete.cases(mix2), ]

# get maximum abundance
max_a2 <- max( c(max(mix2$Y), max(mix2$M)) )

p1 <- 
  ggplot(data = mix2 |> dplyr::mutate(Species = as.character(species)),
         mapping = aes(x = time, y = Y, colour = Species)) +
  geom_line(linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a2+5)) +
  scale_colour_manual(values = c("#2c6ca4", "#d49404")) +
  xlab("Time") +
  ylab("Biomass in mixture") +
  theme_meta() +
  theme(legend.position = "bottom")
plot(p1)

p2 <- 
  ggplot(data = dplyr::filter(mix2, species == "Bel.per") |> dplyr::mutate(Species = as.character(species)),
         mapping = aes(x = time, y = M)) +
  geom_line(colour = "#2c6ca4", linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a2+5) ) +
  xlab("Time") +
  ylab("Biomass in monoculture") +
  theme_meta()
plot(p2)

p3 <- 
  ggplot(data = dplyr::filter(mix1, species == "Pla.lan") |> dplyr::mutate(Species = as.character(species)),
         mapping = aes(x = time, y = M)) +
  geom_line(colour = "#d49404", linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a1+5)) +
  xlab("Time") +
  ylab("") +
  theme_meta()
plot(p3)

# calculate the net biodiversity effect
nbe1 <- 
  mix1 |>
  dplyr::group_by(time) |>
  dplyr::summarise(M = mean(M),
                   Y = sum(Y)) |>
  dplyr::mutate(NBE = Y - M)

# plot the results
p4 <- 
  ggplot(data = nbe1,
         mapping = aes(x = time, y = NBE)) +
  geom_line() +
  xlab("Time") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Net biodiversity effect (NBE)") +
  scale_y_continuous() +
  theme_meta()
plot(p4)
