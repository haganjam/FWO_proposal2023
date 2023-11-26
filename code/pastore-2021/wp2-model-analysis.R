#'
#' @title Preliminary model analysis (Pastore et al. 2021)
#' 
#' @description Use Pastore et al.'s (2021) to get an initial idea of how
#' the net biodiversity effect changes if monocultures are allowed to evolve
#'

# load plotting theme
source("code/helper-plotting-theme.R")

# set the parameters of the mixtures
w <- 0.1 ## competition width
theta <- 1 ## width of intrinsic growth function
K <- c(1.15, 1) ## vector of intrinsic growth potentials
sigma <- c(0.3, 0.3) ## vector of species trait standard deviations
h2 <- c(0.25, 0.25) ## heritability
params <- list(w=w, theta=theta,  K=K, sigma=sigma, h2=h2)
ninit <- c(0.5, 0.5) ## initial densities
muinit <- c(-0.09, 0.09) ## initial trait means
ic <- c(ninit, muinit) ## initial conditions coerced into a vector

tmax <- 100 ## time to integrate equations for
stepout <- tmax/150 ## time step size for output
time <- seq(0, tmax, by=stepout) ## sampling points in time

mix <- 
  ode(func=eqs, y=ic, parms=params, times=time) %>% ## solve ODEs
  organize_results(params)  %>% ## put results in tidy table
  mutate(mono_mix = "mix")
                
# get the niche-fitness differences
rk <- rhokappa(mix, params) ## get rho and kappa ratio through time

# get relevant columns
mix <- dplyr::select(mix, time, species, n, m)

# rename the n column to M
mix <- 
  mix |>
  dplyr::rename(Y = n,
                traitY = m)

# get the evolution of the monocultures through time

# loop over each species
mono <- vector("list", length = length(K))
for(i in 1:length(mono)) {

  params_mono <- list(w=w, theta=theta,  K=K[i], sigma=sigma[i], h2=h2[i])
  ic_mono <- c(ninit[i], muinit[i]) ## initial conditions coerced into a vector
  
  mono[[i]] <- 
    ode(func=eqs, y=ic_mono, parms=params_mono, times=time) %>% ## solve ODEs
    organize_results(params_mono) %>% ## put results in tidy table
    mutate(mono_mix = "mono",
           species = i) 
  
}

# bind the monocultures into a data.frame
mono <- dplyr::bind_rows(mono)

# get the relevant columns
mono <- dplyr::select(mono, time, species, n, m)

# rename the n column to M
mono <- 
  mono |>
  dplyr::rename(M = n,
                traitM = m)

# plot the monocultures and mixtures

# get maximum abundance
max_a <- max( c(max(mix$Y), max(mono$M)) )

p1 <- 
  ggplot(data = mix |> dplyr::mutate(Species = as.character(species)),
         mapping = aes(x = time, y = Y, colour = Species)) +
  geom_line(linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a+0.5)) +
  scale_colour_manual(values = c("#4c8424", "#d49404")) +
  xlab("Time") +
  ylab("Biomass in mixture") +
  theme_meta()
plot(p1)

p2 <- 
  ggplot(data = dplyr::filter(mono, species == 1),
       mapping = aes(x = time, y = M)) +
  geom_line(colour = "#4c8424", linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a+0.5) ) +
  xlab("Time") +
  ylab("Biomass in mixture") +
  theme_meta()
plot(p2)

p3 <- 
  ggplot(data = dplyr::filter(mono, species == 2),
       mapping = aes(x = time, y = M)) +
  geom_line(colour = "#d49404", linewidth = 0.75) +
  scale_y_continuous(limits = c(0, max_a+0.5)) +
  xlab("Time") +
  ylab("") +
  theme_meta()
plot(p3)

# combine the mixture and monoculture data
bef_df <- dplyr::full_join(mono, mix, by = c("time", "species"))

# arrange to by time then species
bef_df <- 
  bef_df |>
  dplyr::arrange(time, species)
bef_df

# calculate the net biodiversity effect
nbe_df <- 
  bef_df |>
  dplyr::group_by(time) |>
  dplyr::summarise(M = mean(M),
                   Y = sum(Y)) |>
  dplyr::mutate(NBE = Y - M)

# plot the results
p4 <- 
  ggplot(data = nbe_df,
       mapping = aes(x = time, y = NBE)) +
  geom_line() +
  xlab("Time") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Net biodiversity effect (NBE)") +
  scale_y_continuous(limits = c(-0.1, max(nbe_df$NBE) + 0.1)) +
  theme_meta()
plot(p4)












