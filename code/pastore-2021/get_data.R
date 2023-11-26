## This script compiles data from repeated runs of the model.
##
## Usage: one can simply run the script in an R GUI such as RStudio, or execute
## "Rscript get_data.R" at the command prompt. This, however, will dump the
## output to the screen and not a file. To save the generated output, one should
## redirect it to a file. For instance, in Unix-like systems, one can execute
##
## Rscript get_data.R > [output_file.csv]
##
## where [output_file.csv] is the name, with path, of the
## data file to which the output should be written.

# load relevant functions
source("code/pastore-2021/functions.R") ## functions for integrating ODEs and organizing data


## --------------------------- parameter definitions ----------------------------

sigma <- c(1, 1) ## vector of trait standard deviations
h2 <- c(0.1, 0.1) ## vector of heritabilities
ninit <- c(1, 1) ## initial species densities
tmax <- 1e6 ## max simulation time
time <- seq(0, tmax, by=tmax/10) ## sampling points in time for the ODE solutions

fact <- expand.grid( ## table for factorial numerical experiment
  w=c(3), ## competition widths
  theta=exp(log(5)), ## intrinsic growth widths
  K1=exp(log(2)), ## species 1 max intrinsic growth rates
  mu1=c(2.5), ## species 1 initial trait means
  mu2=c(-2.5) ## species 2 initial trait means
) %>%
  as_tibble %>% ## convert data frame to tibble
  filter(mu2<=mu1) %>% ## species 1 always to the right of species 2 initially
  ## confine initial trait means to [-theta, theta] interval:
  filter(mu1>=(-theta), mu1<=theta, mu2>=(-theta), mu2<theta)
print(fact)

## write header to stdout (or file, if the output is redirected):
# write(paste0("w,theta,K1,muinit1,muinit2,ninit1,ninit2,mufinal1,mufinal2,",
             # "nfinal1,nfinal2,nichei,fiti,nichef,fitf,coexi,coexf"), stdout())


## ---------------------------- perform calculations ----------------------------

for (r in 1:nrow(fact)) { ## go through each row of experiments
  
  ## set parameters and initial conditions
  params <- list(w=fact$w[r], theta=fact$theta[r], K=c(fact$K1[r], 1),
                 sigma=sigma, h2=h2) ## list of model parameters
  muinit <- c(fact$mu1[r], fact$mu2[r]) ## initial trait means
  ic <- c(ninit, muinit) ## initial conditions coerced into a vector
  
  ## solve ODE system and extract nice overlap & competitive differences
  rkappa <- ode(func=eqs, y=ic, parms=params, times=time) %>% ## solve ODEs
    organize_results(params) %>% ## put results in tidy table
    rhokappa(params) ## extract nice overlap & competitive differences
  
  ## put quantities of interest into variables
  mfinal1 <- rkappa$m1[nrow(rkappa)] ## species 1 final trait mean
  mfinal2 <- rkappa$m2[nrow(rkappa)] ## species 2 final trait mean
  nfinal1 <- rkappa$n1[nrow(rkappa)] ## species 1 final density
  nfinal2 <- rkappa$n2[nrow(rkappa)] ## species 2 final density
  nichei <- rkappa$rho[1] ## initial niche overlap
  fiti <- rkappa$kapparatio[1] ## initial competitive difference
  nichef <- rkappa$rho[nrow(rkappa)] ## final niche overlap
  fitf <- rkappa$kapparatio[nrow(rkappa)] ## final competitive difference
  
  ## set extinction threshold: final densities below 1e-6 are considered extinct
  nfinal1 <- ifelse(nfinal1<1e-6, 0, nfinal1)
  nfinal2 <- ifelse(nfinal2<1e-6, 0, nfinal2)
  
  ## do the species coexist at start & end (based on rho < k1/k2 < 1/rho)?
  coexi <- ifelse((nichei<fiti)&(fiti<1/nichei), TRUE, FALSE) ## initial coex
  coexf <- ifelse((nichef<fitf)&(fitf<1/nichef), TRUE, FALSE) ## final coex
  if ((nfinal1==0)|(nfinal2==0)) coexf <- FALSE ## no coex if both spp dead
  
  ## write results to stdout (or file, if the output is redirected)
  write(paste(paste(fact[r,], collapse=","), ## parameters & initial trait means
              ninit[1], ## species 1 initial density
              ninit[2], ## species 2 initial density
              mfinal1, ## species 1 final trait mean
              mfinal2, ## species 2 final trait mean
              nfinal1, ## species 1 final density
              nfinal2, ## species 2 final density
              nichei, ## initial niche overlap
              fiti, ## initial competitive diff
              nichef, ## final niche overlap
              fitf, ## final competitive diff
              coexi, ## did the species coexist initially?
              coexf, ## did the species coexist at the end?
              sep=","), "figures-tables/pastore-2021-output.txt") ## write row to stdout
  
} ## for loop ends