## ----------------------------------------------------------------------------------------
## Standalone SLURM entry point for monty / dust2 / odin2 plague fits.
##
## The stochastic humans plague model lives in
##   inst/odin/plague_stochastic_humans.R
## and is exported as yersinia::plague_stochastic_humans (compiled at package
## build time). The filter,
## packer, prior, and sampler assembly is shared with vignettes/monty.qmd
## through yersinia::plague_fit_setup(). Do NOT inline the model or the
## pipeline here — edits belong in R/monty_fit.R or the odin2 source.
##
## Usage:
##   Rscript monty.R <n_cores> <outbreak_index>
## ----------------------------------------------------------------------------------------
install.packages(
  c("monty", "dust2", "odin2", "posterior", "bayesplot"),
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))

remotes::install_github('flmnh-ai/yersinia')

## ----------------------------------------------------------------------------------------
library(monty)
library(odin2)
library(dust2)

library(posterior)
library(bayesplot)

library(yersinia)
library(dplyr)

## ----------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
n_cores <- as.integer(args[[1]])
outbreak_ids <- as.integer(args[[2]])

outbreak_list <- unique(outbreaks$outbreak_id)

dat <- outbreaks |>
  filter(outbreak_id == outbreak_list[[outbreak_ids]]) |>
  select(time = day, deaths)

## ----------------------------------------------------------------------------------------
## Assemble the fitting pipeline. Defaults: historical scenario for fixed
## params, plague_fit_fitted_names() for swept params, Uniform + Exponential
## priors, diagonal VCV keyed to those priors. Override any of these via
## arguments to plague_fit_setup() (see ?plague_fit_setup).
setup <- plague_fit_setup(
  dat,
  scenario = "historical",
  n_particles = 2000,
  n_threads = max(1, floor(n_cores / 4))
)

## ----------------------------------------------------------------------------------------
runner <- monty_runner_callr(n_workers = 4)
samples <- monty_sample(setup$posterior, setup$sampler, 10000,
                        runner = runner, n_chains = 4, burnin = 2000)


## ----------------------------------------------------------------------------------------
samples_df <- as_draws_df(samples)
saveRDS(samples_df, 'samples_df.rds')
