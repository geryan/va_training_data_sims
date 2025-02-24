#BACI design indoor outdoor mosquito biomass

library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)

nsites <- 100

site <- 1:nsites

type <- c(
  rep("control", nsites/2),
  rep("treatment", nsites/2)
)

location <- rep(
  c(
    rep("indoor", nsites/4),
    rep("outdoor", nsites/4)
  ),
  2
)



effect_treatment <- 0.5

effect_indoor <- 0.7

outdoor_control_mean <- 5.3

outdoor_control_sd <- 2.1


site_re <- tibble(
  site,
  site_re = rnorm(
    n = nsites,
    mean = 1,
    sd = 0.05
  )
)

set.seed(20250224)
hist(
  rlnorm(
    n = 10000,
    meanlog = log(outdoor_control_mean),
    sdlog = log(outdoor_control_sd)
  ),
  breaks = 50
)

determine_multiplier <- function(
  type,
  location,
  time,
  site_re,
  effect_treatment,
  effect_indoor
){

  multiplier <- site_re

  if(location == "indoor"){
    multiplier <- multiplier*effect_indoor
  }

  if(type == "treatment" & time == "after"){
    multiplier <- multiplier*effect_treatment
  }

  multiplier
}


df1 <- tibble(
  site_re,
  type,
  location
) |>
  expand_grid(
    time = c("before", "after")
  )

multiplier <- mapply(
  FUN = determine_multiplier,
  df1$type,
  df1$location,
  df1$time,
  df1$site_re,
  MoreArgs = list(
    "effect_treatment" = effect_treatment,
    "effect_indoor" = effect_indoor
  ),
  SIMPLIFY = TRUE
)

baci_data <- tibble(
  df1,
  multiplier
) |>
  rowwise() |>
  mutate(
    n = map(
      .x = multiplier,
      .f = function(
    multiplier,
    outdoor_control_mean,
    outdoor_control_sd
        ){
        rlnorm(
          n = 1,
          meanlog = multiplier*outdoor_control_mean,
          sdlog = outdoor_control_sd
        )
      },
    outdoor_control_mean,
    outdoor_control_sd
    ) |>
      unlist(),
    time = factor(
      time,
      levels = c("before", "after")
    )
  )



ggplot(baci_data) +
  geom_boxplot(
    aes(
      x = location,
      y = log(n),
      fill = type
    )
  ) +
  facet_grid(.~time)


baci_data |>
  select(
    site,
    type,
    location,
    time,
    n
  ) |>
  write_csv(
    file = "baci_data.csv"
  )
