## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

library(dplyr)
library(knitr)
library(stringr)
library(tidyr)
library(glue)
library(purrr)

library(psHarmonize)

## -----------------------------------------------------------------------------

harmonization_sheet_example %>%
  filter(study == 'Cohort A' & item == 'education') %>%
  select(code_type, code1) %>%
  kable()


## -----------------------------------------------------------------------------

harmonization_sheet_example %>%
  filter(study == 'Cohort A' & item == 'weight') %>%
  select(code_type, code1) %>%
  kable()


