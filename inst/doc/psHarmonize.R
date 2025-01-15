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
head(harmonization_sheet_example) %>%
  kable()

## -----------------------------------------------------------------------------
head(cohort_a) %>%
  kable()

head(cohort_b) %>%
  kable()

head(cohort_c) %>%
  kable()

## -----------------------------------------------------------------------------

harmonization_object <- harmonization(harmonization_sheet = harmonization_sheet_example, 
                          long_dataset = TRUE, 
                          wide_dataset = TRUE,
                          error_log = TRUE, 
                          source_variables = TRUE)


## -----------------------------------------------------------------------------

harmonized_long_dataset <- harmonization_object$long_dataset

head(harmonized_long_dataset) %>%
  kable()


## -----------------------------------------------------------------------------

harmonized_long_dataset %>%
  filter(cohort == 'cohort_a') %>%
  arrange(visit) %>%
  head() %>%
  kable()


## -----------------------------------------------------------------------------

harmonized_wide_dataset <- harmonization_object$wide_dataset

head(harmonized_wide_dataset) %>%
  kable()


## -----------------------------------------------------------------------------
error_log <- harmonization_object$error_log

table(error_log$completed_status)

## ----eval=FALSE---------------------------------------------------------------
# 
# create_error_log_report(harmonized_object, path = './output/', file = 'Error_log.html')
# 

## ----eval=FALSE---------------------------------------------------------------
# 
# create_summary_report(harmonization_object = harmonization_object, path = './output/', file = 'Summary_report')
# 

## ----Image, echo=FALSE--------------------------------------------------------

# Obtain file path of image in package
summary_out_img_path <- system.file('img', 'summary_output.png', package = 'psHarmonize')


## ----results='asis' , echo=FALSE----------------------------------------------

cat('![', 'Summary output example.', '](',summary_out_img_path,')')


