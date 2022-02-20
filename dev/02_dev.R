# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )

golem::use_recommended_deps()
usethis::use_package("shinyjs")
usethis::use_package("shinyauthr")
usethis::use_package("vroom")
usethis::use_package("readxl")
usethis::use_package("plyr")
usethis::use_package("dplyr")
usethis::use_package("gt")
usethis::use_package("fs")
usethis::use_package("rlang")
usethis::use_package("rmarkdown")
usethis::use_package("magrittr")
usethis::use_package("purrr")
usethis::use_package("stringr")
usethis::use_package("forcats")
usethis::use_package("tidyr")
usethis::use_package("assertthat")
usethis::use_package("broom")
usethis::use_package("ggplot2")
usethis::use_package("rsample")
usethis::use_package("lubridate")

attachment::att_from_rscripts()
attachment::att_from_rscripts() |> purrr::map(usethis::use_package)

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "name_of_module1" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module
golem::add_module( name = "Lectura" ) # Name of the module


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

user_base <- readr::read_rds("user_base.rds")

usethis::use_data_raw( name = "user_base", open = FALSE ) 
# √ Creating 'data-raw/'
# √ Writing 'data-raw/user_base.R'
# * Finish the data preparation script in 'data-raw/user_base.R'
# * Use `usethis::use_data()` to add prepared data to package

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("SmartModelApp")
devtools::build_vignettes()
devtools::document()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

