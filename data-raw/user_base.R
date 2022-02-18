## code to prepare `user_base` dataset goes here
user_base <- readr::read_rds("~/Trabajo/R/Projects/SM_Valid/user_base.rds")
usethis::use_data(user_base, overwrite = TRUE)
user_base
