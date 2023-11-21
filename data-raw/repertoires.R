## code to prepare `repertoires` dataset goes here

library(yaml)

config <- yaml.load_file("data-raw/config.yaml")

usethis::use_data(config, overwrite = TRUE)
