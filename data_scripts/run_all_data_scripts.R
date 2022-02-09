files <- list.files(here::here("data_scripts"), full.names = T)

purrr::map(files, source)