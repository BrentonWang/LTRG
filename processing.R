library(tidyverse)
library(readxl)
library(janitor)
install.packages("usethis")
usethis::use_github(private = TRUE)

read_county <- function(path) {
  peek    <- read_excel(path, col_names = FALSE, n_max = 40,
                        .name_repair = "minimal")
  hdr_row <- which(peek[[1]] == "Demographic")[1]
  read_excel(path, skip = hdr_row - 1) |> clean_names()
}

files <- list.files("data-raw", pattern = "\\.xlsx$", full.names = TRUE)

all_counties <- tibble(path = files) |>
  mutate(county = str_extract(basename(path), "^[^_]+") |> str_to_lower(),
         data   = map(path, read_county)) |>
  unnest(data) |>
  filter(!is.na(demographic))

all_counties <- all_counties |>
  mutate(
    answer   = str_extract(demographic, "[^:]*$") |> str_squish(),
    question = str_remove(demographic, ":+\\s*[^:]*$") |> str_squish()
  )

count(all_counties, question, sort = TRUE) |> print(n = 40)
distinct(all_counties, answer) |> print(n = 60)

glimpse(all_counties)
count(all_counties, county)

