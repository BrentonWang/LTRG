library(tidyverse)
library(readxl)
library(janitor)

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

# Onto the specifics

# Rectify the mixup in Mitchell County
all_counties <- all_counties |>
  mutate(
    tmp               = household_members,
    household_members = if_else(county == "mitchell", case_count, household_members),
    case_count        = if_else(county == "mitchell", tmp, case_count),
    household_count   = if_else(county == "mitchell", tmp, household_count)
  ) |>
  select(-tmp)

# Graphing


household_size <- all_counties |>
  filter(str_detect(question, "^Case Summary/Narrative")) |>
  mutate(mean_hh_size = household_members / household_count)

ggplot(household_size, aes(x = county, y = mean_hh_size)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(mean_hh_size, 2)), vjust = -0.4) +
  labs(
    title = "Mean household size by county",
    subtitle = "Cases with completed narratives; Helene DR-4827 caseload",
    x = NULL, y = "Household members per household"
  ) +
  theme_minimal()
