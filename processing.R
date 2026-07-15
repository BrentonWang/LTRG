library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
dir.create("output", showWarnings = FALSE)

read_county <- function(path) {
  peek    <- read_excel(path, col_names = FALSE, n_max = 40,
                        .name_repair = "minimal")
  hdr_row <- which(peek[[1]] == "Demographic")[1]
  read_excel(path, skip = hdr_row - 1) |> clean_names()
}

show_and_save <- function(plot, name, w = 7, h = 4.5) {
  print(plot)
  ggsave(file.path("output", paste0(name, ".png")), plot,
         width = w, height = h, dpi = 300)
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

p_size <- ggplot(household_size, aes(x = str_to_title(county), y = mean_hh_size)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(mean_hh_size, 2)), vjust = -0.4) +
  labs(title = "Mean household size by county",
       subtitle = "Cases with completed narratives",
       x = NULL, y = "Members per household") +
  theme_minimal()

show_and_save(p_size, "household_size")

housing <- all_counties |>
  filter(question == "Status of Current Housing") |>
  group_by(county, answer) |>
  slice_head(n = 1) |>    # TEMP: duplicate "Other" row (form-version issue)
  ungroup() |>
  mutate(answer = fct_relevel(answer, "Habitable", "Partially Habitable",
                              "Uninhabitable", "Other"))

p_housing <- ggplot(housing, aes(x = str_to_title(county), y = case_count, fill = answer)) +
  geom_col(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "Habitable"           = "#4a9c5d",
    "Partially Habitable" = "#e8b93e",
    "Uninhabitable"       = "#c0392b",
    "Other"               = "grey70"
  )) +
  labs(title = "Status of Current Housing",
       subtitle = "Share of cases per county (gaps may reflect survey coverage)",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal()

show_and_save(p_housing, "housing_status")

show_and_save(combined, "summary_figures", w = 8, h = 9)

print(combined)