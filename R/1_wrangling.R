
get_df_row_items <- function(path_gloria){
  
  df_region_labels <- read_excel(
    paste0(path_gloria, "raw/GLORIA_ReadMe_059a.xlsx"), 
    sheet = "Regions"
  ) %>%
    transmute(
      region_id = `Lfd_Nr`, 
      region_code = `Region_acronyms`, 
      region_name = `Region_names`
    )
  
  df_sector_labels <- read_excel(
    paste0(path_gloria, "raw/GLORIA_ReadMe_059a.xlsx"), 
    sheet = "Sectors"
  ) %>%
    transmute(
      sector_id = `Lfd_Nr`, 
      sector_name = `Sector_names`
    )
  
  df_row_items <- expand.grid(
    sector_id = df_sector_labels$sector_id,
    region_id = df_region_labels$region_id
  ) %>% 
    mutate(
      row_id = as.character(seq(1, 19680))
    ) %>% 
    left_join(df_region_labels) %>%
    left_join(df_sector_labels) %>%
    transmute(
      row_id, 
      region_code, 
      region_name, 
      sector_name
    )
  
}

get_df_regions <- function(df_row_items,
                           list_regions_excluded){
  
  df_regions <- df_row_items %>% 
    select(region_code,
           region_name
    ) %>% 
    unique() %>% 
    filter(!(region_code %in% list_regions_excluded))
  
}

get_df_population <- function(file_population,
                              df_regions){
  
  df_population <- read_ods(
    file_population, 
    sheet = "data"
  ) %>% 
    transmute(
      region_code = `ISO3 Alpha-code`,
      year = `Year`,
      value = `Total Population, as of 1 January (thousands)`
    ) %>% 
    # Replace region codes with the ones in use by GLORIA
    mutate(
      region_code = case_when(
        region_code == "SSD" ~ "SDS", # South Sudan
        TRUE ~ region_code
      )
    ) %>% 
    right_join(df_regions) %>% 
    transmute(
      region_code,
      region_name,
      year,
      indicator_code = "population",
      indicator_name = "Population",
      value,
      unit = "1000 people"
    )
  
}

get_df_social_outcomes <- function(file_life_expectancy,
                                   file_social_thresholds,
                                   file_life_evaluation,
                                   df_regions){
  
  df_life_expectancy <- read_ods(
    file_life_expectancy,
    sheet = "data"
    ) %>%
    transmute(
      region_code = SpatialDimValueCode,
      year = as.character(Period),
      value = FactValueNumeric
      ) %>% 
    right_join(df_regions) %>% 
    transmute(
      region_code,
      region_name,
      year,
      indicator_code = "life_expectancy",
      indicator_name = "Life expectancy",
      value,
      unit = "years"
    )
    
  df_social_thresholds_raw <- read_ods(
    file_social_thresholds,
    sheet = "data"
    ) %>% 
    rename(region_code = iso3c,
           year = date) %>%
    select(
      -c(country)
      ) %>%
    filter(
      complete.cases(.)
      ) %>%
    pivot_longer(
      !c(region_code, year), 
      names_to = "social_threshold", 
      values_to = "value"
    ) %>% 
    mutate(
      is_social_threshold_passed = ifelse(value >= 1, 1, 0)
    ) %>% 
    group_by(
      region_code, 
      year
      ) %>% 
    summarize(
      value = sum(is_social_threshold_passed),
    ) %>% 
    ungroup()
  
  df_social_thresholds <- df_social_thresholds_raw %>% 
    right_join(df_regions) %>% 
    transmute(
      region_code,
      region_name,
      year = as.character(year),
      indicator_code = "nb_social_thresholds_passed",
      indicator_name = "Number of social thresholds passed",
      value,
      unit = "-"
    )
  
  df_life_evaluation_raw <- read_ods(
    file_life_evaluation,
    sheet = "data"
  ) %>% 
    transmute(
    # Replace region names with the ones in use by GLORIA
      region_name = 
        case_when(
          `Country name` == "Ivory Coast" ~ "Cote d'Ivoire",
          `Country name` == "Congo (Kinshasa)" ~ "DR Congo",
          `Country name` == "Congo (Brazzaville)" ~ "Rep Congo",
          `Country name` == "Czech Republic" ~ "CSSR/Czech Republic (1990/1991)",
          `Country name` == "Yemen" ~ "Yemen Arab Republic/Yemen (1990/1991)",
          `Country name` == "Ethiopia" ~ "Ethiopia/DR Ethiopia (1992/1993)",
          `Country name` == "Hong Kong S.A.R. of China" ~ "Hong Kong",
          `Country name` == "Palestinian Territories" ~ "Palestine",
          `Country name` == "Russia" ~ "USSR/Russian Federation (1990/1991)",
          `Country name` == "Serbia" ~ "Yugoslavia/Serbia (1991/1992)",
          `Country name` == "United States" ~ "United States of America",
          `Country name` == "Vietnam" ~ "Viet Nam",
          TRUE ~ `Country name`
        ),
      year = Year,
      value = `Ladder score`
    )
  
  df_life_evaluation <- df_life_evaluation_raw %>%
    right_join(df_regions) %>%
    transmute(
      region_code,
      region_name,
      year = "2017-2019",
      indicator_code = "life_evaluation",
      indicator_name = "Life evaluation",
      value,
      unit = "Ladder score (0-10)"
    )
  
  df_social_outcomes <- df_life_expectancy %>% 
    bind_rows(df_social_thresholds) %>% 
    bind_rows(df_life_evaluation) %>% 
    filter(!is.na(value))

}

get_df_agg_sectors <- function(file_concordances){
  
  df_agg_sectors <- read_excel(
    file_concordances
  )
  
}
