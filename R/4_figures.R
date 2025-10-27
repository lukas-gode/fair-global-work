
get_df_fig1a <- function(df_IEL,
                         df_EEL,
                         year_y0,
                         df_regions){
  
  df_IEL_EEL_y0 <- df_IEL %>%
    bind_rows(df_EEL) %>% 
    right_join(df_regions) %>% 
    filter(year == year_y0) %>%
    group_by(
      region_code, 
      region_name, 
      year, 
      indicator
      ) %>% 
    summarize(
      value = sum(value)
      ) %>% 
    ungroup()
  
  df_ratio_IEL_to_EEL_y0 <- df_IEL_EEL_y0 %>% 
    pivot_wider(
      names_from = "indicator",
      values_from = "value"
      ) %>% 
    transmute(
      region_code,
      region_name,
      year,
      indicator_code = "ratio_IEL_to_EEL",
      indicator_name = "Ratio of imports to exports of embodied labour",
      value = round(IEL / EEL, 3),
      unit = "-"
    )
  
  df_fig1a <- df_ratio_IEL_to_EEL_y0 %>% 
    arrange(region_code)
  
}

get_df_fig1b <- function(df_fig1a,
                         df_population){
  
  df_population <- df_population %>% 
    transmute(
      region_code,
      region_name,
      year,
      population = round(value / 1e3, 3)
    )
  
  df_fig1b <- df_fig1a %>%  
    left_join(df_population) %>% 
    mutate(
      binned_ratio_IEL_to_EEL = cut(
        value, 
        breaks = c(0, 1/10, 1/5, 1/2, 1, 2, 3, 4, 100),
        labels = c("< 1:10", "1:10 to 1:5", "1:5 to 1:2",  "1:2 to 1:1", 
                   "1:1 to 2:1", "2:1 to 3:1", "3:1 to 4:1", "> 4:1")
      )
    ) %>% 
    group_by(binned_ratio_IEL_to_EEL, year) %>% 
    summarize(population = sum(population)) %>%
    transmute(
      year,
      binned_ratio_IEL_to_EEL,
      indicator_code = "population",
      indicator_name = "Population",
      value = round(population),
      unit = "M people" 
    ) %>% 
    arrange(desc(binned_ratio_IEL_to_EEL))
  
}

get_df_fig2a <- function(df_IEL,
                         df_EEL,
                         df_regions){
  
  df_fig2a <- df_IEL %>%
    bind_rows(df_EEL) %>% 
    right_join(df_regions) %>%
    group_by(region_code, region_name, year, indicator) %>% 
    summarize(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(
      names_from = "indicator",
      values_from = "value"
    )
  
  df_fig2a_world <- df_fig2a %>% 
    group_by(year) %>% 
    summarize(
      region_code = "WORLD",
      region_name = "World",
      IEL = sum(IEL),
      EEL = sum(EEL)
    ) %>% 
    ungroup()
  
  df_fig2a <- df_fig2a %>% 
    rbind(
      df_fig2a_world
    ) %>% 
    mutate(
      ratio_IEL_to_EEL = IEL / EEL
    ) %>% 
    transmute(
      region_code,
      region_name,
      year,
      indicator_code = "ratio_IEL_to_EEL",
      indicator_name = "Ratio of imports to exports of embodied labour",
      value = round(ratio_IEL_to_EEL, 3),
      unit = "-"
    )
  
  df_fig2a <- df_fig2a %>% 
    mutate(
      value = case_when(
        !is.finite(value) ~ NA,
        is.na(value) ~ NA,
        TRUE ~ value
      )
    )
  
}

get_df_fig2b <- function(df_EEL_by_src_sector,
                         df_LS,
                         df_regions){
  
  df_fig2b <- df_EEL_by_src_sector %>%
    group_by(region_code, year) %>% 
    summarize(EEL = sum(value)) %>% 
    ungroup() %>% 
    left_join(df_LS) %>%
    right_join(df_regions) %>%
    transmute(
      region_code,
      region_name,
      year,
      EEL,
      LS = value
      )
  
  df_fig2b_world <- df_fig2b %>% 
    group_by(year) %>% 
    summarize(
      region_code = "WORLD",
      region_name = "World",
      EEL = sum(EEL),
      LS = sum(LS)
    )
  
  df_fig2b <- df_fig2b %>% 
    rbind(df_fig2b_world) %>% 
    mutate(pc_LS_as_EEL = EEL / LS) %>% 
    transmute(
      region_code,
      region_name,
      year,
      indicator_code = "pc_LS_as_EEL",
      indicator_name = "Percentage share of domestic labour supplied as exports of embodied labour",
      value = round(pc_LS_as_EEL * 100 , 2),
      unit = "%"
    )
  
  df_fig2b <- df_fig2b %>% 
    mutate(
      value = case_when(
        !is.finite(value) ~ NA,
        is.na(value) ~ NA,
        TRUE ~ value
      )
    )
  
}

get_df_fig2c <- function(df_EEL_by_src_sector,
                         df_agg_sectors,
                         df_regions){
  
  df_fig2c <- df_EEL_by_src_sector %>% 
    left_join(
      df_agg_sectors,
      join_by(src_sector == gloria_sector)
    ) %>% 
    rename(src_sector_agg = agg_sector) %>% 
    right_join(df_regions) %>% 
    group_by(
      region_code, 
      region_name, 
      src_sector_agg, 
      year, 
      indicator
    ) %>% 
    summarize(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(
      names_from = "indicator",
      values_from = "value"
    ) %>% 
    group_by(region_code, region_name, year) %>% 
    mutate(
      total_EEL = sum(EEL)
    )
  
  df_fig2c_world <- df_fig2c %>% 
    group_by(year, src_sector_agg) %>% 
    summarize(
      region_code = "WORLD",
      region_name = "World",
      EEL = sum(EEL),
      total_EEL = sum(total_EEL)
    )
  
  df_fig2c <- df_fig2c %>% 
    rbind(df_fig2c_world) %>% 
    mutate(
      pc_EEL = EEL / total_EEL
    ) %>% 
    ungroup() %>%
    transmute(
      region_code,
      region_name,
      src_sector = factor(src_sector_agg,
                          levels = c(
                            "Primary sectors",
                            "Manufacturing",
                            "Services: trade, transport",
                            "Services: other"
                            )
                          ),
      year,
      indicator_code = "pc_EEL",
      indicator_name = "Percentage share of exports of embodied labour by sector of origin",
      value = round(pc_EEL * 100, 2),
      unit = "%"
    ) %>% 
    arrange(
      region_code, 
      year, 
      src_sector
    )
  
  
  df_fig2c <- df_fig2c %>% 
    mutate(
      value = case_when(
        !is.finite(value) ~ NA,
        is.na(value) ~ NA,
        TRUE ~ value
      )
    )
  
}

get_df_fig3 <- function(df_LF,
                        df_population,
                        df_social_outcomes,
                        year_y0,
                        df_regions){

  df_population_y0 <- df_population %>% 
    filter(year == year_y0) %>%
    transmute(
      region_code,
      region_name,
      year,
      population = value
    )
  
  df_LF_y0 <- df_LF %>% 
    filter(year == year_y0) %>% 
    right_join(df_regions) %>% 
    transmute(
      region_code,
      region_name,
      year,
      LF = value
    ) 
  
  df_LF_per_cap_y0 <- df_LF_y0 %>% 
    left_join(df_population_y0) %>% 
    transmute(
      region_code,
      region_name,
      year = as.character(year),
      indicator_code = "LF_per_cap",
      indicator_name = "Labour embodied in final demand",
      # 1 FTE = 40 hours/week
      value = round(LF / population / 1000 * 40, 1),
      unit = "hours/week/capita"
    )
  
    
  df_fig3 <- df_LF_per_cap_y0 %>% 
    bind_rows(df_social_outcomes)

}

get_df_fig4 <- function(df_IEL,
                        df_EEL,
                        df_LS,
                        df_population,
                        df_social_outcomes,
                        year_y0,
                        df_regions){
  
  df_LS_y0 <- df_LS %>%
    filter(year == year_y0) %>% 
    right_join(df_regions) %>% 
    transmute(
      region_code,
      region_name,
      year,
      LS = value
    )
  
  df_IEL_EEL_y0 <- df_IEL %>% 
    bind_rows(df_EEL) %>% 
    right_join(df_regions) %>% 
    filter(year == year_y0) %>%
    pivot_wider(
      names_from = "indicator",
      values_from = "value"
    ) %>% 
    select(-unit)
  
  df_ratio_netEEL_to_LS_y0 <- df_LS_y0 %>% 
    left_join(df_IEL_EEL_y0) %>% 
    mutate(ratio_netEEL_to_LS = (EEL - IEL) / LS) %>% 
    filter(ratio_netEEL_to_LS > 0) %>% 
    transmute(
      region_code,
      region_name,
      year = as.character(year),
      indicator_code = "ratio_netEEL_to_LS",
      indicator_name = "Net exports of embodied labour as percentage of domestic labour supply",
      value = round(ratio_netEEL_to_LS * 100, 2),
      unit = "%"
    )
  
  df_fig4 <- df_ratio_netEEL_to_LS_y0 %>% 
    bind_rows(df_social_outcomes)
    
}
