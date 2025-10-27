

get_list_labour_trade_y <- function(df_D,
                                    year_y,
                                    list_regions_excluded){
  
  df_IEL <- df_D %>%
    filter(!(dest_region %in% list_regions_excluded)) %>% 
    group_by(dest_region) %>%
    filter(src_region != dest_region) %>% 
    summarize(value = sum(value)) %>% 
    ungroup() %>% 
    transmute(
      region_code = dest_region,
      year = year_y,
      indicator = "IEL",
      value,
      unit = "Full-Time-Equivalent"
    )

  df_EEL <- df_D %>%
    filter(!(src_region %in% list_regions_excluded)) %>% 
    group_by(src_region) %>% 
    filter(dest_region != src_region) %>% 
    summarize(value = sum(value)) %>% 
    ungroup() %>% 
    transmute(
      region_code = src_region,
      year = year_y,
      indicator = "EEL",
      value,
      unit = "Full-Time-Equivalent"
    )

  df_LF <- df_D %>%
    filter(!(dest_region %in% list_regions_excluded)) %>% 
    group_by(dest_region) %>%
    summarize(value = sum(value)) %>% 
    ungroup() %>% 
    transmute(
      region_code = dest_region,
      year = year_y,
      indicator = "LF",
      value,
      unit = "Full-Time-Equivalent"
    )

  
  df_LS <- df_D %>%
    filter(!(src_region %in% list_regions_excluded)) %>% 
    group_by(src_region) %>%
    summarize(value = sum(value)) %>% 
    ungroup() %>% 
    transmute(
      region_code = src_region,
      year = year_y,
      indicator = "LS",
      value,
      unit = "Full-Time-Equivalent"
    )
  
  
  df_EEL_by_src_sector <- df_D %>%
    filter(!(src_region %in% list_regions_excluded)) %>% 
    group_by(src_region, src_sector) %>% 
    filter(dest_region != src_region) %>% 
    summarize(value = sum(value)) %>% 
    ungroup() %>% 
    transmute(
      region_code = src_region,
      src_sector,
      year = year_y,
      indicator = "EEL",
      value,
      unit = "Full-Time-Equivalent"
    )
  
  list_labour_trade <- list(
    df_IEL = df_IEL,
    df_EEL = df_EEL,
    df_LF = df_LF,
    df_LS = df_LS,
    df_EEL_by_src_sector = df_EEL_by_src_sector
  )
  
}