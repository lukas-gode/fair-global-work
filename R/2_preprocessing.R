

tidy_gloria_y <- function(path_gloria,
                          year_y){
  
  if(!dir.exists(paste0(path_gloria, "tidy/", year_y))){
    
    dir.create(paste0(path_gloria, "tidy/", year_y))
    
    ## Parameters for extraction of MRIO objects from the MR-SUT tables
    nb_regions <- 164
    nb_sectors <- 120
    nb_final_demand_cat <- 6
    
    cols_ixi <- c()
    for (r in seq(0, nb_regions - 1)){
      cols_ixi <- c(
        cols_ixi,
        seq(2 * r * nb_sectors + 1, 
            2 * r * nb_sectors + nb_sectors
        )
      )
    }
    
    rows_ixi <- c()
    for (r in seq(0, nb_regions - 1)) {
      rows_ixi <- c(
        rows_ixi,
        seq(2 * r * nb_sectors + nb_sectors + 1, 
            2 * r * nb_sectors + 2 * nb_sectors
        ) 
      )
    }
    
    rows_emp <- read_excel(
      path = paste0(path_gloria, "raw/GLORIA_ReadMe_059a.xlsx"),
      sheet = "Satellites"
    ) %>%
      filter(Sat_head_indicator == "Employment") %>%
      pull(Lfd_Nr)
    
    ## Vector of final demand (Y)
    mat_Y_raw <- fread(
      file = list.files(
        path = paste0(
          path_gloria, 
          "raw/GLORIA_MRIOs_59_",
          year_y
        ),
        pattern = paste0(
          "120secMother_AllCountries_002_Y-Results_",
          year_y, 
          "_059_Markup001"
        ),
        full.names = TRUE)[1],
      header = F,
      colClasses = "numeric"
    )
    
    mat_Y_raw <- mat_Y_raw[c(rows_ixi), ]
    mat_Y_raw[is.na(mat_Y_raw)] <- 0
    mat_Y_raw <- mapply(mat_Y_raw, FUN = as.numeric)
    
    mat_Y <- matrix(0, 
                    nrow = nb_regions * nb_sectors, 
                    ncol = nb_regions
    )
    
    for(i in 1:nb_regions) {
      mat_Y[, i] <- rowSums(
        mat_Y_raw[, seq(
          (i - 1) * nb_final_demand_cat + 1,
          (i - 1) * nb_final_demand_cat + nb_final_demand_cat)]
      )
    }
    
    rm(mat_Y_raw)
    
    
    ## Matrix of inter-industry transactions (Z)
    mat_Z <- fread(
      file = list.files(
        path = paste0(
          path_gloria, 
          "raw/GLORIA_MRIOs_59_",
          year_y
        ),
        pattern = paste0(
          "120secMother_AllCountries_002_T-Results_",
          year_y, 
          "_059_Markup001"
        ),
        full.names = TRUE)[1], 
      header = F,
      select = cols_ixi,
      colClasses = "numeric"
    )
    
    mat_Z <- mat_Z[c(rows_ixi), ]
    mat_Z[is.na(mat_Z)] <- 0
    
    mat_Z <- mapply(mat_Z, FUN = as.numeric)
    
    
    # Vector of gross outputs (X)
    mat_X <- matrix(
      rowSums(mat_Z) + rowSums(mat_Y), 
      ncol = 1
    )
    
    # Matrix of inter-industry coefficients (A)
    mat_A <- sweep(mat_Z, 2, mat_X, "/")
    rm(mat_Z)
    mat_A[is.na(mat_A)] <- 0
    
    
    # Leontief Inverse (L)
    np <- import("numpy")
    mat_L <- np$linalg$inv(diag(nrow(mat_A)) - mat_A)
    rm(mat_A)
    
    # Vector of labour inputs (H)
    mat_emp <- fread(
      file = list.files(
        path = paste0(
          path_gloria,
          "raw/GLORIA_SatelliteAccounts_059_",
          year_y
        ),
        pattern = paste0(
          "120secMother_AllCountries_002_TQ-Results_",
          year_y,
          "_059_Markup001"
        ),
        full.names = TRUE)[1],
      select = cols_ixi,
      header = F,
      skip = min(rows_emp) - 1,
      nrows = length(rows_emp)
    )
    
    mat_H <- matrix(
      colSums(mat_emp),
      nrow = 1
    )
    
    # Vector of labour inputs coefficients (Hc)
    
    mat_Hc <- mat_H / t(mat_X)
    mat_Hc[is.na(mat_Hc)] <- 0
    
    # Save files
    saveRDS(mat_H,
            paste0(path_gloria, "tidy/", year_y, "/H.rds"))
    
    saveRDS(mat_Hc,
            paste0(path_gloria, "tidy/", year_y, "/Hc.rds"))
    
    saveRDS(mat_Y,
            paste0(path_gloria, "tidy/", year_y, "/Y.rds"))
    
    saveRDS(mat_L,
            paste0(path_gloria, "tidy/", year_y, "/L.rds"))
    
  }
  
  return(TRUE)
  
  
}




get_df_D_y <- function(path_gloria,
                       df_row_items, 
                       year_y){
  
  df_regions <- df_row_items %>%
    select(region_code,
           region_name) %>%
    distinct()
  
  mat_L <- readRDS(paste0(path_gloria, "tidy/", year_y, "/L.rds"))
  mat_Y <- readRDS(paste0(path_gloria, "tidy/", year_y, "/Y.rds"))
  mat_Hc <- readRDS(paste0(path_gloria, "tidy/", year_y, "/Hc.rds"))
  
  mat_D <- sweep(mat_L %*% mat_Y, 1, mat_Hc, "*")
  colnames(mat_D) <- df_regions$region_code
  
  df_D <- df_row_items %>% 
    select(
      src_region = region_code,
      src_sector = sector_name
    ) %>% 
    cbind(mat_D) %>%
    pivot_longer(
      cols = -c(src_region, src_sector),
      names_to = "dest_region",
      values_to = "value"
    )
  
}
