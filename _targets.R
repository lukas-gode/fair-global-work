# Description: 
# This script contains the pipeline used to generate the empirical results presented in:
# "Unequal exchange of labour and global justice: Principles for a fair international distribution of work"

# Instructions to reproduce the results can be found in the README file attached.

# Install packages ------------------------------------------------------------------

# # You can install the packages required to execute the pipeline
# # by un-commenting  and executing the following lines:
# 
# if(!require("pacman")){install.packages("pacman")}
# pacman::p_load(
#   # Targets pipeline
#   here, readxl, readODS, readr,
#   # wrangle data
#   dplyr, data.table, stringr, tidyr,
#   # execute .py scripts
#   reticulate
#   )

# ------------------------------------------------------------------------------------

# Load (and install if necessary) required packages
if(!require("pacman")){install.packages("pacman")}
pacman::p_load(reticulate, here, tarchetypes, targets, qs)

# Set main options of the "targets" pipeline
tar_option_set(
  format = "qs",
  packages = c(
    # read data
    "here", "readxl", "readODS", "readr",
    # wrangle data
    "dplyr", "data.table", "stringr", "tidyr",
    # execute .py scripts
    "reticulate"
    )
  )

# Load functions used to generate the results
tar_source("R")

# Create output directory
if(!dir.exists(here("outputs"))) dir.create(here("outputs"))

# Parameters
parameters <- tar_plan(
  
  # Folder where the GLORIA dataset is stored, must be manually set
  path_gloria = "../MRIO_dataset/Gloria_059a/", 
  list_years = seq(1990, 2018),
  year_y0 = 2018,
  # Regions not considered in the analysis
  list_regions_excluded = c("XAM", "XEU", "XAF",
                            "XAS", "PRK", "MDA",
                            "TKM", "GNQ", "BLR",
                            "DYE")
  
)

# Trace local files
files <- tar_plan(
  
  tar_file(file_population,
           here("data", "external", "UN_population.ods")),
  tar_file(file_life_expectancy,
           here("data", "external", "WHO_life_expectancy.ods")),
  tar_file(file_social_thresholds,
           here("data", "external", "FanningEtAl_social_thresholds.ods")),
  tar_file(file_life_evaluation,
           here("data", "external", "Helliwell_life_evaluation.ods")),
  tar_file(file_concordances,
           here("data", "own", "concordances.xlsx"))
  
)

# Import and wrangle local files
wrangling <- tar_plan(
  
  tar_target(df_row_items,
             get_df_row_items(path_gloria)
             ),
  
  tar_target(df_regions,
             get_df_regions(df_row_items,
                            list_regions_excluded)
             ),
  
  tar_target(df_population,
             get_df_population(file_population,
                               df_regions)
             ),
  
  tar_target(df_social_outcomes,
             get_df_social_outcomes(file_life_expectancy,
                                    file_social_thresholds,
                                    file_life_evaluation,
                                    df_regions)
             ),
  
  tar_target(df_agg_sectors,
             get_df_agg_sectors(file_concordances)
             )
  

  )

# Preprocess MRIO objects from the GLORIA dataset
preprocessing <- tar_plan(

  tar_target(is_tidy_gloria_y,
             tidy_gloria_y(path_gloria,
                           list_years),
             pattern = map(list_years)
             ),
  
  tar_target(df_D_y,
             get_df_D_y(path_gloria,
                        df_row_items,
                        list_years),
             pattern = map(list_years)
  )

)

# Compute the labour embodied in trade
processing <- tar_plan(

  # Compute all results (branched on yearly values)
  tar_target(list_labour_trade_y,
             get_list_labour_trade_y(df_D_y,
                                     list_years,
                                     list_regions_excluded),
             pattern = map(df_D_y,
                           list_years)),
  
  # EEL: Exports of embodied labour
  tar_target(df_EEL_y,
             list_labour_trade_y$df_EEL,
             pattern = map(list_labour_trade_y)
             ),
  
  # IEL: Imports of emboided labour
  tar_target(df_IEL_y,
             list_labour_trade_y$df_IEL,
             pattern = map(list_labour_trade_y)
  ),
  
  # LF: Labour embodied in final demand / Labour footprint
  tar_target(df_LF_y,
             list_labour_trade_y$df_LF,
             pattern = map(list_labour_trade_y)
  ),
  
  # LS: Labour supply
  tar_target(df_LS_y,
             list_labour_trade_y$df_LS,
             pattern = map(list_labour_trade_y)
  ),
  
  # EEL_by_src_sector: Exports of embodied labour, by source sector
  tar_target(df_EEL_by_src_sector_y,
             list_labour_trade_y$df_EEL_by_src_sector,
             pattern = map(list_labour_trade_y)
  ),
  
  tar_target(df_EEL, df_EEL_y),
  tar_target(df_IEL, df_IEL_y),
  tar_target(df_LF, df_LF_y),
  tar_target(df_LS, df_LS_y),
  tar_target(df_EEL_by_src_sector, df_EEL_by_src_sector_y)

)

# Compile data for the figures of the article
figures <- tar_plan(

  tar_target(df_fig1a,
             get_df_fig1a(df_IEL,
                          df_EEL,
                          year_y0,
                          df_regions)
             ),
  
  tar_target(df_fig1b,
             get_df_fig1b(df_fig1a,
                          df_population)),

  tar_target(df_fig2a,
             get_df_fig2a(df_IEL,
                          df_EEL,
                          df_regions)
  ),
  
  tar_target(df_fig2b,
             get_df_fig2b(df_EEL_by_src_sector,
                          df_LS,
                          df_regions)
  ),
  
  tar_target(df_fig2c,
             get_df_fig2c(df_EEL_by_src_sector,
                          df_agg_sectors,
                          df_regions)
  ),
  
  tar_target(df_fig3,
             get_df_fig3(df_LF,
                         df_population,
                         df_social_outcomes,
                         year_y0,
                         df_regions)
             ),

  tar_target(df_fig4,
             get_df_fig4(df_IEL,
                         df_EEL,
                         df_LS,
                         df_population,
                         df_social_outcomes,
                         year_y0,
                         df_regions)
             ),
  
  ods_data_fig = write_ods(
    list(
      "Figure 1a" = df_fig1a,
      "Figure 1b" = df_fig1b,
      "Figure 2a" = df_fig2a,
      "Figure 2b" = df_fig2b,
      "Figure 2c" = df_fig2c,
      "Figure 3" = df_fig3,
      "Figure 4" = df_fig4
    ),
    path = here("outputs", "SupplementaryMaterials2.ods"),
    na_as_string = T
  )

)

list(
  
  # Parameters
  parameters,
  # Trace local files
  files,
  #------------------------
  # 1. Import and wrangle local files
  wrangling,
  # 2. Preprocess MRIO objects from the GLORIA dataset
  preprocessing,
  # 3. Compute the labour embodied in trade
  processing,
  # 4. Compile data for the figures of the article
  figures
  
)
