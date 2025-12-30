#======================================
# Job Polarization Study
# Using Microdata from Merged FIES-LFS
#=====================================

# LIBRARY AND PACKAGES -----

library(readxl)       # Importing data from Excel
library(tidyverse)    # Collection of packages for handling data
library(writexl)      # Save df as Excel workbooks
library(stats)        # Statistical calculations and random number generation
library(performance)  # Checking visually the model assumption
library(magick)       # Advanced image processing in R
library(survey)       # Package ideal for handling survey data, microdata
library(svyVGAM)      # Provides inference based on the survey package for the
# wide range of parametric models in the 'VGAM' package.
library(gtsummary)    # Pairwise comparisons
library(flextable)    # Saving tables
library(equatiomatic) # Extracting equations: use extract_eq
library(dplyr)        # A grammar of data manipulation


# Adjust variance by pretending the lonely PSU is centered at the
# population mean

options(survey.lonely.psu = "adjust")


# DIRECTORIES ----
raw_dir <- "../data/raw"         # Where raw data is stored
output_dir <- "../data/output"   # Where converted df is stored as csv and json


# DATA ----

# Merged FIES-LFS microdata ----

years <- c("2023", "2021","2018","2015","2012","2009","2006")


mdata_dir <- file.path(raw_dir, "FIES-LFS Merged Microdata")

mdata_files <- list(
  `2023_HH` = file.path(mdata_dir,
                        "PHL-PSA-FIES-LFS-2023-PUF",
                        "FIES-LFS PUF 2023 Household Summary.CSV"),
  `2023_MEM`= file.path(mdata_dir,
                        "PHL-PSA-FIES-LFS-2023-PUF",
                        "FIES-LFS PUF 2023 Household Members.CSV"),
  `2021_HH` = file.path(mdata_dir,
                        "PHL-PSA-FIES-LFS-2021-PUF",
                        "FIES-LFS PUF 2021 - HHLD_SUMMARY.CSV"),
  `2021_MEM`= file.path(mdata_dir,
                        "PHL-PSA-FIES-LFS-2021-PUF",
                        "FIES-LFS PUF 2021 - HHMEM.CSV"),
  `2018_HH` = file.path(mdata_dir,
                        "PHL-PSA-FIES-LFS-2018-PUF",
                        "FIES-LFS PUF 2018 Household Summary.CSV"),
  `2018_MEM` = file.path(mdata_dir,
                         "PHL-PSA-FIES-LFS-2018-PUF",
                         "FIES-LFS PUF 2018 Household Members.CSV"),
  `2015` = file.path(mdata_dir,
                     "PHL-PSA-FIES-LFS-2015-PUF",
                     "FIES-LFS PUF 2015 Merged.csv"),
  `2012` = file.path(mdata_dir,
                     "PHL-PSA-FIES-LFS-2012-PUF",
                     "FIES-LFS PUF 2012 Merged.csv"),
  `2009` = file.path(mdata_dir,
                     "PHL-PSA-FIES-LFS-2009-PUF",
                     "FIES-LFS PUF 2009 Merged.csv"),
  `2006_HH` = file.path(mdata_dir,
                        "PHL-PSA-FIES-LFS-2006-PUF",
                        "FIES-LFS PUF FIES 2006.CSV"),
  `2006_MEM` = file.path(mdata_dir,
                         "PHL-PSA-FIES-LFS-2006-PUF",
                         "FIES-LFS PUF LFS JAN 2007.CSV"))

mdata_df <- lapply(mdata_files, function(path) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
})

# Common names for columns
colmeta <- readxl::read_excel(file.path(mdata_dir, "FIES-LFS Columns.xlsx"))
colmeta <- colmeta %>% mutate(across(everything(), as.character))

col_maps <- lapply(years, function(y) {
  tmp <- colmeta %>%
    select(`Common Code`, all_of(y))
  %>% tidyr::drop_na()
  setNames(tmp$`Common Code`, tmp[[y]])
})

names(col_maps) <- years

# Function: Apply common names for columns

clean_df <- function(df, year) {
  mapping <- col_maps[[year]]
  cols_present <- intersect(names(df), names(mapping))
  
  rename_map <- setNames(cols_present, mapping[cols_present])
  
  df %>%
    select(all_of(cols_present)) %>%
    rename(!!!rename_map)
}


mdata_clean <- list()

for (key in names(mdata_df)) {
  year <- strsplit(key, "_")[[1]][1]
  mdata_clean[[key]] <- clean_df(mdata_df[[key]], year)
}

dtype_map <- list()

for (i in seq_len(nrow(colmeta))) {
  common <- colmeta$`Common Code`[i]
  dtype_raw <- tolower(trimws(colmeta$`Data Type`[i]))
  
  if (is.na(common) || is.na(dtype_raw)) next
  
  # Only map types you care about
  if (dtype_raw %in% c("string", "character")) {
    dtype_map[[common]] <- "character"
  } else if (dtype_raw %in% c("integer")) {
    dtype_map[[common]] <- "integer"
  } else if (dtype_raw %in% c("numeric", "double")) {
    dtype_map[[common]] <- "numeric"
  } else if (dtype_raw == "factor") {
    dtype_map[[common]] <- "factor"
  }
}

apply_dtypes <- function(df, dtype_map) {
  
  # Columns to pre-convert to numeric
  pre_numeric <- c("HHN", "PSU", "STR")
  
  for (col in names(dtype_map)) {
    if (!col %in% names(df)) next
    
    # Pre-convert if in pre_numeric
    if (col %in% pre_numeric) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
    
    # Apply final type from dtype_map
    dtype <- dtype_map[[col]]
    
    if (dtype == "factor") {
      df[[col]] <- as.factor(df[[col]])
    } else if (dtype == "integer") {
      df[[col]] <- suppressWarnings(as.integer(df[[col]]))
    } else if (dtype == "numeric") {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    } else if (dtype == "character") {
      df[[col]] <- as.character(df[[col]])
    }
  }
  df
}

for (key in names(mdata_clean)) {
  mdata_clean[[key]] <- apply_dtypes(mdata_clean[[key]], dtype_map)
}

attach_survey_fields <- function(hh_df, mem_df) {
  
  # Ensure HHN exists
  if (!("HHN" %in% names(hh_df))) stop("HHN missing in HH")
  if (!("HHN" %in% names(mem_df))) stop("HHN missing in MEM")
  
  # Convert HHN to numeric (strip leading zeros)
  hh_df$HHN  <- as.numeric(as.character(hh_df$HHN))
  mem_df$HHN <- as.numeric(as.character(mem_df$HHN))
  
  # Match HHN
  idx <- match(mem_df$HHN, hh_df$HHN)
  
  # Attach only STR and PSU from HH
  mem_df$STR <- hh_df$STR[idx]
  mem_df$PSU <- hh_df$PSU[idx]
  
  mem_df
}

years_with_split <- c("2006", "2018", "2021", "2023")

mdata_merged <- list()

for (year in years) {
  hh_key  <- paste0(year, "_HH")
  mem_key <- paste0(year, "_MEM")
  
  if (year %in% years_with_split) {
    hh_df  <- mdata_clean[[hh_key]]
    mem_df <- mdata_clean[[mem_key]]
    mdata_merged[[year]] <- attach_survey_fields(hh_df, mem_df)
    
  } else {
    # Already merged years like 2009/2012/2015
    mdata_merged[[year]] <- mdata_clean[[year]]
  }
}

# PSOC ----

psoc_file <- file.path(raw_dir, "PSOC", "PSOC_2012.xlsx")
sheets <- c("UNIT", "MINOR", "SUB-MAJOR", "MAJOR", "EDITIONS")
psoc_data <- setNames(
  lapply(sheets, function(sheet) {
    read_excel(psoc_file, sheet = sheet, col_types = "text")
  }),
  sheets
)

# Add unit columns to other sheets
psoc_unit <- psoc_data[["UNIT"]] %>% 
  dplyr::mutate(
    unit_code = Code,
    minor_code = str_sub(Code, 1, 3),
    sub_major_code = str_sub(Code, 1, 2),
    major_code = str_sub(Code, 1, 1)
  )

# Mapping of PSOC Editions
psoc_editions <- psoc_data[["EDITIONS"]]

map_1992_to_2012 <- setNames(psoc_editions[["2012_PSOC"]],
                             psoc_editions[["1992_PSOC"]])
map_2008_to_2012 <- setNames(psoc_editions[["2012_PSOC"]],
                             psoc_editions[["2008_PSOC"]])

# Simplified mapping
psoc_mapping_file <- file.path(raw_dir, "PSOC", "PSOC Mapping.xlsx")
sheets <- c("2006", "2009", "2012", "2015", "2018", "2021", "2023")
psoc_mapping <- setNames(
  lapply(sheets, function(sheet) {
    read_excel(psoc_mapping_file, sheet = sheet, col_types = "text")
  }),
  sheets
)

# PSIC ----

# PSIC Descriptions (Recent)
psic_file <- file.path(raw_dir, "PSIC", "PSIC_2009_2019 Updates.xlsx")
sheets <- c("Division", "Section", "Sector")
psic_data <- setNames(
  lapply(sheets, function(sheet) {
    read_excel(psic_file, sheet = sheet, col_types = "text")
  }),
  sheets
)

# Mapping of PSIC Editions
psic_mapping_file <- file.path(raw_dir, "PSIC", "PSIC mapping.xlsx")
sheets <- c("psic_old", "psic_new")
psic_mapping <- setNames(
  lapply(sheets, function(sheet) {
    read_excel(psic_mapping_file, sheet = sheet, col_types = "text")
  }),
  sheets
)

# Task mapping ----

task_mapping_file <- file.path(raw_dir, "Task Mapping", "task_mapping.xlsx")
sheets <- c("generalao", "tied", "added")
task_mapping <- setNames(
  lapply(sheets, function(sheet) {
    read_excel(task_mapping_file, sheet = sheet, col_types = "text")
  }),
  sheets
)

# Poverty threshold ----

poverty_threshold_file <- file.path(raw_dir, "Poverty threshold.xlsx")
poverty_threshold <- read_excel(poverty_threshold_file)

# CPI (2018 = 100) ----

cpi_file <- file.path(raw_dir, "CPI Annual.xlsx")
cpi <- read_excel(cpi_file)

# INCOME-CLASS CLASSIFICATION ----

# Household data
# Read the column metadata
hh_colnames_file <- file.path(raw_dir, "FIES-LFS Merged Microdata",
                              "Columns for Income Class.xlsx")

hh_colnames <- read_excel(hh_colnames_file) %>%
  mutate(across(everything(), as.character))

# Build dtype map
dtype_map_hh <- hh_colnames %>%
  filter(!is.na(`Common Code`), !is.na(`Data Type`)) %>%
  mutate(Data_Type = tolower(trimws(`Data Type`))) %>%
  select(`Common Code`, Data_Type) %>%
  deframe()

# Function: Select, rename columns for hh data and apply dtype
process_hh_df <- function(df, year, colmeta = hh_colnames, dtype_map = dtype_map_hh) {
  
  # 1. Mapping for this year
  mapping <- colmeta %>%
    select(`Common Code`, all_of(year)) %>%
    drop_na()
  
  # 2. Keep only columns that exist in df
  cols_present <- intersect(names(df), mapping[[year]])
  
  # 3. Select & rename safely
  df_clean <- df %>%
    select(all_of(cols_present)) %>%
    rename_with(~ mapping$`Common Code`[match(.x, mapping[[year]])], .cols = all_of(cols_present))
  
  # 4. Apply data types
  for (col in names(dtype_map)) {
    if (!col %in% names(df_clean)) next
    
    dtype <- dtype_map[[col]]
    df_clean[[col]] <- switch(dtype,
                              factor = as.factor(df_clean[[col]]),
                              integer = suppressWarnings(as.integer(df_clean[[col]])),
                              numeric = suppressWarnings(as.numeric(df_clean[[col]])),
                              character = as.character(df_clean[[col]]),
                              df_clean[[col]])
  }
  
  df_clean
}

mdata_hh <- list()

for (year in years) {
  hh_key <- if (year %in% years_with_split) paste0(year, "_HH") else year
  mdata_hh[[year]] <- process_hh_df(mdata_df[[hh_key]], year)
}

# Income class classification per household ----

classify_income <- function(hh_df, year, pov_thresh) {
  
  # Get per-capita poverty threshold for the year
  threshold <- pov_thresh %>% 
    filter(Year == year) %>% 
    pull(`Per capita`)
  
  if (length(threshold) == 0) stop(paste("Threshold not found for year", year))
  
  hh_df %>%
    # Compute per-capita income
    mutate(per_capita = TOINC / FSIZE) %>%
    # 3-level classification
    mutate(
      income_class_3 = factor(
        case_when(
          per_capita < 2 * threshold ~ "1_low",
          per_capita < 15 * threshold ~ "2_middle",
          TRUE ~ "3_upper"
        ),
        levels = c("1_low", "2_middle", "3_upper"),
        ordered = TRUE
      ),
      # 7-level classification
      income_class_7 = factor(
        case_when(
          per_capita < threshold ~ "1_poor",
          per_capita < 2 * threshold ~ "2_low_income",
          per_capita < 4 * threshold ~ "3_lower_mid",
          per_capita < 10 * threshold ~ "4_mid_mid",
          per_capita < 15 * threshold ~ "5_upper_mid",
          per_capita < 20 * threshold ~ "6_upper_upper",
          TRUE ~ "7_rich"
        ),
        levels = c("1_poor", "2_low_income", "3_lower_mid", "4_mid_mid", 
                   "5_upper_mid", "6_upper_upper", "7_rich"),
        ordered = TRUE
      )
    )
}


mdata_hh_classified <- lapply(names(mdata_hh), function(year) {
  classify_income(mdata_hh[[year]], as.numeric(year), poverty_threshold)
})

names(mdata_hh_classified) <- names(mdata_hh)


# DATA CLEAN-UP ----

# Select: Employed (only)

mdata_hhmem <- map(mdata_merged, ~ .x %>% filter(NEWEMPSTAT == 1))

mdata_hhmem <- imap(mdata_hhmem, function(df, year) {
  
  year <- as.character(year)
  
  # Get mapping for this year
  map_df <- psoc_mapping[[year]]
  
  # Column in mapping (Y2006, Y2009, etc.)
  year_col <- names(map_df)[1]
  
  # Create new PSOC_2012 column via match
  df$PSOC_2012 <- map_df$PSOC_2012[
    match(df$OCCUP, map_df[[year_col]])
  ]
  
  df
})

# Common referencing: PSIC ----

agri <- "A"
industry <- LETTERS[2:6] # B to F
services <- LETTERS[7:21] # G to U

add_psic_and_sector <- function(df) {
  df %>%
    mutate(
      econ_sector = case_when(
        PSIC_section %in% agri ~ "Agriculture",
        PSIC_section %in% industry ~ "Industry",
        PSIC_section %in% services ~ "Services",
        TRUE ~ NA_character_
      )
    )
}

mdata_hhmem <- imap(mdata_hhmem, function(df, year) {
  
  year <- as.character(year)
  
  # ---- 1. Clean PKB inside the pipeline ----
  df <- df %>%
    mutate(
      PKB_chr = as.character(PKB),
      PKB_clean = case_when(
        nchar(PKB_chr) == 3 ~ str_pad(PKB_chr, 4, pad = "0"),
        nchar(PKB_chr) == 5 ~ str_sub(PKB_chr, 2, 5),          
        TRUE ~ str_sub(PKB_chr, 1, 4)
      )
    )
  
  # ---- 2. Select correct PSIC mapping ----
  psic_map <- if (year %in% c("2006", "2009")) {
    psic_mapping$psic_old    
  } else {
    psic_mapping$psic_new    
  }
  
  # ---- 3. Map PSIC_section using match (instead of left_join) ----
  df$PSIC_section <- psic_map$Section[
    match(df$PKB_clean, psic_map$Class)
  ]
  
  # ---- 4. Add econ_sector ----
  df %>% add_psic_and_sector()
})

# Add: Major occupation group
# Also: Remove "0 -- Armed Forces Occupation"

mdata_hhmem <- map(mdata_hhmem, function(df) {
  df %>%
    # Remove Armed Forces (0110, 0210, 0310)
    filter(!PSOC_2012 %in% c("0110", "0210", "0310")) %>%
    
    # Extract major occupation group from PSOC_2012
    mutate(
      major_group = substr(PSOC_2012, 1, 1)
    )
})

# Add: Occupation classification

# Helper function to determine the job classification
determine_task_mapping <- function(df) {
  
  df %>%
    group_by(PSOC, task_type) %>% 
    summarise(n = n(), .groups = "drop") %>%
    group_by(PSOC) %>%
    mutate(prop = n / sum(n)) %>%
    select(-n) %>%
    pivot_wider(
      names_from = task_type,
      values_from = prop,
      values_fill = 0
    ) %>%
    mutate(across(c(`Non-routine cognitive`, `Non-routine manual`, 
                    `Routine cognitive`, `Routine manual`, `NA`), as.numeric)) %>%
    rename(
      NRC = `Non-routine cognitive`,
      NRM = `Non-routine manual`,
      RC  = `Routine cognitive`,
      RM  = `Routine manual`,
      `NA`  = `NA`
    ) %>%
    mutate(
      routine = RC + RM,
      manual  = NRM + RM
    ) %>%
    rowwise() %>%
    mutate(
      dt = {
        scores <- c(NRC, NRM, RC, RM, `NA`)
        labels <- c("NRC", "NRM", "RC", "RM", "NA")
        max_val <- max(scores, na.rm = TRUE)
        if (sum(scores == max_val, na.rm = TRUE) > 1) "TIE" else labels[which.max(scores)]
      }
    ) %>%
    ungroup() %>%
    mutate(PSOC_2012 = PSOC) %>%
    left_join(df %>% select(PSOC, title) %>% distinct(PSOC, .keep_all = TRUE),
              by = "PSOC") %>%
    select(PSOC_2012, title, NRC, NRM, RC, RM, `NA`, routine, manual, dt)
}

# Apply function
gen_map <- determine_task_mapping(task_mapping$generalao)

# For tied and added, they already have NRC, NRM, RC, RM, NA, routine, manual, dt
# Ensure same columns
tied_map <- task_mapping$tied %>%
  mutate(PSOC_2012 = PSOC) %>%
  select(PSOC_2012, title, NRC, NRM, RC, RM, `NA`, routine, manual, dt)

added_map <- task_mapping$added %>%
  mutate(PSOC_2012 = PSOC) %>%
  select(PSOC_2012, title, NRC, NRM, RC, RM, `NA`, routine, manual, dt)

# Combine all three
numeric_cols <- c("NRC", "NRM", "RC", "RM", "NA", "routine", "manual")
gen_map <- gen_map %>% mutate(across(all_of(numeric_cols), as.numeric))
tied_map <- tied_map %>% mutate(across(all_of(numeric_cols), as.numeric))
added_map <- added_map %>% mutate(across(all_of(numeric_cols), as.numeric))



# Remove duplicates (in case a PSOC exists in multiple sources)
occup_mapping <- bind_rows(tied_map, added_map, gen_map) %>%
  distinct(PSOC_2012, .keep_all = TRUE)

mdata_hhmem <- imap(mdata_hhmem, function(df, year) {
  df %>%
    left_join(
      occup_mapping %>% select(PSOC_2012, dt),
      by = "PSOC_2012"
    )
})

# Add: Income class classification

mdata_hhmem <- imap(mdata_hhmem, function(df, year) {
  
  hh_class <- mdata_hh_classified[[year]]
  
  # Ensure HHN is comparable
  df <- df %>% mutate(HHN = as.numeric(as.character(HHN)))
  hh_class <- hh_class %>% mutate(HHN = as.numeric(as.character(HHN)))
  
  # Use match to extract per row
  idx <- match(df$HHN, hh_class$HHN)
  
  df$per_capita      <- hh_class$per_capita[idx]
  df$income_class_3  <- hh_class$income_class_3[idx]
  df$income_class_7  <- hh_class$income_class_7[idx]
  
  df
})


# Survey Design
# Transforming the microdata as survey design objects using `survey` package.

for (yr in names(mdata_hhmem)) {
  df <- mdata_hhmem[[yr]] %>% 
    
    # Remove NAs
    filter(!is.na(PSU) & !is.na(STR) & !is.na(FWGT))  
  
  sd_obj <- svydesign(
    id = ~PSU,
    strata = ~STR,
    weights = ~FWGT,
    data = df,
    nest = TRUE
  )
  
  assign(paste0("sd_", yr), sd_obj, envir = .GlobalEnv)
  
  # Remove df to free memory
  rm(df)
  gc()
}







# FUNCTIONS: Data processing ----

# Extract employed subset
get_emp <- function(sd) {
  # subset employed individuals
  subset(sd, NEWEMPSTAT == 1)
}


# Total employed
calc_total_emp <- function(emp) {
  st <- svytotal(~NEWEMPSTAT, emp, na.rm = TRUE)
  
  data.frame(
    variable  = names(st),
    estimate  = as.numeric(st),
    se        = sqrt(attr(st, "var")),
    statistic = attr(st, "statistic"),
    stringsAsFactors = FALSE
  )
}


calc_nat_wage <- function(emp) {
  st <- svymean(~PBASIC, subset(emp, NEWEMPSTAT == 1), na.rm = TRUE)
  data.frame(
    variable = names(st),
    estimate = as.numeric(st),
    se       = sqrt(attr(st, "var")),
    stringsAsFactors = FALSE
  )
}


# Employment by 4-digit PSOC
calc_job_4digit <- function(emp, total_emp_value) {
  svyby(~NEWEMPSTAT, ~PSOC_2012, emp, svytotal, na.rm = TRUE) %>%
    as.data.frame() %>%
    mutate(share = NEWEMPSTAT / total_emp_value * 100)
}


# Dominant task employment
calc_dt <- function(emp) {
  df <- svyby(~NEWEMPSTAT, ~dt, emp, svytotal, na.rm = TRUE)
  as.data.frame(df)
}


# Major occupational groups employment
calc_major_emp <- function(emp) {
  df <- svyby(~NEWEMPSTAT, ~major_group, emp, svytotal, na.rm = TRUE)
  as.data.frame(df)
}


# Average wage by occupation (4-digit)
calc_wage_4digit <- function(emp) {
  df <- svyby(~PBASIC, ~PSOC_2012, emp, svymean, na.rm = TRUE)
  as.data.frame(df)
}


calc_wage_ind <- function(emp) {
  df <- svyby(~PBASIC, ~PSOC_2012 + PSIC_section, emp, svymean, na.rm = TRUE)
  as.data.frame(df)
}


# Average wage by dominant task
calc_dt_wage <- function(emp) {
  df <- svyby(~PBASIC, ~dt, emp, svymean, na.rm = TRUE)
  as.data.frame(df)
}


# Average wage by major group
calc_major_wage <- function(emp) {
  df <- svyby(~PBASIC, ~major_group, emp, svymean, na.rm = TRUE)
  as.data.frame(df)
}


calc_ind <- function(emp) {
  df <- svyby(~NEWEMPSTAT, ~~PSOC_2012 + PSIC_section, emp, svytotal, na.rm = TRUE)
  as.data.frame(df) %>%
    
    group_by(PSIC_section) %>%
    mutate(
      total_by_industry = sum(NEWEMPSTAT),
      pct = NEWEMPSTAT / total_by_industry * 100
    ) %>%
    ungroup()
}


# Employment: dominant task × industry section
calc_dti <- function(emp) {
  df <- svyby(~NEWEMPSTAT, ~PSIC_section + dt, emp, svytotal, na.rm = TRUE)
  as.data.frame(df)
}

# Employment: dominant task × income class
calc_dt_income <- function(emp, total_emp) {
  total_emp_value <- total_emp$estimate
  
  svyby(~NEWEMPSTAT, ~dt + income_class_3, emp, svytotal, na.rm = TRUE) %>%
    as.data.frame() %>%
    group_by(income_class_3) %>%
    mutate(
      total_by_income = sum(NEWEMPSTAT),
      pct = NEWEMPSTAT / total_by_income * 100
    ) %>%
    ungroup() %>%
    mutate(pct_of_total_employed = NEWEMPSTAT / total_emp_value * 100)
}


# Employment: dominant task × major occupation group
calc_dt_major <- function(emp, total_emp) {
  total_emp_value <- total_emp$estimate
  
  svyby(~NEWEMPSTAT, ~dt + major_group, emp, svytotal, na.rm = TRUE) %>%
    as.data.frame() %>%
    group_by(major_group) %>%
    mutate(
      total_by_group = sum(NEWEMPSTAT),
      pct = NEWEMPSTAT / total_by_group * 100
    ) %>%
    ungroup() %>%
    mutate(pct_of_total_employed = NEWEMPSTAT / total_emp_value * 100)
}


# Employment: major group × industry section
calc_maj_ind <- function(emp) {
  total_emp_value <- total_emp$estimage
  
  svyby(~NEWEMPSTAT, ~major_group + PSIC_section, emp, svytotal, na.rm = TRUE) %>%
    as.data.frame() %>%
    group_by(PSIC_section) %>%
    mutate(
      total_by_section = sum(NEWEMPSTAT),
      pct = NEWEMPSTAT / total_by_section * 100
    ) %>%
    ungroup() %>%
    mutate(pct_of_total_employed = NEWEMPSTAT / total_emp_value * 100)
}


# Average wage: dominant task × industry section
calc_dti_wage <- function(emp) {
  df <- svyby(~PBASIC, ~PSIC_section + dt, emp, svymean, na.rm = TRUE)
  as.data.frame(df)
}


# Average wage: dominant task × income class
calc_dt_income_wage <- function(emp) {
  df <- svyby(~PBASIC, ~dt + income_class_3, emp, svymean, na.rm = TRUE)
  as.data.frame(df)
}


# Create sd (survey design) subsets: Employed

emp_list <- list()

for (yr in years) {
  
  message("Processing ", yr, " ...")
  
  sd <- get(paste0("sd_", yr))
  
  emp <- subset(sd, NEWEMPSTAT == 1 & !is.na(major_group))
  
  emp_list[[yr]] <- emp
  
  rm(emp)
  rm(sd)
  gc()
}


# APPLY the functions ----


## Total number of employed
a_emp_total <- list()

for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  a_emp_total[[yr]] <- calc_total_emp(emp)
}


a_wage_nat <- list()

for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  a_wage_nat[[yr]] <- calc_nat_wage(emp)
}


## Total number of employed: By 4-digit job code
a_emp_job_4digit <- list()

for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  # Extract numeric total from a_emp_total data.frame
  total_emp_value <- a_emp_total[[yr]]$estimate
  
  a_emp_job_4digit[[yr]] <- calc_job_4digit(emp, total_emp_value)
}


## Total number of employed: By dominant task categories
a_emp_dt <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_emp_dt[[yr]] <- calc_dt(emp)
}


## Total number of employed: By major occupation groups
a_emp_major <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_emp_major[[yr]] <- calc_major_emp(emp)
}


## Average wages: By 4-digit job codes
a_wage_4digit <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_wage_4digit[[yr]] <- calc_wage_4digit(emp)
}

a_wage_ind <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_wage_ind[[yr]] <- calc_wage_ind(emp)
}


## Average wages: By major occupation groups
a_wage_major <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_wage_major[[yr]] <- calc_major_wage(emp)
}

a_emp_ind <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_emp_ind[[yr]] <- calc_ind(emp)
}


## Total number of employed: Dominant task × industry section
a_emp_dti <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_emp_dti[[yr]] <- calc_dti(emp)
}


## Total number of employed: Dominant task × income class
a_emp_dt_income <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  total_emp <- a_emp_total[[yr]]
  
  a_emp_dt_income[[yr]] <- calc_dt_income(emp, total_emp)
}


## Total number of employed: Major group × industry section
a_emp_major_ind <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_emp_major_ind[[yr]] <- calc_maj_ind(emp)
}


## Average wages: By dominant task
a_wage_dt <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_wage_dt[[yr]] <- calc_dt_wage(emp)
}


## Total number of employed: Dominant task × major occupation group
a_emp_dt_major <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  total_emp <- a_emp_total[[yr]]
  
  a_emp_dt_major[[yr]] <- calc_dt_major(emp, total_emp)
}


## Average wages: Dominant task × industry section
a_wage_dti <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_wage_dti[[yr]] <- calc_dti_wage(emp)
}


## Average wages: Dominant task × income class
a_wage_dt_income <- list()
for (yr in names(emp_list)) {
  emp <- emp_list[[yr]]
  
  a_wage_dt_income[[yr]] <- calc_dt_income_wage(emp)
}

rm(emp)


# COMBINE into single dfs ----


df_analysis <- c(
  "a_emp_total",         # Total number of employed per year
  "a_emp_job_4digit",    # Total employed per 4-digit occup
  "a_emp_dt",            # Total employed per dominant task classification
  "a_emp_major",         # Total employed per major occupation group
  "a_wage_4digit",
  "a_wage_major",
  "a_emp_dti",
  "a_emp_dt_income",
  "a_wage_dt",
  "a_emp_dt_major",
  "a_wage_dti",
  "a_wage_dt_income",
  "a_emp_major_ind",
  "a_emp_ind",
  "a_wage_ind",
  "a_wage_nat"
)

analyses <- mget(df_analysis)


combine_analysis <- function(df_analysis) {
  
  for (df_name in names(df_analysis)) {
    
    message("Combining: ", df_name)
    
    # Remove the "a_" prefix → make c_ version
    new_name <- paste0("c_", sub("^a_", "", df_name))
    
    nested_list <- df_analysis[[df_name]]
    
    # Combine into one long df
    combined_df <- dplyr::bind_rows(nested_list, .id = "year")
    
    # Ensure year is character
    combined_df$year <- as.character(combined_df$year)
    
    # Remove row names
    rownames(combined_df) <- NULL
    
    # Save into global environment
    assign(new_name, combined_df, envir = .GlobalEnv)
  }
}







# TRENDS ----


## Total employment trends: t_emp_total

t_emp_total <- c_emp_total %>%
  select(-NEWEMPSTAT) %>%                 # remove NEWEMPSTAT column
  arrange(as.numeric(year)) %>%           # ensure sorted by year
  mutate(
    perc_g = (estimate / lag(estimate) - 1) * 100,   # percent growth
    index_g = estimate / estimate[year == "2006"] * 100
  )


## Total employement trends by 4-digit code: t_emp_job_4digit

t_emp_job_4digit <- c_emp_job_4digit %>%
  rename(total = NEWEMPSTAT) %>%
  arrange(as.numeric(year)) %>%
  group_by(PSOC_2012) %>%
  mutate(
    perc_g = (total / lag(total) - 1) * 100, # percent growth
    index_g = ifelse(any(year == "2006"), total / first(total[year == "2006"]) * 100, NA), # indexed growth
    share_g = share - lag(share)
  ) %>%
  ungroup()


s_emp_job_4digit <- t_emp_job_4digit %>%
  select(year, PSOC_2012, total, share) %>%
  filter(year %in% c("2006", "2023")) %>%
  pivot_wider(
    names_from = year,
    values_from = c(total, share),
    names_glue = "{.value}_{year}"
  ) %>%
  mutate(
    perc_g = (total_2023 / total_2006 - 1) * 100,
    cagr = ((total_2023 / total_2006)^(1 / (2023 - 2006)) - 1) * 100,
    change_g_share = share_2023 - share_2006,
  )


## Total employement trends by dominant task: t_emp_dt

t_emp_dt <- c_emp_dt %>%
  rename(total = NEWEMPSTAT) %>%
  group_by(year) %>%
  mutate(
    year_total = sum(total)
  ) %>%
  ungroup() %>%
  mutate(
    share = (total / year_total) * 100
  ) %>%
  arrange(as.numeric(year)) %>%
  group_by(dt) %>%
  mutate(
    perc_g = (total / lag(total) - 1) * 100,
    index_g = total / (total[year == "2006"]) * 100,
    share_g = share - lag(share)
  ) %>%
  ungroup()


s_emp_dt <- t_emp_dt %>%
  select(year, dt, total, share) %>%
  filter(year %in% c("2006", "2023")) %>%
  pivot_wider(
    names_from = year,
    values_from = c(total, share),
    names_glue = "{.value}_{year}"
  ) %>%
  mutate(
    perc_g = (total_2023 / total_2006 - 1) * 100,
    cagr = ((total_2023 / total_2006)^(1 / (2023 - 2006)) - 1) * 100,
    change_g_share = share_2023 - share_2006,
  )








