# --------------------------------------------------------
# 1. Install and Load Required Packages
# --------------------------------------------------------
install.packages(c("httr", "jsonlite", "dplyr", "stringr", "writexl"))
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(writexl)
install.packages("purrr")
library(purrr)

if (!require("gtrendsR")) install.packages("gtrendsR")
if (!require("tidyverse")) install.packages("tidyverse")
library(gtrendsR)
library(tidyverse)

# --------------------------------------------------------
# 2. Define Keywords Lists(k_culture, hansik)
# --------------------------------------------------------
k_culture <- c(
  "K-pop", "BTS", "Blackpink", "Twice", "Enhyphen", "Stray Kids", "aespa", "Seventeen", "Psy", "Bigbang", "GD", "IU", "2NE1",
  "K-drama", "When Life Gives You Tangerines", "Squid Game", "The Glory", "Crash Landing on You", "Itaewon Class", "Extraordinary Attorney Woo",
  "K-movie", "Korean movie", "Parasite", "Kpop Demon Hunters", "Train to Busan",
  "Korean culture", "K-culture", "Korean wave", "Hallyu"
)
hansik <- c(
  "kimchi", "bulgogi", "tteokbokki", "K-food", "Korean food", "Korean fried chicken", "buldak", "bibimbap", "gochujang", "dakgalbi",
  "kimbap", "korean cuisine", "korean street food", "KBBQ", "Korean BBQ", "japchae", "jjigae", "samgyeopsal", "bingsu", "hansik",
  "soju", "makgeolli", "samgyetang", "kimchi stew", "pajeon", "mandu", "yangnyeom chicken", "Korean restaurant"
)

# --------------------------------------------------------
# 3. Set Analysis Parameters
# --------------------------------------------------------
geo <- "IE"   # Country code (US, GB, CA, AU, NZ, IE)
time_span <- "2015-01-01 2025-07-25" 
gprop <- "web" # Search property: "web", "news", or "youtube"

# --------------------------------------------------------
# 4. Split Keywords into Groups of 5 (API limitation)
# --------------------------------------------------------
split_keywords <- function(keywords) split(keywords, ceiling(seq_along(keywords)/5))
k_culture_list <- split_keywords(k_culture)
hansik_list <- split_keywords(hansik)

# --------------------------------------------------------
# 5. Define Function to Retrieve Google Trends Data
# --------------------------------------------------------
get_trends <- function(keyword_group, geo, time_span, gprop) {
  gtrends(keyword = keyword_group, geo = geo, time = time_span, gprop = gprop)$interest_over_time
}

# --------------------------------------------------------
# 6. Retrieve Data for All Keyword Groups (tryCatch)
# --------------------------------------------------------
fetch_gtrends <- function(kw_list, label = "keyword") {
  all_data <- tibble()
  failed_groups <- c()
  for (i in seq_along(kw_list)) {
    Sys.sleep(30) # avoiding 429 error
    tryCatch({
      temp <- get_trends(kw_list[[i]], geo, time_span, gprop)
      temp$hits <- as.character(temp$hits) 
      all_data <- bind_rows(all_data, temp)
      cat(sprintf("[%s] Group %d: Success\n", label, i))
    }, error = function(e) {
      cat(sprintf("[%s] Group %d: Failed - %s\n", label, i, conditionMessage(e)))
      failed_groups <- c(failed_groups, i)
    })
  }
  list(data = all_data, failed = failed_groups)
}

# --------------------------------------------------------
# 7. Collect Data for K-culture and Hansik Keywords
# --------------------------------------------------------
result_k_culture <- fetch_gtrends(k_culture_list, "K-culture")
all_k_culture_data <- result_k_culture$data
failed_k_culture <- result_k_culture$failed

result_hansik <- fetch_gtrends(hansik_list, "Hansik")
all_hansik_data <- result_hansik$data
failed_hansik <- result_hansik$failed

# --------------------------------------------------------
# 8. Save Data (.csv)
# --------------------------------------------------------
write_csv(all_k_culture_data, "C:/Users/nicol/OneDrive/Desktop/Univ/Out-campus activities/인사이트/2025-7/k_culture_gtrend_IE.csv")
write_csv(all_hansik_data, "C:/Users/nicol/OneDrive/Desktop/Univ/Out-campus activities/인사이트/2025-7/hansik_gtrend_IE.csv")

# --------------------------------------------------------
# 9. Sum/aggregate by 2-year intervals
# --------------------------------------------------------

# --------------------------------------------------------
# 9 - 1. Load Required Packages
# --------------------------------------------------------
library(dplyr)
library(readr)
library(stringr)

# --------------------------------------------------------
# 9 - 2. Define Country Codes and File Path Pattern
# --------------------------------------------------------
geo_list <- c("US", "GB", "CA", "AU", "NZ", "IE")
base_dir <- "C:/Users/nicol/OneDrive/Desktop/Univ/Out-campus activities/인사이트/2025-7"

# --------------------------------------------------------
# 9 - 3. Helper: 2-Year Interval Label Function
# --------------------------------------------------------
get_2yr_interval <- function(date_col) {
  year <- as.numeric(format(as.Date(date_col), "%Y"))
  interval <- paste0(year - (year %% 2), "-", year - (year %% 2) + 1)
  return(interval)
}

# --------------------------------------------------------
# 9 - 4. Main Loop: Load, Process, Save All Countries (No manual file path input needed)
# --------------------------------------------------------
for (geo in geo_list) {
  # Auto-create file paths
  kfile <- file.path(base_dir, sprintf("k_culture_gtrend_%s.csv", geo))
  hfile <- file.path(base_dir, sprintf("hansik_gtrend_%s.csv", geo))
  
  # Try reading files (skip if missing)
  if (!file.exists(kfile) | !file.exists(hfile)) {
    cat(sprintf("Warning: Missing file for %s. Skipping...\n", geo))
    next
  }
  
  kdata <- read_csv(kfile, show_col_types = FALSE)
  hdata <- read_csv(hfile, show_col_types = FALSE)
  
  # After loading raw data (kdata, hdata):
  kdata$hits <- as.numeric(gsub("<1", "0.5", kdata$hits))
  hdata$hits <- as.numeric(gsub("<1", "0.5", hdata$hits))
  
  
  # Add 2-year interval
  kdata <- kdata %>% mutate(interval_2yr = get_2yr_interval(date))
  hdata <- hdata %>% mutate(interval_2yr = get_2yr_interval(date))
  
  # Aggregate
  k2yr <- kdata %>%
    group_by(keyword, interval_2yr) %>%
    summarise(
      sum_hits = sum(as.numeric(hits), na.rm = TRUE),
      mean_hits = mean(as.numeric(hits), na.rm = TRUE),
      .groups = "drop"
    )
  h2yr <- hdata %>%
    group_by(keyword, interval_2yr) %>%
    summarise(
      sum_hits = sum(as.numeric(hits), na.rm = TRUE),
      mean_hits = mean(as.numeric(hits), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Save(Automated)
  write_csv(k2yr, file.path(base_dir, sprintf("k_culture_gtrend_%s_2yr.csv", geo)))
  write_csv(h2yr, file.path(base_dir, sprintf("hansik_gtrend_%s_2yr.csv", geo)))
  
  cat(sprintf("Done: %s (K-culture & Hansik 2-year aggregation)\n", geo))
}

