# 1. Install & Load Packages
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("lubridate")

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# 2. Set Parameters
keywords <- c(
  "kimchi", "bulgogi", "tteokbokki", "K-food", "Korean food", "Korean fried chicken",
  "buldak", "bibimbap", "gochujang", "dakgalbi", "kimbap", "korean cuisine",
  "korean street food", "KBBQ", "Korean BBQ", "japchae", "jjigae", "samgyetang",
  "bingsu", "hansik", "soju", "makgeolli", "kimchi stew", "pajeon", "mandu",
  "yangnyeom chicken", "Korean restaurant", "Seoul food", "ramyeon"
)
date_start <- as.Date("2015-01-01")
date_end <- as.Date("2025-07-25")
intervals <- seq(year(date_start), year(date_end), by = 2)
max_sample_per_kw_interval <- 20

# 3. Helper: Download GDELT Exporter Metadata by Keyword & Interval
get_gdelt_sample <- function(keyword, from_date, to_date, n_max = 20) {
  url <- sprintf(
    "https://api.gdeltproject.org/api/v2/doc/doc?query=%s&mode=artlist&maxrecords=%d&startdatetime=%s&enddatetime=%s&format=json",
    URLencode(keyword), n_max,
    format(from_date, "%Y%m%d%H%M%S"),
    format(to_date, "%Y%m%d%H%M%S")
  )
  res <- tryCatch(httr::GET(url), error = function(e) NULL)
  if (is.null(res) || httr::status_code(res) != 200) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(parsed) || length(parsed$articles) == 0) return(NULL)
  dat <- as.data.frame(parsed$articles, stringsAsFactors = FALSE)
  dat$keyword <- keyword
  dat
}

# 4. Main Loop: For Each Keyword, Each 2-Year Interval, Download Sample
results <- list()
for (kw in keywords) {
  for (i in seq_along(intervals)) {
    from_yr <- intervals[i]
    to_yr <- ifelse(i < length(intervals), intervals[i+1] - 1, year(date_end))
    from_date <- as.Date(sprintf("%d-01-01", from_yr))
    to_date <- as.Date(sprintf("%d-12-31", to_yr))
    if (to_date > date_end) to_date <- date_end
    cat(sprintf("Query: %s [%s ~ %s]\n", kw, from_date, to_date))
    sample_df <- get_gdelt_sample(kw, from_date, to_date, max_sample_per_kw_interval)
    if (!is.null(sample_df) && nrow(sample_df) > 0) {
      sample_df$interval_2yr <- paste0(from_yr, "-", to_yr)
      results[[length(results) + 1]] <- sample_df
    }
    Sys.sleep(5) 
  }
}
gdelt_all <- bind_rows(results)

# 5. Filter for Anglosphere countries & English articles ONLY
anglosphere <- c("US", "GB", "CA", "AU", "NZ", "IE")
gdelt_out <- gdelt_all %>%
  transmute(
    keyword,
    interval_2yr,
    pub_date = as.Date(substr(seendate, 1, 8), "%Y%m%d"),
    title,
    url,
    country = sourcecountry,
    lang = language
  ) %>%
  filter(country %in% anglosphere) %>%
  filter(tolower(lang) == "english") %>%
  filter(!is.na(title) & !is.na(url)) %>%
  distinct(url, title, .keep_all = TRUE)

# 6. Save (.csv)
save_dir <- "Insert Repository Path"
csv_out <- file.path(save_dir, "gdelt_hansik_anglosphere_sampled.csv")
write.csv(gdelt_out, csv_out, row.names = FALSE)
cat("Saved:", csv_out, "\n")

