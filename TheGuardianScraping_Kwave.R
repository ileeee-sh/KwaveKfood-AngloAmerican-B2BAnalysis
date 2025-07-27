# 1. Install and Load Required Packages

install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("stringr")
install.packages("writexl")
install.packages("lubridate")
install.packages("tidyr")
install.packages("readr")

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(writexl)
library(lubridate)
library(tidyr)
library(readr)

# 2. Set Guardian API Key
guardian_api_key <- "Insert Individual The Guardian API Key" 
Sys.setenv(GUARDIAN_API_KEY = guardian_api_key)

# 3. Define Hansik Keywords (same as Google Trends)
hansik_keywords <- c(
  "kimchi", "bulgogi", "tteokbokki", "K-food", "Korean food", "Korean fried chicken", "buldak",
  "bibimbap", "gochujang", "dakgalbi", "kimbap", "korean cuisine", "korean street food",
  "KBBQ", "Korean BBQ", "japchae", "jjigae", "samgyetang", "bingsu", "hansik",
  "soju", "makgeolli", "kimchi stew", "pajeon", "mandu", "yangnyeom chicken",
  "Korean restaurant", "Seoul food", "ramyeon"
)

# 4. Define Guardian API Fetch Function (Full BodyText)-
fetch_guardian_articles <- function(keyword, from_date = "2015-01-01", to_date = "2025-07-25", page_size = 50, max_pages = 10) {
  url <- "https://content.guardianapis.com/search"
  all_results <- list()
  for (page in 1:max_pages) {
    params <- list(
      "q" = keyword,
      "from-date" = from_date,
      "to-date" = to_date,
      "api-key" = Sys.getenv("GUARDIAN_API_KEY"),
      "page-size" = page_size,
      "show-fields" = "bodyText"
    )
    res <- GET(url, query = params)
    if (status_code(res) != 200) next
    json_data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    if (is.null(json_data$response$results) || length(json_data$response$results) == 0) break
    all_results[[page]] <- json_data$response$results
    Sys.sleep(1)
  }
  if (length(all_results) == 0) return(NULL)
  df <- bind_rows(all_results)
  df$bodyText <- df$fields.bodyText
  return(df)
}

# 5. Article Scraping Loop (All Keywords, Full BodyText)
all_articles <- data.frame(
  source = character(),
  title = character(),
  pub_date = as.Date(character()),
  matched_keywords = character(),
  link = character(),
  body_text = character(),
  stringsAsFactors = FALSE
)

for (keyword in hansik_keywords) {
  cat(sprintf("Scraping for keyword: '%s' ...\n", keyword))
  articles_raw <- fetch_guardian_articles(keyword, from_date = "2015-01-01", to_date = "2025-07-25")
  # Robust: check for data.frame and non-zero rows
  if (is.data.frame(articles_raw) && nrow(articles_raw) > 0) {
    articles_processed <- articles_raw %>%
      mutate(
        source = "The Guardian",
        title = webTitle,
        pub_date = as.Date(webPublicationDate),
        link = webUrl,
        matched_keywords = vapply(bodyText, function(text) {
          if (is.null(text) || is.na(text) || nchar(text) == 0) return(NA_character_)
          hits <- hansik_keywords[str_detect(text, regex(paste(hansik_keywords, collapse = "|"), ignore_case = TRUE))]
          if (length(hits) > 0) paste(unique(hits), collapse = ", ") else NA_character_
        }, character(1)),
        body_text = bodyText
      ) %>%
      select(source, title, pub_date, matched_keywords, link, body_text) %>%
      filter(!is.na(matched_keywords) & !is.na(body_text) & body_text != "")
    all_articles <- bind_rows(all_articles, articles_processed)
    cat(sprintf("  -> %d articles found, total: %d\n", nrow(articles_processed), nrow(all_articles)))
  } else {
    cat("  -> No articles found or error.\n")
  }
  Sys.sleep(10) 
}

# 6. Remove Duplicate Articles (URL)
all_articles_unique <- all_articles %>%
  distinct(link, .keep_all = TRUE)

cat("Articles before deduplication:", nrow(all_articles), "\n")
cat("Articles after deduplication:", nrow(all_articles_unique), "\n")

# 7. Save Final Dataset (.xlsx, .csv) to Custom Directory
save_dir <- "Insert Repository Location"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
xlsx_file <- file.path(save_dir, "guardian_hansik_articles_unique.xlsx")
csv_file  <- file.path(save_dir, "guardian_hansik_articles_unique.csv")

write.csv(all_articles_unique, csv_file, row.names = FALSE)
cat(sprintf("Saved: %s\n", csv_file))

# 8. Aggregate by 2-Year Interval
# Re-load (for safety)
guardian_articles <- read_csv(csv_file, show_col_types = FALSE)

guardian_articles$interval_2yr <- cut(
  year(guardian_articles$pub_date),
  breaks = c(2014, 2016, 2018, 2020, 2022, 2025),
  right = TRUE,
  labels = c("2015-2016", "2017-2018", "2019-2020", "2021-2022", "2023-2025")
)

interval_count <- guardian_articles %>%
  group_by(interval_2yr) %>%
  summarise(article_count = n())

long_df <- guardian_articles %>%
  separate_rows(matched_keywords, sep = ",\\s*")

interval_keyword_count <- long_df %>%
  group_by(interval_2yr, matched_keywords) %>%
  summarise(article_count = n()) %>%
  arrange(interval_2yr, desc(article_count))

write.csv(interval_count, file.path(save_dir, "guardian_hansik_articles_interval_count.csv"), row.names = FALSE)
write.csv(interval_keyword_count, file.path(save_dir, "guardian_hansik_articles_interval_keyword_count.csv"), row.names = FALSE)
cat("Saved aggregated results by 2-year intervals in:\n", save_dir, "\n")
