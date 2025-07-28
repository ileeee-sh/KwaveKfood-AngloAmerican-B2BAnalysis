# 1. Preprocessing (clean, stopwords, QA)
# 2. Sentiment, trend, KWIC, wordcloud, regression

# 0. Install & Load Packages
pkgs <- c("readxl", "dplyr", "stringr", "lubridate", "tidytext", "syuzhet",
          "textdata", "ggplot2", "reshape2", "wordcloud", "writexl", "quanteda")
for (pkg in pkgs) if (!require(pkg, character.only = TRUE)) install.packages(pkg)
lapply(pkgs, library, character.only = TRUE)

# 1. Load Data & Stopwords
save_dir <- "insert your repository location"
csv_file <- file.path(save_dir, "guardian_hansik_articles_unique.csv")
stopwords_file <- file.path(save_dir, "English_Stopwords.xlsx")

articles <- read.csv(csv_file, stringsAsFactors = FALSE)
stopwords_en <- read_excel(stopwords_file, col_names = FALSE)[[1]]
stopwords_en <- tolower(trimws(stopwords_en))

# 2. Preprocessing & Cleaning
articles <- articles %>%
  filter(!is.na(pub_date) & !is.na(title) & !is.na(matched_keywords) & !is.na(body_text))
articles$pub_date <- as.Date(articles$pub_date)

# Lowercase, remove special chars, trim spaces
clean_text <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-zA-Z0-9\\s]", " ", x)
  str_squish(x)
}
articles$title <- sapply(articles$title, clean_text)
articles$body_text <- sapply(articles$body_text, clean_text)

# Remove stopwords from title (can do for body_text if needed)
remove_stopwords <- function(text, stopwords) {
  words <- unlist(str_split(text, "\\s+"))
  filtered <- words[!words %in% stopwords]
  paste(filtered, collapse = " ")
}
articles$title_nostop <- sapply(articles$title, remove_stopwords, stopwords = stopwords_en)
articles$body_text_nostop <- sapply(articles$body_text, remove_stopwords, stopwords = stopwords_en)

# Save cleaned data for reproducibility
cleaned_file <- file.path(save_dir, "guardian_hansik_articles_cleaned.csv")
write.csv(articles, cleaned_file, row.names = FALSE)

# 3. Assign 2-Year Intervals for Aggregation
articles$interval_2yr <- paste0(
  2 * ((year(articles$pub_date) - 2015) %/% 2) + 2015, "-",
  2 * ((year(articles$pub_date) - 2015) %/% 2) + 2016
)
articles$interval_2yr[year(articles$pub_date) >= 2025] <- "2025-2025"

# 4. Sentiment Analysis (Syuzhet, Bing, Afinn)
articles$sentiment_syuzhet <- get_sentiment(articles$body_text, method = "syuzhet")
bing_lex <- get_sentiments("bing")
afinn_lex <- get_sentiments("afinn")
articles$sentiment_bing <- sapply(articles$body_text, function(text) {
  words <- unlist(str_split(tolower(text), "\\W+"))
  sum(bing_lex$sentiment[match(words, bing_lex$word)] == "positive", na.rm = TRUE) -
    sum(bing_lex$sentiment[match(words, bing_lex$word)] == "negative", na.rm = TRUE)
})
articles$sentiment_afinn <- sapply(articles$body_text, function(text) {
  words <- unlist(str_split(tolower(text), "\\W+"))
  sum(afinn_lex$value[match(words, afinn_lex$word)], na.rm = TRUE)
})

# 5. Long Format for Per-Keyword Analysis
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)

articles_long <- articles %>%
  filter(!is.na(matched_keywords)) %>%
  mutate(keyword = strsplit(as.character(matched_keywords), ",\\s*")) %>%
  unnest(keyword)

# 6. Aggregation: 2-Year and Per-Keyword
sentiment_summary <- articles %>%
  group_by(interval_2yr) %>%
  summarise(
    article_count = n(),
    mean_syuzhet = mean(sentiment_syuzhet, na.rm = TRUE),
    mean_bing = mean(sentiment_bing, na.rm = TRUE),
    mean_afinn = mean(sentiment_afinn, na.rm = TRUE)
  )
sentiment_by_kw <- articles_long %>%
  group_by(interval_2yr, keyword) %>%
  summarise(
    mean_syuzhet = mean(sentiment_syuzhet, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )
sentiment_summary_file <- file.path(save_dir, "guardian_sentiment_summary_2yr.csv")
write.csv(sentiment_summary, sentiment_summary_file, row.names = FALSE)
cat("Saved:", sentiment_summary_file, "\n")

# From now on, these are the codes just for testing, optional
# 7. Time Series & Trend Visualization
ggplot(sentiment_summary, aes(x = interval_2yr, y = article_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Articles per 2-Year Interval", x = "Interval", y = "Count") + theme_minimal()
ggplot(sentiment_summary, aes(x = interval_2yr, y = mean_syuzhet, group = 1)) +
  geom_line(size = 1.2, color = "tomato") + geom_point(size = 2, color = "darkred") +
  labs(title = "Mean Syuzhet Sentiment by Interval", x = "Interval", y = "Mean Sentiment") +
  theme_minimal()
heatmap_data <- sentiment_by_kw %>%
  filter(count > 1) %>%
  acast(keyword ~ interval_2yr, value.var = "mean_syuzhet", fill = NA)
heatmap(heatmap_data, Rowv = NA, Colv = NA, col = topo.colors(10), scale = "column")

# 8. Outlier Detection (Peaks)
max_interval <- sentiment_summary$interval_2yr[which.max(sentiment_summary$mean_syuzhet)]
min_interval <- sentiment_summary$interval_2yr[which.min(sentiment_summary$mean_syuzhet)]
cat(sprintf("Highest Sentiment Interval: %s\nLowest Sentiment Interval: %s\n", max_interval, min_interval))

# 9. Correlation and Regression
cor_stats <- sentiment_summary
lm_model <- lm(mean_syuzhet ~ article_count, data = cor_stats)
summary(lm_model)

# 10. Keyword Sentiment Distribution(Boxplot)
top_keywords <- articles_long %>% count(keyword, sort = TRUE) %>% top_n(5, n) %>% pull(keyword)
articles_long %>%
  filter(keyword %in% top_keywords) %>%
  ggplot(aes(x = keyword, y = sentiment_syuzhet, fill = keyword)) +
  geom_boxplot() +
  labs(title = "Sentiment Distribution by Top Keywords", x = "Keyword", y = "Sentiment") +
  theme_minimal()

# 11. KWIC
all_keywords <- articles$matched_keywords %>%
  paste(collapse = ",") %>%        
  str_split(",\\s*") %>%            
  unlist() %>%
  unique() %>%
  trimws() %>%
  .[. != ""]                       

print(all_keywords)

corp <- corpus(articles$body_text)
toks <- tokens(corp)

for (kw in all_keywords) {
  kwic_res <- kwic(toks, pattern = kw, window = 7)
  if (nrow(kwic_res) > 0) {
    kwic_df <- as.data.frame(kwic_res)
    out_file <- file.path(save_dir, paste0("kwic_", kw, ".csv"))
    write.csv(kwic_df, out_file, row.names = FALSE)
    cat("Saved KWIC for:", kw, "\n")
  }
}

getwd()

# 12. Wordcloud Visualization
words <- articles %>%
  unnest_tokens(word, body_text) %>%
  anti_join(data.frame(word = stopwords("en")), by = "word") %>%
  count(word, sort = TRUE)
set.seed(123)
wordcloud(words = words$word, freq = words$n, max.words = 80, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"), scale = c(4,0.8))

cat("Full Guardian hansik news workflow (preprocessing + sentiment + visualization) complete.\n")
