# word-cloud
library(wordcloud2)
library(rvest)
library(dplyr)

# requires a dataframe with 2 columns: word, freq
df <- data.frame(word = c("word1", "word2", "word3"), freq = c(10, 5, 8))
wordcloud2(df)

# customization options (color themes, shape, bg color, font size, rotation %)

# Using custom shapes(figpath)

# Interactive feature


# Practical example of making word cloud of State Names:

url <- "https://jakobtures.github.io/web-scraping/turnout.html"
page <- read_html(url)


# Extract state names
states <- page3 %>% html_elements(css = "span.state-name") %>% html_text2()

year <- page3 %>% html_elements(css = ".election-year") %>% html_text2()

turnout <- page3 %>% html_elements(css = ".election-turnout") %>% html_text2()


# combine into df
election_df <- data.frame(
  State = states,
  Year = as.integer(year),
  Turnout = as.numeric(gsub("%", "", turnout)),
  stringsAsFactors = FALSE
)

# check structure
head(election_df)


# wordcloud of top 20 states by avg turnout
state_avg_turn <- election_df %>%
  group_by(State) %>%
  summarise(avg_turnout = mean(Turnout)) %>%
  arrange(desc(avg_turnout)) %>%
  slice(1:20)


# create wordcloud
wordcloud2(state_avg_turn, size = 0.1, color = "random-light", backgroundColor = "black")












