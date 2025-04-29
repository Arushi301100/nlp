# Example 3

library(rvest)
library(ggplot2)
library(scales)
pacman::p_load(tidyr, dplyr, readxl)


# Import data from website
link <- "https://jakobtures.github.io/web-scraping/turnout.html"
page3 = read_html(link)
page3

# read raw html file
#html_txt <- readLines(link, warn = F)
#cat(html_text[1:20], sep = "\n")


# Extracting the title
(title <- page3 %>% html_element("title") %>% html_text2())


# Extract the data elements using html_name
page3 %>% html_elements(xpath = "//*") %>% html_name()


# Finding the data from class names 
year <- page3 %>% html_elements(css = ".election-year") %>% html_text2()
head(year, 10)

turnout <- page3 %>% html_elements(css = ".election-turnout") %>% html_text2()
head(turnout, 10)

states <- page3 %>% html_elements(css = "span.state-name") %>% html_text2()
head(states, 10)



