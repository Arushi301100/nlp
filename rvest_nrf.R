# Extract Nirf data 2024

library(rvest)
library(tidyverse)
library(RCurl)


url = "https://www.nirfindia.org/Rankings/2024/OverallRanking.html"
url.exists(url)

# Scrap the data
page = read_html(url)
page

all_nodes <- page %>% html_elements(xpath = '//*')
all_nodes %>% html_name()
tables <- page %>% html_elements("table") # %>% html_table(fill = T)
length(tables)
table1 <- tables[[1]] %>% html_table(fill = T)
head(table1, 5)

table1B <- table1 %>% janitor::clean_names()
slice(table1B, c(1, 4))
table1B %>% slice(c(1,4))
dim(table1B)
table1C1 <- table1B %>% slice(c(1,4)) %>% select(1,2,8,9,10,11,12,13,14,15,16) %>% set_names(c('institutionid', 'name','TLR','RPC','GO','OI','PR','city','state', 'score','rank'))
head(table1C1)

table1C2 <- table1B %>% slice(seq(1,300, 3)) %>% select(1,2,8,9,10,11,12,13,14,15,16) %>% set_names(c('institutionid', 'name','TLR','RPC','GO','OI','PR','city','state', 'score','rank')) %>% mutate(domain='Overall', nyear = 'N2024')
head(table1C2)

