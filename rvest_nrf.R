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
#View(table1B)
table1C1 <- table1B %>% slice(c(1,4)) %>% select(1,2,8,9,10,11,12,13,14,15,16) %>% set_names(c('ins_id', 'name','TLR','RPC','GO','OI','PR','city','state', 'score','rank'))
head(table1C1)

table1C2 <- table1B %>% slice(seq(1,300, 3)) %>% select(1,2,8,9,10,11,12,13,14,15,16) %>% set_names(c('ins_id', 'name','TLR','RPC','GO','OI','PR','city','state', 'score','rank')) %>% mutate(domain='Overall', nyear = 'N2024')
View(table1C2)


# Other years data scrapping from web
url23 = "https://www.nirfindia.org/Rankings/2023/OverallRanking.html"
url22 = "https://www.nirfindia.org/Rankings/2022/OverallRanking.html"


# check if URL exists
url.exists(url23)
url.exists(url22)

# Read the HTML content of the page:
page23 = read_html(url23)
page22 = read_html(url22)


# Get all HTML nodes from the page:
all_node23 = page23 %>% html_elements(xpath = '//*')
all_node22 = page22 %>% html_elements(xpath = '//*')

# it tells u the type of HTML_elements like div, table etc
all_node22 %>% html_name()


# Find all tables on the page
tables23 <- page23 %>% html_elements("table")
tables22 <- page22 %>% html_elements("table")


# Extract the first table and convert it into a data frame
table1_22 <- tables22[[1]] %>% html_table(fill=T)
table1_23 <- tables23[[1]] %>% html_table(fill=T)


# Clean the column names using Janitor
cleanTable23 <- table1_23 %>% janitor::clean_names()
cleanTable22 <- table1_22 %>% janitor::clean_names()


# Preview specific rows (1:4)
slice(cleanTable22, c(1, 4))


# Create a cleaned selected table for Sample
CTable22 <- cleanTable22 %>% 
  slice(c(1, 4)) %>% 
  select(1, 2, 8, 9, 10, 11, 12, 13, 14, 15, 16) %>%
  set_names(c('ins_id', 'name', 'TLR', 'RPC', 'GO', 'OI', 'PR', 'city', 'state', 'score', 'rank'))

CTable23 <- cleanTable23 %>%
  slice(c(1, 4)) %>%
  select(1, 2, 8, 9, 10, 11, 12, 13, 14, 15, 16) %>%
  set_names(c('ins_id', 'name', 'TLR', 'RPC', 'GO', 'OI', 'PR', 'city', 'state', 'score', 'rank'))


# Final Cleaned table for full dataset (every 3rd row)
FCTable22 <- cleanTable22 %>%
  slice(seq(1, 300, 3)) %>%
  select(1, 2, 8, 9, 10, 11, 12, 13, 14, 15, 16) %>%
  set_names(c('ins_id', 'name', 'TLR', 'RPC', 'GO', 'OI', 'PR', 'city', 'state', 'score', 'rank')) %>%
  mutate(domain="Overall", nyear='N2022')


FCTable23 <- cleanTable23 %>%
  slice(seq(1, 300, 3)) %>%
  select(1, 2, 8, 9, 10, 11, 12, 13, 14, 15, 16) %>%
  set_names(c('ins_id', 'name', 'TLR', 'RPC', 'GO', 'OI', 'PR', 'city', 'state', 'score', 'rank')) %>%
  mutate(domain="Overall", nyear='N2023')

#View(FCTable23)
#View(FCTable22)

nirf_all <- rbind(table1C2, FCTable23, FCTable22)
View(nirf_all)

nirf_all$name <- sub("More.*", "", nirf_all$name)
head(nirf_all)
nirf_all$name
names(nirf_all)


#abc <- nirf_all %>% arrange(desc(c('nyear','score'))) %>% slice_head(n=6)
top6_names <- nirf_all %>% 
  arrange(desc(nyear), desc(score)) %>%
  slice_head(n=6) %>% pull(name)

top6_all_year <- nirf_all %>% filter(name %in% top6_names)
top6_all_year

# Line Chart:
top6_all_year %>% ggplot(., aes(x=nyear, y=score)) + geom_line(aes(group=name, colour="red", linewidth = 2)) + geom_point() + facet_wrap(vars(name), scales='free')

# Heat-Map:
top6_all_yearl <- top6_all_year %>% pivot_longer(cols = c('TLR', 'RPC', 'GO', 'OI', 'PR'), names_to ="category")

top6_all_yearl %>% 
  ggplot(., aes(x=category, y=ins_id, fill=value)) + 
  geom_tile(color = "black", lwd=1.5, linetype=1) +
  geom_text(aes(label=value), color="black", size=4) +
  facet_wrap(vars(nyear), scales='free') +
  theme(axis.text.y = element_text(angle = 45, hjust=1)) +
  scale_fill_gradient2(high = '#2c699a', mid = '#16db93', midpoint = 75, low='#b9e769')


# Top performing states under Top 10:
#nirf_all <- nirf_all %>% mutate(nyear = trimws(as.character(nyear)))

top10_year <- nirf_all %>% 
  group_by(nyear) %>%
  arrange(desc(score)) %>%
  slice_head(n=10) %>%
  ungroup()
top10_year

# Count how many time a state apeears in top 10

top_states <- top10_year %>% 
  group_by(state) %>% 
  summarise(count=n()) %>% 
  filter(count==3)

consistent_top_states <- top10_year %>%
  group_by(state) %>%  filter(state %in% top_states$state)

st10 <- consistent_top_states %>% group_by(state) %>% select(state, nyear, score)
#View(st10)


st10 %>% ggplot(., aes(x=state, y=score)) + geom_col(aes(fill=state, width=0.75)) + facet_wrap(vars(nyear), scales = 'free') + geom_text(aes(label=score), color="black", size = 4, vjust=1.5) + theme(axis.text.y = element_text(angle = 45, hjust=1)) + coord_flip()

