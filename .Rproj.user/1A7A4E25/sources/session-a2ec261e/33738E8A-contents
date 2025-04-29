# Example 2
library(rvest)
library(ggplot2)
library(scales)
pacman::p_load(tidyr, dplyr, readxl)

html <- minimal_html("<body>
                      <h1> This is a heading </h1>
                     <p> This is a paragraph <p>
                     <p> this is third paragraph </p>
         <p> <a href='https://amity.edu'>amity University</a></p>
                     <ul>
                     <li> This is a bulleted list </li>
                     </ul
                     <p> this is second paragraph </p>
                     </body>
                     ")
html


html %>% html_element("h1") %>% html_text2()


html %>% html_elements("p") %>% html_text()

html %>% html_element("body") %>% html_text2()

html %>% html_elements("a") %>% html_attr("href")

html %>% html_elements("a") %>% html_text()



html2 <- minimal_html("
                      <table>
                        <tr>
                          <th> Name </th>
                          <th> Gender </th>
                        </tr>
                        <tr>
                          <th> Arushi </th>
                          <th> Female </th>
                        </tr>
                        <tr>
                          <th> Dhiraj upadhyaya </th>
                          <th> Male </th>
                        </tr>
                        <tr>
                          <th> Ekta </th>
                          <th> Female </th>
                        </tr>
                      </table>")
html2

html2 %>% html_element("table") %>% html_table()
html2 %>% html_element("table") %>% html_text()
html2 %>% html_element("table") %>% html_text2()

url = "https://w3schools.com/html/html_elements.asp"
page = read_html(url)


  # Inspect
headings <- page %>% html_elements("h2") %>% html_text()
headings

page %>% html_element("table") %>% html_table()



# ---------------------------
# Reading data from Wikipedia
url1 <- "https://en.wikipedia.org/wiki/States_and_union_territories_of_India"
page1 = read_html(url1)


# Inspect
headings1 <- page1 %>% html_elements("h1") %>% html_text()
headings2 <- page1 %>% html_elements("h2") %>% html_text2()
headings3 <- page1 %>% html_elements("h3") %>% html_text2()
headings4 <- page1 %>% html_elements("h4") %>% html_text2()
headings4


page1 %>% html_element("table") %>% html_table()
tables1 <- html_table(html_nodes(page1, 'table'))
tables1

table1 <- tables1[[1]]

page %>% html_element("table1") %>% html_table(fill = T)
head(table1)
View(table1)



# __________________________________________________________
# Importing data from scrapethissite.com/pages/simple

url2 <- "https://www.scrapethissite.com/pages/simple/"
page2 = read_html(url2)


# Extract all country blocks
countries <- page2 %>% html_elements(".country")
countries

# Inspect
headings12 <- page2 %>% html_elements("h1") %>% html_text()
headings22 <- page2 %>% html_elements("h2") %>% html_text2()
headings32 <- page2 %>% html_elements("h3") %>% html_text2()
headings42 <- page2 %>% html_elements("h4") %>% html_text2()
headings32


# There is no table here, so we have to scrap manually.

# Extract data manually
data <- tibble(
  country = countries %>% html_element("h3") %>% html_text2(),
  capital = countries %>% html_element(".country-capital") %>% html_text2(),
  population = countries %>% html_element(".country-population") %>% html_text2() %>% as.numeric(),
  area = countries %>% html_element(".country-area") %>% html_text2() %>% as.numeric()
)

# View the table
print(data)
View(data)
str(data)

options(scipen = 99)
# Plot: Top 10 countries by Population
data %>% 
  arrange(desc(population)) %>%
  slice_head(n=10) %>%
  ggplot(., aes(x=reorder(country, population), y = population)) +
  geom_col(fill = "#3dccc7") +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(accuracy =1, scale_cut = cut_short_scale())
    , breaks = seq(min(data$population), max(data$population), length.out = 5)) +
  labs(title = "Top 10 Countries by Population", x="Country", y="Population") + theme(axis.text.y = element_text(angle = 0, hjust=0.5)) + theme(axis.text.x = element_text(angle = 45, hjust=0.5)) + geom_hline(yintercept = c(0, 332511000, 665022000, 997533000, 1330044000), color = "red") + geom_text(y=c(332511000), label = "q1") + geom_vline(xintercept = 9)


# Plot: Top 10 countries by Area
data %>% 
  arrange(desc(area)) %>%
  slice_head(n=10) %>%
  ggplot(aes(x = reorder(country, area), y = factor(area))) +
  geom_col(fill = "#b8b8ff") +
  labs(title = "Top 10 countries by Area", x="Country", y="Area(km^2)") +
  theme(axis.text.y = element_text(angle = 45, hjust=0.5), axis.text.x = element_text(angle = 45, hjust=1)) + coord_flip()











