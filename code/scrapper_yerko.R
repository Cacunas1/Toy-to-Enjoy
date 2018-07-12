library(rvest)
library(tidyverse)

direccion <- "https://www.amazon.com/ALEX-Toys-Rub-Draw-Crayons/product-reviews/B000G2FOLC/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"

download.file(direccion,destfile = "scrapedpage.html", quiet = TRUE)

webpage <- read_html("scrapedpage.html")

Using CSS selectors to scrap the rankings section
webpage <- read_html(webpage)
rank_data_html <- html_nodes(webpage, ".a-icon-alt") %>% html_text()
Title <- html_nodes(webpage,"span.a-size-base review-title a-text-bold") %>% html_text()
Comment <- html_nodes(webpage,"a-size-base review-text") %>% html_text()

Converting the ranking data to text
rank_data <- html_text(rank_data_html)
head(rank_data)
rank_data %>% View