# Clean and setup ---------------------------------------------------------

rm(list = ls())

library(rvest)
library(tidyverse)
library(stringr)
library(rebus)
library(lubridate)
library(magrittr)

# Function definitions ----------------------------------------------------

# Function get_last_page
get_last_page <- function(html){
	pages_data <- html %>% html_nodes(".page-button")%>% html_text()
	pages_data[(length(pages_data))] %>% unname() %>% as.numeric()
}

Scrap <- function(urlAddr){
	# Acquiring html page
	scrapped <- "../output/scrapedpage.html"
	download.file(urlAddr, destfile = scrapped, quiet = TRUE)
	webpage <- read_html(scrapped)

	# Extracting ratings
	rank_data_html <- html_nodes(webpage, ".a-icon-alt") %>% html_text()
	rank_data_html <- rank_data_html[rank_data_html != "|" ]
	only_stars <- str_sub(rank_data_html,1,1)
	new_rating <- c(only_stars[4:13])

	# Extracting comments
	Comment <- html_nodes(webpage, ".review-text , .a-spacing-top-mini .a-size-base") %>% html_text()
	new_comments <- c(Comment[3:13])
	pos <- complete.cases(new_comments)
	new_comments <- new_comments[pos]

	Reviews <- cbind(Rating = new_rating, Review = new_comments) %>% as_tibble()
	return(Reviews)
}


scrappe <- function(baseURL) {
	scrapped <- "../output/scrapedpage.html"
	download.file(baseURL, destfile = scrapped, quiet = TRUE)
	webpage <- read_html(scrapped)

	# Get the last page
	latest_page_number <- get_last_page(webpage)

	list_of_pages <- str_c(baseURL, "&pageNumber=", 1:latest_page_number)

	answer <- tibble()

	for (i in list_of_pages) {
		answer %<>% rbind(Scrap(i))
	}

	return(answer)
}

# JUGUETE 1 Crayons -------------------------------------------------------

direccion <- "https://www.amazon.com/ALEX-Toys-Rub-Draw-Crayons/product-reviews/B000G2FOLC/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"

answer1 <- scrappe(direccion)

answer1 <- answer1[1:431,]

answer1 %>% View

# JUGUETE 2 Boon Building Bath Pipes Toy ----------------------------------

direccion <- "https://www.amazon.com/Boon-Building-Bath-Pipes-Toy/product-reviews/B00R0V7PUU/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"

answer2 <- scrappe(direccion)

answer2 <- answer2[1:1219,]

# JUGUETE 3 NICK JR PAW ---------------------------------------------------

direccion <- "https://www.amazon.com/Nick-Paw-Patrol-Bifold-Wallet/product-reviews/B01836F31I/ref=cm_cr_othr_d_paging_btm_1?ie=UTF8&reviewerType=all_reviews"

answer3 <- scrappe(direccion)

answer3 <- answer3[1:260,]

# JUGUETE 4 LIGHT-UP TOY WATERPROOF ---------------------------------------

direccion <- "https://www.amazon.com/Light-up-Waterproof-Floating-Instruction-Educational/product-reviews/B078Y98FSB/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"

answer4 <- scrappe(direccion)

answer4 <- answer4[1:160,]

# JUGUETE 5 MOTOROLA MBP854 -----------------------------------------------

direccion <- "https://www.amazon.com/Motorola-MBP854CONNECT-Monitor-4-3-Inch-Internet/product-reviews/B00ROEBAK4/ref=cm_cr_dp_d_show_all_top?ie=UTF8&reviewerType=all_reviews"

answer5 <- scrappe(direccion)

answer5 <- answer5[1:1025,]

Reviews_All <- rbind(answer1, answer2, answer3, answer4, answer5)

# save scrapping ----------------------------------------------------------

write.csv(Reviews_All, file = "../input/scrapped_reviews.csv")
