#Question a

data(iris)
par(mfrow = c(1, 2))

boxplot(iris$Sepal.Length ~ iris$Species, main = "Sepal Length by Species")

boxplot(iris$Petal.Length ~ iris$Species, main = "Petal Length by Species")

plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species,
     xlab = "Sepal Length", ylab = "Petal Length", main = "Scatterplot of Sepal.Length and Petal.Length")
legend("topright", legend = unique(iris$Species), col = unique(iris$Species), pch = 1)


#Question b

library(imager)

flip <- function(image) {
  
  width = dim(image)[2]
  height = dim(image)[1]
  
  
  flipped_image = imager::imager(height = height, width = width)
  
  for (y in 1:height) {
    for (x in 1:width) {
      
      pixel <- imager::get(image, x, y)
      
      imager::set(flipped_image, width - x + 1, y, pixel)
    }
  }
  
  return(flipped_image)
}

image <- load.example("parrots")

final_image <- flip(image)
imager::plot(final_image)

#Question c

library(MASS)

data(ships)

incident_counts <- tapply(ships$incidents, ships$type, sum)

barplot(incident_counts, 
        main = "Total Number of Incidents by Ship Type",
        xlab = "Ship Type",
        ylab = "Total Number of Incidents",
        col = "steelblue")

#Question d

library(rvest)

list = data.frame()

for (page in 1:13855) {
  
  link = paste0("https://stats.stackexchange.com/questions?tab=votes&page=","page")
  page_link = read_html(link)
  
  title = page_link %>% html_nodes("#questions .s-link") %>% html_text()
  votes = page_link %>% html_nodes(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text()
  views = page_link %>% html_nodes(".is-supernova .s-post-summary--stats-item-number") %>% html_text()
  answers = page_link %>% html_nodes(".has-answers .s-post-summary--stats-item-number") %>% html_text()
  
  list = rbind(list, data.frame(title,votes,views,answers,stringsAsFactors = FALSE))
  
}

View(list)

#Question e

trials <- 100000
total_days <- 0
for (i in 1:trials) {
  days <- 0
  tablets <- 100
  while (tablets > 0) {
    days <- days + 1
    if (runif(1) < tablets / (tablets + days - 1)) {
      break
    }
    tablets <- tablets - 1
  }
  total_days <- total_days + days
}
average_days <- total_days / trials
average_days
