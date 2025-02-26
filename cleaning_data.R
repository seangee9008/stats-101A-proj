library(dplyr)
library(ggplot2)

data <- read.csv("~/Downloads/dataset.csv", header=TRUE)

# Clean but check it just in case

fill_na_with_mean <- function(data) {
data[] <- lapply(data, function(x) {
if (is.numeric(x)) {
x[[is.na](http://is.na/)(x)] <- mean(x, na.rm = TRUE)
}
return(x)
})
return(data)
}

df_cleaned <- fill_na_with_mean(data)
head(df_cleaned)
