# Import libraries
library(dplyr)
library(ggplot2)

# Parameters
data <- read.csv("dataset.csv", header=TRUE)
save_data <- TRUE

# Clean but check it just in case

fill_na_with_mean <- function(data) {
  data[] <- lapply(data, function(x) {
    if (is.numeric(x)) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
    }
    return(x)
  })
  return(data)
}

df_cleaned <- fill_na_with_mean(data)
head(df_cleaned)

#drop columns

df_cleaned <- df_cleaned %>% select(-c(`Report.Link`, `Company.Site`))

if (save_data){
  write.csv(df_cleaned, file = "cleaned_dataset.csv")
}
