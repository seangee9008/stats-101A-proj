# Import libraries
library(tidyverse)

# Parameters
data <- read.csv("dataset.csv", header=TRUE)
save_data <- FALSE

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
df_cleaned <- na.omit(data)
df_cleaned <- data[complete.cases(data[, c("Mean.Hourly.Gap", "Median.Hourly.Gap", "Q1.Female", "Q2.Female", "Q3.Female", "Q4.Female", "Percentage.Employees.Female")]), ]
head(df_cleaned)

#drop columns

df_cleaned <- df_cleaned %>% select(-c(`Report.Link`, `Company.Site`))

if (save_data){
  write.csv(df_cleaned, file = "cleaned_dataset.csv")
}

# Explore cleaned data
attach(df_cleaned)
## Distribution of NACE Section
company_type <- table(NACE.Letter)
barplot(company_type)

## Distribution of years
report_years <- table(Report.Year)
barplot(report_years)

# Define initial exploration function
initial_exploration <- function(dataframe){
  ## See the summary of quantitative variables
  quant_vars <- df_cleaned %>% 
    select(-c('Company.Name', 'NACE.Section', 'NACE.Letter', "NACE.Division", "Report.Year"))
  
  print(summary(quant_vars))
  print(cor(quant_vars))
  ### Observation: pay gap variables correlate strongly with each others but only correlate weakly with gender composition variables, and vice versa.
  
  ## Narrow down the tables to only include certain variables
  new_table <- data.frame(Report.Year, Mean.Hourly.Gap, Median.Hourly.Gap, Q1.Female, Q2.Female, Q3.Female, Q4.Female, Percentage.Employees.Female)
  print(cor(new_table))
  print(pairs(new_table))
  ### Same observations
  
  m1 <- lm(Mean.Hourly.Gap ~ Q1.Female + Q2.Female + Q3.Female + Q4.Female + Percentage.Employees.Female)
  summary(m1)
  # Looks pretty interesting
}

#explore full data
initial_exploration(df_cleaned)

# Group by NACE Letter
df_cleaned %>% 
  group_by(NACE.Letter) %>% 
  summarize(mean = mean(Mean.Hourly.Gap), 
            range = max(Mean.Hourly.Gap)-min(Mean.Hourly.Gap),
            variation = var(Mean.Hourly.Gap),
            count = n()) %>% 
  arrange(desc(range))

#Examine interesting industries
C <- df_cleaned %>% 
  filter(NACE.Letter == "C")
initial_exploration(C)

P <- df_cleaned %>% 
  filter(NACE.Letter == "P")
initial_exploration(P)
