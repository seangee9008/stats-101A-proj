library(tidyverse)
library(car)
library(GGally)
library(MASS)
library(vtable)
data <- read.csv("cleaned_dataset.csv", header=TRUE) %>% dplyr::select(-X)
attach(data)
```
# Summary statistics and data exploration

## Distribution of categorical variables
par(mfrow = c(1, 2))
company_type <- table(NACE.Letter)
barplot(company_type, xlab="Company Type by NACE Letters", ylab="Count")

report_years <- table(Report.Year)
barplot(report_years, xlab="Year Reported", ylab="Count")

highlighted_industry <- c('K', 'L', 'M', 'N', "G", "J")
# create an added variable group
data$group <- ifelse((data$NACE.Letter %in% highlighted_industry), 1, 0)
head(data)
#group 1 is denoted by 1
#group 2 is denoted by 0

#Two groups 
## Companies that have NACE letter F, K, or M
group_1 <- data %>% filter(NACE.Letter %in% highlighted_industry)
## Companies not in these letters
group_2 <- data %>% filter(!(NACE.Letter %in% highlighted_industry))

# I created two distinct groups using the pipe operator and in function! lmk if you guys want me to make any changes
# - Shelby

## Extract variables

new_table <- data %>% dplyr::select(-c(`Company.Name`, `NACE.Section`, `NACE.Letter`, `NACE.Division`, `Q1.Female`, `Q2.Female`, `Q3.Female`, `Q4.Female`, `Q1.Men`, `Q2.Male`, `Q3.Male`, `Q4.Male`))

group_1_table <- group_1 %>% dplyr::select(-c(`Company.Name`, `NACE.Section`, `NACE.Letter`, `NACE.Division`, `Q1.Female`, `Q2.Female`, `Q3.Female`, `Q4.Female`, `Q1.Men`, `Q2.Male`, `Q3.Male`, `Q4.Male`)) 

group_2_table <- group_2 %>% dplyr::select(-c(`Company.Name`, `NACE.Section`, `NACE.Letter`, `NACE.Division`, `Q1.Female`, `Q2.Female`, `Q3.Female`, `Q4.Female`, `Q1.Men`, `Q2.Male`, `Q3.Male`, `Q4.Male`))


### Summary Table
st(new_table, digits=4)

st(group_1_table, digits =4,title = "Group 1 Summary Statistics")

st(group_2_table, digits =4,title = "Group 2 Summary Statistics")

### Distribution of variables
par(mfrow = c(2,4))
for (col in 2:ncol(new_table)){
  hist(new_table[, col], main=colnames(new_table)[col], xlab=colnames(new_table)[col])
}

#group 1
par(mfrow = c(2,4))
for (col in 2:ncol(group_1_table)){
  hist(group_1_table[, col], main=colnames(group_1_table)[col], xlab=colnames(group_1_table)[col])
}

#group 2
par(mfrow = c(2,4))
for (col in 2:ncol(group_2_table)){
  hist(group_2_table[, col], main=colnames(group_2_table)[col], xlab=colnames(group_2_table)[col])
}


### Correlation table
cor(new_table)
pairs(new_table)

#group 1
cor(group_1_table)
pairs(group_1_table)

#group 2
cor(group_2_table)
pairs(group_2_table)

new_table <- new_table %>% dplyr::select(-c(`Median.Hourly.Gap`, `Median.Hourly.Gap.Part.Time`, `Median.Hourly.Gap.Part.Temp`, `Percentage.Bonus.Paid.Male`, `Percentage.BIK.Paid.Male`, `Percentage.Employees.Male`))

st(new_table, digits = 3)

#option 2 simple linear regression as opposed to bin log reg
group_1_table <- group_1_table %>% dplyr::select(-c(`Median.Hourly.Gap`, `Median.Hourly.Gap.Part.Time`, `Median.Hourly.Gap.Part.Temp`, `Percentage.Bonus.Paid.Male`, `Percentage.BIK.Paid.Male`, `Percentage.Employees.Male`))

st(group_1_table, digits = 3)

group_2_table <- group_2_table %>% dplyr::select(-c(`Median.Hourly.Gap`, `Median.Hourly.Gap.Part.Time`, `Median.Hourly.Gap.Part.Temp`, `Percentage.Bonus.Paid.Male`, `Percentage.BIK.Paid.Male`, `Percentage.Employees.Male`))

st(group_2_table, digits = 3)

# Plot to see ditribution
color = new_table$group + 1
par(mfrow=c(2,4))
plot(new_table$Mean.Bonus.Gap, new_table$`Mean.Hourly.Gap`, col=color)
plot(new_table$Median.Bonus.Gap, new_table$`Mean.Hourly.Gap`, col=color)
plot(new_table$Mean.Hourly.Gap.Part.Time, new_table$`Mean.Hourly.Gap`, col=color)
plot(new_table$Mean.Hourly.Gap.Part.Temp, new_table$`Mean.Hourly.Gap`, col=color)
plot(new_table$Percentage.Bonus.Paid.Female, new_table$`Mean.Hourly.Gap`, col=color)
plot(new_table$Percentage.BIK.Paid.Female, new_table$`Mean.Hourly.Gap`, col=color)
plot(new_table$Percentage.Employees.Female, new_table$`Mean.Hourly.Gap`, col=color)

## Results and Interpretation

full_model <- lm(`Mean.Hourly.Gap` ~ ., data = new_table)
summary(full_model)

par(mfrow = c(2,2))
plot(full_model)

