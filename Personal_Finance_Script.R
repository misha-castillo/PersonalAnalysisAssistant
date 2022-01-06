library(lubridate)
library(tidyverse)
library(dplyr)

# Setting workspace
setwd("~/R/expenses_analysis")

# Loading the data set for expenses
expenses_dataset = read.csv(file = 'expenses_dataset.csv')
expenses_dataset <- separate(expenses_dataset, date_of_purchase, into=c('day','month','year'),sep='/')
months_name <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
expenses_dataset$day <- as.integer(expenses_dataset$day)
expenses_dataset$month <- as.integer(expenses_dataset$month)
expenses_dataset$year <- as.integer(expenses_dataset$year)
expenses_dataset$price <- as.numeric(expenses_dataset$price)
#expenses_dataset$month <- month.abb[expenses_dataset$month]

#########################################
# Creating Specific variables
#########################################

# The initial year from the data set
minyear <- min(expenses_dataset$year)
# The last year from the data set
maxyear <- max(expenses_dataset$year)

#########################################
# Creating the filtered expenses and their respective variables
#########################################

# Creating the category_expense data frame
# Grouping by category and getting the total for each category
# sorting in descending order per total
category_expense <- expenses_dataset %>%
  group_by(category) %>%
  summarise(total = sum(price)) %>%
  arrange(desc(total))

# Creating the month_expense data frame
# Grouping by year and getting the total for each category
month_expense <- expenses_dataset %>%
  group_by(month) %>%
  summarise(total = sum(price))
# Converting months to name for easiness reading
month_expense$month <- months_name[as.integer(month_expense$month)]
# Creating vector of months in numeric value to get the trend line
numeric_months <- match(month_expense$month,month.abb)

# Creating the year_expense data frame
# Grouping by year and getting the total for each category
year_expense <- expenses_dataset %>%
  group_by(year) %>%
  summarise(total = sum(price))


#########################################
# Plotting the expense data
#########################################

# Plotting the category_expense data set
# Using columns (bars) 
# Ordering in descending total order using reorder()
# Filling each bar from green to red
ggplot(category_expense) + 
  geom_col(mapping = aes(x=reorder(category, -total), y = total, fill = total)) + 
  scale_fill_gradient(low = "green", high = "red", space = 'Lab') + 
  theme(axis.text.x = element_text(angle = 90, size = 11), axis.text.y = element_text(angle = 90, size = 11), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  labs(title = 'Expenses grouped by Category', subtitle = paste0("Data from: ", minyear, " to: ", maxyear), x = "Categories", y = 'Total (MXN)')

# Plotting the month_expense data set
# Using columns (bars)
# Filling each bar from green to red
ggplot(month_expense) + 
  geom_col(mapping = aes(x=fct_inorder(month), y=total, fill=total)) + 
  scale_fill_gradient(low="green", high="red", space='Lab') + 
  geom_smooth(aes(x=numeric_months, y=total), method=loess, color="blue", formula=y ~ x, se=FALSE ,linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 90, size = 11), axis.text.y = element_text(angle = 90, size = 11), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  labs(title = 'Expenses grouped by Month', subtitle = paste0("Data from: ", minyear, " to: ", maxyear, " with trend line using y=x and using the loesses method"), x = "Month", y = 'Total (MXN)')

# Plotting the year_expense data set
# Using columns (bars)
# Filling each bar from green to red
ggplot(year_expense) + geom_col(mapping = aes(x=year, y=total, fill=total)) + 
  scale_fill_gradient(low="green", high="red", space='Lab') + 
  geom_smooth(aes(x=year, y=total), method=glm, color="blue", formula=y ~ x, se=FALSE, linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 90, size = 11), axis.text.y = element_text(angle = 90, size = 11), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  labs(title = 'Expenses grouped by Year', subtitle = paste0("Data from: ", minyear, " to: ", maxyear, " with trend line using y=x and using the Generalized linear model"), x = "Year", y = 'Total (MXN)')

#########################################
# Some analysis
#########################################

# If we want to see why a specific category has a higher expense than the others
# Use the filter functions
# Or what stuff was bought for each category.
EDS_Filtered_category = filter(expenses_dataset, category == "House")
view(EDS_Filtered_category)

# If we want to see why a specific or specific months have a higher expense than the others
# Use the filter functions
# Remember that months are stored as numeric values.
EDS_Filtered_month = filter(expenses_dataset, month == 9 | month == 7)
view(EDS_Filtered_month)

# If we want to see why a specific or specific year have a higher expense than the others
# Use the filter functions
# Remember that years are stored as numeric values.
EDS_Filtered_year = filter(expenses_dataset, year == 2018)
view(EDS_Filtered_year)
