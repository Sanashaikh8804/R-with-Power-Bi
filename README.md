# R-with-Power-Bi

1. Chart: State, Sales and Discount
🧩 What the code does:
Categorizes transactions based on whether a discount was applied (With Discount vs Without Discount).
Calculates the average sales for each state under both categories.
Compares the difference in average sales to see where discounts were most effective.
Displays the top 5 states where discounts led to the highest increase in sales using a horizontal bar chart.

SCRIPT:
library(dplyr)
library(tidyr)
library(ggplot2)
sales <- dataset
state_discount_analysis <- sales %>%
  mutate(discount_given = ifelse(Discount > 0, "With Discount", "Without Discount")) %>%
  group_by(State, discount_given) %>%
  summarise(avg_sales = mean(Sales, na.rm = TRUE), .groups = "drop")
state_comparison <- state_discount_analysis %>%
  pivot_wider(names_from = discount_given, values_from = avg_sales) %>%
  mutate(increase = `With Discount` - `Without Discount`) %>%
  filter(!is.na(increase)) %>%
  arrange(desc(increase)) %>%
  slice_head(n = 5)  # Top 5 states
ggplot(state_comparison, aes(x = reorder(State, increase), y = increase)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 5 States Where Discount Increased Sales",
    x = "State",
    y = "Increase in Avg Sales (With - Without Discount)"
  ) +
  theme_minimal()

2. Chart:  Average Sales With vs. Without Discount by Category
🧩 What the code does:
Classifies each sale as either "With Discount" or "Without Discount".
Groups the data by category and discount status, then calculates:
The average sales
The number of records (count)
Visualizes the comparison using a grouped bar chart to show differences in average sales between the two discount statuses for each category.

SCRIPT:
library(dplyr)
library(ggplot2)
sales <- dataset
sales$Discount <- as.numeric(sales$Discount)
sales$Sales <- as.numeric(sales$Sales)
category_discount_analysis <- sales %>%
  mutate(discount_given = ifelse(Discount > 0, "With Discount", "Without Discount")) %>%
  group_by(Category, discount_given) %>%
  summarise(
    avg_sales = mean(Sales, na.rm = TRUE),
    count = n(),
    .groups = 'drop'  )
ggplot(category_discount_analysis, aes(x = Category, y = avg_sales, fill = discount_given)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Sales with vs. without Discount by Category",
    x = "Product Category",
    y = "Average Sales"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("With Discount" = "steelblue", "Without Discount" = "orange"))

3.Chart: Average Profit by Ship Mode
🧩 What the code does: 
Converts the Profit column to numeric (if needed).
Groups the data by Ship Mode and calculates the average profit for each mode.
Orders the results in descending order of average profit.
Visualizes the results using a horizontal bar chart.

SCRIPT:
library(dplyr)
library(ggplot2)
sales <- dataset
sales$Profit <- as.numeric(sales$Profit)
shipmode_profit <- sales %>%
  group_by(`Ship Mode`) %>%
  summarise(
    avg_profit = mean(Profit, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_profit))
ggplot(shipmode_profit, aes(x = reorder(`Ship Mode`, avg_profit), y = avg_profit)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Average Profit by Ship Mode",
    x = "Ship Mode",
    y = "Average Profit"
  ) +
  theme_minimal()

4.Chart: Correlation Heatmap of Sales Data
🔍 What the code does:
Selects numeric columns only from the dataset.
Removes rows with missing values to ensure accurate correlations.
Calculates the correlation matrix using cor() to see how strongly variables are related.
Plots a heatmap using ggcorrplot():
type = "lower": shows only the lower triangle for clarity.
lab = TRUE: displays correlation values inside squares.
Color scale from red (negative) to blue (positive) shows direction and strength.

SCRIPT:
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggcorrplot)
df <- dataset
numeric_data <- df %>% 
  select(where(is.numeric)) %>% 
  na.omit()  # remove rows with NA
cor_matrix <- cor(numeric_data)
ggcorrplot(cor_matrix, 
           method = "square", 
           type = "lower", 
           lab = TRUE,
           colors = c("red", "white", "blue"),
           title = "Correlation Heatmap")

5.Chart: Monthly Sales Trend Line Chart
🔧 Steps performed:
✅ Loads and cleans the dataset:
Converts column names to lowercase for consistency.
Checks and converts Order Date to proper Date format.
🗓️ Creates a new column month_year by rounding order dates to the first day of each month using floor_date() from the lubridate package.
📊 Groups data by month and calculates total sales per month.
📉 Plots a line chart with:
X-axis: Month-Year
Y-axis: Total Sales
A smooth line showing how sales trend over time

SCRIPT:
library(ggplot2)
library(dplyr)
library(lubridate)
sales <- dataset
colnames(sales) <- tolower(colnames(sales)) # Make it lowercase to avoid mismatch
if("order_date" %in% names(sales)){
  sales$order_date <- as.Date(sales$order_date)
} else if("order date" %in% names(sales)){
  sales$`order date` <- as.Date(sales$`order date`)
  sales$order_date <- sales$`order date`
}
df_monthly <- sales %>%
  filter(!is.na(order_date)) %>%
  mutate(month_year = floor_date(order_date, "month")) %>%
  group_by(month_year) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE))
ggplot(df_monthly, aes(x = month_year, y = total_sales)) +
  geom_line(color = "steelblue", size = 1.2) +
  labs(title = "Monthly Sales Trend", x = "Month-Year", y = "Total Sales") +
  theme_minimal()

6.Chart:  Yearly Sales Trend Analysis
🔍 What the script does:
🧹 Cleans column names to lowercase to avoid mismatches.
📅 Parses Order Date using multiple common date formats (ymd, dmy, mdy, etc.) with parse_date_time() to handle various date types safely.
✅ Ensures Sales is numeric and filters out rows with missing dates.
📆 Extracts year from each order date and calculates total sales for each year.
📈 Plots a line chart with:
X-axis: Year
Y-axis: Total sales
Line and points showing how sales changed year over year

SCRIPT:
library(ggplot2)
library(dplyr)
library(lubridate)
sales <- dataset
colnames(sales) <- tolower(colnames(sales))
if ("order date" %in% names(sales)) {
  sales$order_date <- parse_date_time(sales$`order date`, orders = c("ymd", "dmy", "mdy", "Ymd HMS"))
} else if ("order_date" %in% names(sales)) {
  sales$order_date <- parse_date_time(sales$order_date, orders = c("ymd", "dmy", "mdy", "Ymd HMS"))
}
sales$sales <- as.numeric(sales$sales)
sales <- sales %>% filter(!is.na(order_date))
df_yearly <- sales %>%
  mutate(order_year = year(order_date)) %>%
  group_by(order_year) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop")
ggplot(df_yearly, aes(x = order_year, y = total_sales)) +
  geom_line(color = "forestgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  labs(
    title = "Yearly Sales Trend",
    x = "Year",
    y = "Total Sales"
  ) +
  theme_minimal()

