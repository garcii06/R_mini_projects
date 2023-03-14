# Libraries -----------------------------------------------------------------------------------
library(skimr)
library(readxl)
library(janitor)
library(lubridate)
library(tidyverse)

# ETL -----------------------------------------------------------------------------------------
## Data Extraction ----------------------------------------------------------------------------
Orders <- read_xlsx("Data/E-commerce.xlsx", sheet = 1) |> 
  select(-c(`Row ID`)) |> 
  clean_names()

Returns <- read_xlsx("Data/E-commerce.xlsx", sheet = 2) |> 
  clean_names()

People <- read_xlsx("Data/E-commerce.xlsx", sheet = 3) |> 
  clean_names()

## Data Transformation ------------------------------------------------------------------------
# No missing values.
# A total of 3312 orders, and 1687 unique orders.
# 4 Different ship modes.
# 693 customers.
# Multiple products with negative profit. Possible devolution, errors, or something else.
Orders |> 
  skim()

Orders |> 
  count()

# Total of 639 Orders with profit less than or equal to zero.
Orders |> 
  filter(profit <= 0) |> 
  view()

# Not all the negative profit records are returns.
# I will just assume that the profit was calculated by the owner, and that the returned products 
# profit was the profit calculated before it was returned.
Orders |> 
  left_join(Returns, by = c("order_id" = "order_id")) |> 
  filter(profit <= 0) |> 
  view()

## Data Loading -------------------------------------------------------------------------------
# As I do not modify at all the data (only remove the column `Row ID`, and rename columns) I will
# leave the sheets as they are.

# EDA -----------------------------------------------------------------------------------------
## Inspiration Questions ----------------------------------------------------------------------
# This questions were proposed in the original source, I will try to answer every question and plot
# the result in here, and maybe in SQL and Power BI. 

### Highest Sale in 2020? ---------------------------------------------------------------------
# The highest sale was made by Raymond Buch how bought 4 Canon imageCLASS 2200 Advanced Copier.
Orders |> 
  filter(sales == max(sales)) |> 
  view()

### Average discount rate of chairs? ----------------------------------------------------------
# The average discount for chairs is 0.167 or or 16.7%. With a total of 190 transactions involving
# chairs.
Orders |> 
  filter(sub_category == "Chairs") |> 
  summarise(avg_discount = mean(discount, na.rm = TRUE),
            total_sales = n())

### Highest selling months in 2020? -----------------------------------------------------------
Orders |> 
  filter(year(order_date) == 2020) |> 
  ggplot(aes(x = month(order_date), y = sales)) + 
  geom_col() +
  scale_x_continuous(breaks = c(1:12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "June",
                                "July", "Aug", "Sept", "Oct", "Nov", "Dec")) +
  labs(x = "Total Sales",
       y = "Month Number",
       title = "Monthly sales (2020)")

### Profit Margin for each sale? --------------------------------------------------------------
# We got scenarios where the profit margin is +50%, while also have several transactions with
# a margin of -275%.
Orders |> 
  mutate(profit_margin = (profit / sales) * 100,
         profit_margin = round(profit_margin, 2)) |> 
  arrange(desc(profit_margin)) |> 
  view()

### Profit gain for each product? -------------------------------------------------------------
# I want to believe that the profit gain is just the sum of all the products group by the name
# of the product. 
# I will add the total transactions with the product, and the quantity of pieces sold.
Orders |> 
  group_by(product_name) |> 
  summarise(total_profit = sum(profit),
            average_profit = mean(profit),
            total_transactions = n(),
            total_sold = sum(quantity)) |> 
  arrange(desc(total_profit))

### Total Profit & Sales by Sub-Category? -----------------------------------------------------
# The best subcategory is Copies as it has the highest profit with a low number of items sold.
# Bookcases, Supplies, Machines, and Tables have the worst Total profit and represent losses to the
# company. They are low sales, high quantity, low profit subcategories.
Orders |> 
  group_by(sub_category) |> 
  summarise(total_profit = sum(profit),
            total_sales = n(),
            total_sold = sum(quantity)) |> 
  arrange(desc(total_profit))

Orders |> 
  group_by(sub_category) |> 
  summarise(total_profit = sum(profit),
            total_sales = n(),
            total_sold = sum(quantity)) |> 
  ggplot(aes(x = reorder(sub_category, -total_profit), y = total_sales, fill = total_profit)) +
  geom_col() +
  guides(x = guide_axis(angle = 35)) +
  scale_fill_gradient(low = "#703d06",
                      high = "#063970") + 
  labs(x ="Product subcategory",
       y = "Total Sales",
       title = "Profit by Product Subcategory")

### People from city/state shop the most? -----------------------------------------------------
# California, New York and Washington are the most profitable states and have the highest amount of
# sales.
Orders |> 
  group_by(state) |> 
  summarise(total_profit = sum(profit),
            total_sales = n()) |> 
  arrange(desc(total_profit))

# New York City, Seattle, Los Angeles are the most profitable cities which we already know when we
# looked in the states.
Orders |> 
  group_by(city) |> 
  summarise(total_profit = sum(profit),
            total_sales = n()) |> 
  arrange(desc(total_profit))

# Looking into the State-city combinations.
Orders |> 
  group_by(state, city) |> 
  summarise(total_profit = sum(profit),
            total_sales = n()) |> 
  arrange(desc(total_profit), state)

## Other Questions ----------------------------------------------------------------------------
# Most frequent returned items. By Customer, State-City, Region.
# Total sells by Region.
# Most common Ship Mode by Region, Customer.
# Discount offered for the first order, and multiple orders.
# Dates between sales for each customer. 

