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
  group_by(month(order_date)) |> 
  summarise(total_sales = sum(sales)) |>  
  ggplot(aes(x = `month(order_date)`, y = total_sales)) + 
  geom_col(aes(fill = total_sales)) +
  scale_x_continuous(breaks = c(1:12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "June",
                                "July", "Aug", "Sept", "Oct", "Nov", "Dec")) + 
  labs(x = "",
       y = "Total Sales",
       title = "Monthly sales",
       fill = "Sales") +
  scale_fill_gradient2(low = "#3A5100",
                       mid = "#51003A",
                       high = "#003A51",
                       midpoint = 60000)

ggsave("Monthly sales.png", path = str_c(getwd(), "/Plots"))

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
  scale_fill_gradient2(low = "#3A5100",
                      mid = "#51003A",
                      high = "#003A51",
                      midpoint = 10000,
                      breaks = c(0, 10000 , 20000)) + 
  labs(x ="Product subcategory",
       y = "Total Sales",
       title = "Profit by Product Subcategory")

ggsave("Profit by Subcategory.png", path = str_c(getwd(), "/Plots"))

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
### Returned Items ----------------------------------------------------------------------------
# The most frequent orders returned by Region.
# West suffers the most from returns.
Returns |> 
  inner_join(Orders, by = c("order_id" = "order_id")) |> 
  group_by(region) |> 
  summarise(orders_returned = n()) |> 
  ggplot(aes(x = region, y = orders_returned)) +
  geom_col() +
  labs(x = "Region",
       y = "Total orders returned",
       title = "Orders returned by Region")

# Total items returned by region.
Returns |> 
  inner_join(Orders, by = c("order_id" = "order_id")) |> 
  group_by(region) |> 
  summarise(items_returned = sum(quantity)) |> 
  ggplot(aes(x = region, y = items_returned)) +
  geom_col() +
  labs(x = "Region",
       y = "Total items returned",
       title = "Items returned by Region")

# Name of the product that was returned. All of the products were returned less than 5 times.
Returns |> 
  inner_join(Orders, by = c("order_id" = "order_id")) |> 
  group_by(product_name) |> 
  summarise(items_returned = n()) |> 
  arrange(desc(items_returned))

# Categories of the products that were returned.
# Office Supplies is the category which is most often returned.
Returns |> 
  inner_join(Orders, by = c("order_id" = "order_id")) |> 
  group_by(category) |> 
  summarise(items_returned = n()) |> 
  arrange(desc(category))

### Sales by Region ---------------------------------------------------------------------------
Orders |> 
  group_by(region) |> 
  summarise(total_profit = sum(profit),
            total_orders = n(),
            total_sales = sum(quantity),
            profit_margin = total_profit / total_sales)


### Sales by Ship Mode ------------------------------------------------------------------------
Orders |> 
  group_by(ship_mode, category) |> 
  summarise(total_orders = n()) |> 
  ggplot(aes(x = ship_mode, y = total_orders)) +
  geom_col(aes(fill = category), position = "dodge") +
  scale_fill_manual(values = c("#3A5100", "#51003A", "#003A51")) +
  labs(x = "Ship Mode",
       y = "Total Orders",
       title = "Total Orders by Ship Mode",
       fill = "Product Category")

ggsave("Orders by Ship Mode.png", path = str_c(getwd(), "/Plots"))