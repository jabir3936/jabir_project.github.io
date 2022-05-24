# Data Science at TUHH es
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "ds_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("ds_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("ds_data/01_bike_sales/01_raw_data/bikeshops.xlsx")


# 3.0 Examining Data ----

orderlines_tbl

glimpse(orderlines_tbl)

# 4.0 Joining Data ----


left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

#Chaining commands with the pipe and assigning it to order_items_joined_tbl

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Examine the results with glimpse()

bike_orderlines_joined_tbl %>% select(location) %>% unique()

# 5.0 Wrangling Data ----
# All actions are chained with the pipe already. You can perform each step 
#separately and use glimpse() or View() to validate your code. 
#Store the result in a variable at the end of the steps.
  # 5.1 Separate category name to city and state
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% separate(col = location, into = c("City","State"), sep = (",")) %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity)  %>%
  
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
select(-ends_with(".id")) %>%
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
  

# 6.0 Business Insights ----
# 6.1 Sales by State ----

                                                                                                                                    
# Step 1 - Manipulate

sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>% select(State,total_price) %>% group_by(State) %>% 
  summarise(total_sales= sum(total_price))%>% 
  ungroup() %>% 
  mutate(sales_text = scales::dollar(total_sales, big.mark = ".",decimal.mark = ",", prefix = "",suffix = " €"))

# Step 2 - Visualize

sales_by_state_tbl %>% ggplot(aes(x = State, y = total_sales)) + geom_col(fill = "#2DC6D6") +geom_label(aes(label = sales_text)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",  decimal.mark = ",",prefix = "", suffix = " €")) +
  labs(  title    = "Revenue by State", y = "Revenue" ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 6.2 Sales by Location and Year

# Step 1 - Manipulate
# We have to group the data based on the location and the year which is the order dates

sales_by_styr_tbl <- bike_orderlines_wrangled_tbl %>% 
select(State,order_date,total_price)%>%
#Include another column for year that is the year when it was ordered
  mutate(year = year(order_date)) %>%

#Order the data by year and state and include the total sum of bikes.
  group_by(year,State)%>% 
  summarise(sales= sum(total_price)) %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize

  #Now to visualize each graph
  
  sales_by_styr_tbl %>%
    ggplot(aes(x = year, y = sales, fill = State)) +
    geom_col() +
    facet_wrap(~ State) +
    
    scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                      decimal.mark = ",", 
                                                      prefix = "", 
                                                      suffix = " €")) +
labs(
      title = "Revenue by Year and State",
      fill = "Main category" # Changes the legend name)
      )

# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----

