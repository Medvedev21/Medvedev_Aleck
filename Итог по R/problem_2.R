# Install and load the necessary packages
install.packages("dplyr")
library(dplyr)

# Load the data
transaction_data <- read.csv("transaction_data.csv")
product_data <- read.csv("product.csv")

# Merge tables based on the PRODUCT_ID field
merged_data <- inner_join(transaction_data, product_data, by = "PRODUCT_ID")

# Convert QUANTITY and SALES_VALUE columns to numeric format
merged_data$QUANTITY <- as.numeric(merged_data$QUANTITY)
merged_data$SALES_VALUE <- as.numeric(merged_data$SALES_VALUE)

# Top 10 products by quantity sold
top_products_quantity <- merged_data %>%
  group_by(PRODUCT_ID, COMMODITY_DESC) %>%
  summarise(total_quantity = sum(QUANTITY), total_sales = sum(SALES_VALUE)) %>%
  arrange(desc(total_quantity)) %>%
  head(10)

# Identify stores with low or no sales of the top 10 products
stores_with_low_sales <- merged_data %>%
  group_by(STORE_ID) %>%
  filter(all(!(PRODUCT_ID %in% top_products_quantity$PRODUCT_ID))) %>%
  summarise(total_sales = sum(SALES_VALUE)) %>%
  arrange(desc(total_sales)) %>%
  head(10)

# Print the results
print("Top 10 products by quantity sold with total sales value:")
print(top_products_quantity)

print("Top 10 stores with low or no sales of top 10 products:")
print(stores_with_low_sales)