# Message: What is the most bought product in each country?

package_list = c("dplyr", "ggplot2", "tidyverse", "tidyr")

new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)){
  install.packages(new_packages, quiet=TRUE)
} 

sapply(package_list, library, character.only = TRUE)


folder = '~/Documents/Bioinformatics_Master/notes/semester_02_sose2025/BioDaten_meistern/data/'
files = list.files(folder, pattern = '0415')

sales = read.csv(paste0(c(folder, files), collapse = ""))
sales
dim(sales)                 
str(sales)
summary(sales)
anyNA(sales)

# if only showing the one product as scatter plot (like correlation): sparse, loss of information
# update df with new column that shows which is the rank 1 product?
# dotplot dot size as max number per row/country?

top_sales <- sales %>% 
  mutate("Top_Product" = case_when(
    A == pmax(A, B, C, D, E) ~ "A",
    B == pmax(A, B, C, D, E) ~ "B",
    C == pmax(A, B, C, D, E) ~ "C",
    D == pmax(A, B, C, D, E) ~ "D",
    E == pmax(A, B, C, D, E) ~ "E"
           )
  ) %>%
  rowwise() %>%
  mutate("Top_Sales" = max(c(A, B, C, D, E)))

ggplot(data = top_sales,
       mapping = aes(x = Top_Product, 
                     y = Country, 
                     size = Top_Sales, colour = Top_Sales)) +
  geom_point() +
  ggtitle('Dotplot of most popular products in each country')

## from the task title inferred: show them all, but only highlight rank 1s
# as a heatmap: convert df into long format suitable for heatmaps

sales_long <- pivot_longer(sales,
                           cols = 2:6,
                           names_to = "Product",
                           values_to = "Sales")
dim(sales_long)

ggplot(data = sales_long,
       mapping = aes(x = Product, y = Country, fill = Sales)) +
  geom_tile() +
  ggtitle('Heatmap of product sales in each country')
