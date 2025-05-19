library(readxl)
library(tidyverse)

# load data and clean it
product_retail <- read_xlsx("../data/Dataset_stwd_0801.xlsx", col_names = TRUE)
names(product_retail)[1] <- "Product"
product_retail_clean <- product_retail %>%
  slice(-n()) %>%
  select(-2) %>%
  mutate(across(-Product, ~ as.numeric(gsub("\\$|\\.", "", gsub(",", ".", .)))))

# transform data to long format
product_long <- product_retail_clean %>%
  pivot_longer(-Product, names_to = "Year", values_to = "Price") %>%
  mutate(Year = as.numeric(Year))

# create plot
ggplot(product_long, aes(x = Year, y = Price, color = Product)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black") +
#  annotate("text", x = 2010, y = max(product_long$Price, na.rm = TRUE), label = "Launch Product C", vjust = -0.5, hjust = 0, angle = 90) +
  labs(title = "Price Trend since 2008",
       subtitle = "Vertical line marks the launch of Product C (2010)",
       x = "Year", y = "Price") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$"), limits = c(0, NA)) +
  theme_minimal()

