library("tidyverse")
library(dplyr)

# Load the data
product_ranks <- read_csv(
  file = "../../data/Dataset_stwd_0415.csv", # my data file
  col_names = TRUE,
  col_types = "cddddd" # keep numbers instead of categories/factors for now
)


# Create a long format of the data
product_ranks_long <- pivot_longer(
  data = product_ranks,
  names_to = "products", # new column holding the original column names
  values_to = "rank", # new column holding the actual values of the original cells
  cols = A:E # these columns will be extended towards a longer dataframe
) %>% mutate("rank_f" = as.factor(
  case_when(
    rank < 5 ~ as.character(rank),
    rank >= 5 & rank <= 6 ~ "5-6",
    rank >= 7 & rank <= 9 ~ "7-9",
    rank >= 10 & rank <= 12 ~ "10-12",
    TRUE ~ "12+"
  )
))

# Compute average rank per country
country_order <- product_ranks_long %>%
  group_by(Country) %>%
  summarise(avg_rank = mean(rank, na.rm = TRUE)) %>%
  arrange(avg_rank) %>%
  pull(Country)

# Then set Country as factor with this order
product_ranks_long <- product_ranks_long %>%
  mutate(Country = factor(Country, levels = country_order))


# Convert rank_f to a factor with ordered levels
product_ranks_long$rank_f <- factor(
  product_ranks_long$rank_f,
  levels = c("1", "2", "3", "4", "5-6", "7-9", "10-12", "12+")
)

# plot the data
ggplot(product_ranks_long, aes(x = products, y = reorder(Country, desc(Country)), fill = rank_f)) +
  geom_tile(color = "white", width = 0.95, height = 0.95) +
  geom_text(aes(label = rank), color = "black", size = 3.5) +
  scale_fill_manual(
    values = c(
      "1" = "#1a9850",      # dark green
      "2" = "#66bd63",
      "3" = "#a6d96a",
      "4" = "#d9ef8b",
      "5-6" = "#fee08b",
      "7-9" = "#fdae61",
      "10-12" = "#f46d43",
      "12+" = "#d73027"     # red
    ),
    name = "Rank Group"
  ) +
  labs(
    title = "Product Rankings by Country",
    x = "Product",
    y = "Country",
    caption = "Color encodes rank: green = best, red = worst"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0),
    legend.position = "none",
    legend.direction = "horizontal",
    plot.title.position = "plot",     # enables full-width title
    plot.caption.position = "plot",   # same for caption
    plot.title = element_text(hjust = 0.5),   # centered
    plot.caption = element_text(hjust = 0.5)  # centered
  )