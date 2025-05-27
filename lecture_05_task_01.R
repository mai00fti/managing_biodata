library("tidyverse")

product_ranks <- read_csv(
  file = "Dataset_stwd_0415.csv", # my data file
  col_names = TRUE,
  col_types = "cddddd" # keep numbers instead of categories/factors for now
)

product_ranks_long <- pivot_longer(
  data = product_ranks,
  names_to = "products", # new column holding the original column names
  values_to = "rank", # new column holding the actual values of the original cells
  cols = A:E # these columns will be extended towards a longer dataframe
) %>%
  mutate("rank_f" = as.factor(ifelse(rank < 5,
                                     yes = as.character(rank),
                                     no = "5+")))

ggplot(data = product_ranks_long,
       aes(x = products, y = Country, fill = rank_f)) +
  geom_tile() +
  scale_fill_manual(values = c("darkblue", "blue", "lightblue", "lightgreen", "white")) +
  labs(
    title = "",
    subtitle = "" # ...
  ) +
  theme_classic() +
  theme(
    legend.position = "top"
  )
