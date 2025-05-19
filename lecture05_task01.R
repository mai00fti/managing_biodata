library("tidyverse")

# Daten einlesen
product_ranks <- read_csv(
  file = "Dataset_stwd_0415.csv",
  col_names = TRUE,
  col_types = "cddddd" # keep numbers instead of categories/factores for now
)

# Distanz matrix erstellen
rank_matrix <- product_ranks %>%
column_to_rownames("Country") %>%
as.matrix()

# paarweise Distanzen berechnen
euc_dist_matrix <- dist(rank_matrix, method = "euclidean")
hier_cluster <- hclust(euc_dist_matrix, method = "complete")
ordered_countries <- hier_cluster$labels[hier_cluster$order]

# Daten vorbereiten
product_ranks_long <- pivot_longer(
  data = product_ranks,
  names_to = "products", # new column holding the original column names
  values_to = "rank", # new columns holding the actual values of the original cells
  cols = A:E # these columns will be extended towards a longer data frame
) %>%
  mutate("rank_f" = as.factor(ifelse(rank < 5,
                                     yes = as.character(rank),
                                     no = "5+"))
  ) %>%
  mutate("Country" = factor(Country, levels = ordered_countries))

# Plot erstellen
heatmap_plot <- ggplot(data = product_ranks_long,
       aes(x = products, y = Country, fill = rank_f)) +
  geom_tile() +
  scale_fill_brewer(name = "rank", palette = "Greens", direction = -1 )+
  labs(
    title = "Product rankings across selected countries",
    subtitle = "Product A and B sell best in most of the countries", # ...
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

#install.packages("ggdendro") # fall noch nicht installiert
library(ggdendro)

dendro_data <- ggdendro::dendro_data(hier_cluster)

# Dendrogramm Plot erstellen
dendro_plot <- ggplot(segment(dendro_data)) +
  geom_segment(aes(x = y, y = x, xend = yend, yend = xend)) +
  scale_y_discrete(limits = hier_cluster$labels[hier_cluster$order]) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"))

#install.packages("patchwork") # wenn noch nicht installiert
library(patchwork)

# Plots zusammen führen
p = heatmap_plot + dendro_plot + plot_layout(widths = c(3, 2))

# Abbildung speichern
ggsave("figure_task01.png", p, width = 10, height = 6, dpi = 300)