library("tidyverse")
library(readxl)

# Daten einlesen
product_prices <- read_xlsx(
  "Dataset_stwd_0801.xlsx",
  col_names = TRUE,
  col_types = "text"
) %>%
  rename("Product" = "...1") %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(!is.na(Product))

# Daten vorbereiten
product_prices_long <- pivot_longer(
  data = product_prices,
  #cols = Product,
  names_to = "year", # new column holding the original column names
  values_to = "price", # new columns holding the actual values of the original cells
  cols = c("2008", "2009", "2010", "2011", "2012", "2013","2014") # these columns will be extended towards a longer data frame
  ) %>%
  mutate(
    "year" = as.integer(year),
    "price" = as.numeric(price))

#install.packages("ggrepel")  # falls noch nicht installiert
library(ggrepel)

# Daten fürs Label nur für das jeweils letzte Jahr eines Produkts
label_data <- product_prices_long %>%
  group_by(Product) %>%
  filter(year == max(year[!is.na(price)]))  # letztes Jahr mit Preis

# Plot erstellen mit direkter Beschriftung
p <- ggplot(product_prices_long, aes(x = year, y = price, color = Product)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_text_repel(
    data = label_data,
    aes(label = Product),
    nudge_x = 0.3,
    direction = "y",
    hjust = 0,
    segment.color = NA,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "grey50") +
  annotate("text", x = 2010.2, y = max(product_prices_long$price, na.rm = TRUE),
           label = "Launch of Product C", hjust = 1.3, vjust = 20, size = 3.5, color = "grey40") +
  labs(
    title = "Price development since the launch of Product C in 2010",
    subtitle = "Price has declined for all products on the market since the launch of product C in 2010",
    x = "Year",
    y = "Price"
  ) +
  scale_x_continuous(breaks = 2008:2014) +
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black"), # Achenlinien anzeigen
    panel.grid = element_blank(), # entfernt alle Gitterlinien
    legend.position = "none" # Legende ausblenden
  )

# Abbildung speichern
ggsave("figure_task02.png", p, width = 10, height = 6, dpi = 300)