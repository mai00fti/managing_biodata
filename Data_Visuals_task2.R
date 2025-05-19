library("paletteer")
library("tidyverse")
library("ggplot2")
library("readxl")

product_decline <- read_xlsx(
  path = "Dataset_stwd_0801.xlsx", # my data file
  range = "A1:I6",
  col_names = TRUE,
  col_types = c("text", "skip", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric"),# keep numbers instead of categories/factors for now
  trim_ws = TRUE
)

names(product_decline)[1] <- "Products"
product_decline[is.na(product_decline)] <- 0

#hier jetzt noch die Daten normalisieren, so dass der Gesamtpreis pro Anzahl Produkte vorliegt
product_decline_normalised <- product_decline
for (spaltenname in colnames(product_decline_normalised)) {
  spalte <- product_decline_normalised[[spaltenname]]
  # Nur fortfahren, wenn die Spalte numerisch ist (erste Spalte sind die Produktnamen)
  if (is.numeric(spalte)) {
    count_nonzero <- sum(spalte != 0)
    if (count_nonzero > 0) {
      product_decline_normalised[[spaltenname]] <- spalte / count_nonzero
    } 
  }
}

product_decline_long <- pivot_longer(
  data = product_decline_normalised,
  names_to = "years", # new column holding the original column names
  values_to = "price", # new column holding the actual values of the original cells
  cols = 2:8 # these columns will be extended towards a longer dataframe
)

product_decline_long$years <- as.numeric(product_decline_long$years)

ggplot(data = product_decline_long,
       aes(y = price, x = years, fill = Products))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(
    breaks = seq(min(product_decline_long$years), 
                 max(product_decline_long$years), 
                 by = 1)  # jedes Jahr
  ) +
  theme_classic()+
  scale_fill_paletteer_d("MetBrewer::Benedictus")+
  labs(
    title = "Average Retail Product Price per Year")+
  theme(plot.title=element_text(hjust=0.5)
        )+
  xlab("Year")+
  ylab("Averaged Price")
  


