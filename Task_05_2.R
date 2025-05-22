# Message: 

package_list = c("dplyr", "ggplot2", "readxl", "tidyr")

new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)){
  install.packages(new_packages, quiet=TRUE)
} 

sapply(package_list, library, character.only = TRUE)


folder = '~/Documents/Bioinformatics_Master/notes/semester_02_sose2025/BioDaten_meistern/data/'
files = list.files(folder, pattern = '0801.xlsx')

sales = read_excel(paste0(c(folder, files), collapse = ""))
sales
dim(sales)                 
str(sales)
summary(sales)
anyNA(sales)

sales <- sales[1:5,]
products <- sales[,1] %>% unlist()
products
sales <- sales[,3:9]
sales
# transpose row/columns

year_product <- t(sales)
year_product
dim(year_product)
length(products)
colnames(year_product) <- products
year_product
dim(year_product)

# correct values from character back into numeric
year_product <- as.data.frame(year_product)
class(year_product$`Product A`)

myNumeric <- function(df){
  for (i in (1:ncol(df))){
    df[,i] <- df[,i] %>% as.numeric()
  }
  return(df)
}

year_product <- myNumeric(year_product)
class(year_product$`Product C`)
# addas.numeric()# add average per year

year_product_average <- year_product %>%
  mutate("Average" = rowMeans(year_product, na.rm=TRUE)) %>%
  mutate("Year" = 2008:2014)
year_product_average

# convert to long format for plotting

results_long <- pivot_longer(year_product_average,
                           cols = 1:6,
                           names_to = "Products",
                           values_to = "Price")
dim(results_long)

##########

# Get last data point for each group
labels <- results_long %>%
  filter(!is.na(Price)) %>% 
  group_by(Products) %>%
  filter(Year == min(Year))

ggplot(data = results_long,
       mapping = aes(x = Year, 
                     y = Price,
                     colour = Products)) +
  geom_line() +
  ggtitle('Retail Price of Products per Year') +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)) +
  ylim(0,500) +
  ylab("Price [$]") +
  xlab("") +
  geom_vline(xintercept = 2010, linetype="dotted") +
  annotate("text", x=2009.6, y=130, label="Launch of", angle=0) +
  labs(caption = "***************") +
  theme(plot.caption = element_text(hjust=-0.5)) +
  geom_text(data = labels, aes(label = Products), 
                size = 3.5,
                hjust = 1, show.legend = FALSE) +
  expand_limits(x = min(results_long$Year) - 0.5)  # Extend space for labels

