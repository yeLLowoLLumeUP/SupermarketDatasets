supermarket_aldi<- read.csv("All_Data_Aldi.csv")
supermarket_ASDA <- read.csv("All_Data_ASDA.csv")
supermarket_Morrisons <- read.csv("All_Data_Morrisons.csv")
supermarket_tesco <- read.csv("All_Data_Tesco.csv")

#bozuk karakterleri düzeltme(.csv dosyası içindende halledilebilir.)
names(supermarket_sains) <- iconv(names(supermarket_sains), from = "ISO-8859-9", to = "UTF-8")

supermarket_sains <- read.csv("All_Data_Sains.csv")

library(ggplot2)

head(supermarket_sains)

View(supermarket_sains)

#date kolonunu datetime'a çevirme.
supermarket_sains$date <- as.Date(as.character(supermarket_sains$date), format = "%Y%m%d")

baby_products_plot <- ggplot(supermarket_sains = baby_products, aes (x = prices)) + 
  geom_histogram(binwidth = 1)

#benzersiz kategorileri listeleme.
unique(supermarket_sains$category)

#hangi kategoride kaç ürün var?
table(supermarket_sains$category)

sum(is.na(supermarket_sains))

library(ggplot2)

ggplot(supermarket_sains, aes(x = category, fill = category)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  theme_dark() +
  theme(legend.position = "none")
  

#baby products ürünlerinin ortalamasını alma.

library(dplyr)

supermarket_sains %>%
  filter(category == "baby_products") %>%
  summarise(ortalama_fiyat = mean(prices, na.rm = TRUE))

supermarket_sains %>%
  filter(category == "pets") %>%
  summarise(ortalama_fiyat = mean(prices, na.rm = TRUE))

aggregate(prices ~ category, data = supermarket_sains, FUN = mean)



#tüm marketlerin fiyat ortalamasını alalım.
supermarket_aldi$date <- as.Date(as.character(supermarket_aldi$date), format = "%Y%m%d")
supermarket_ASDA$date <- as.Date(as.character(supermarket_ASDA$date), format = "%Y%m%d")
supermarket_Morrisons$date <- as.Date(as.character(supermarket_Morrisons$date), format = "%Y%m%d")
supermarket_tesco$date <- as.Date(as.character(supermarket_tesco$date), format = "%Y%m%d")
supermarket_sains$date <- as.Date(as.character(supermarket_sains$date), format = "%Y%m%d")

library(dplyr)

all_supermarkets <- bind_rows(
  supermarket_aldi,
  supermarket_ASDA,
  supermarket_Morrisons,
  supermarket_tesco,
  supermarket_sains
)


#marketlerin kaç tanesi kendi ürününü satıyor?(own_brand)

tables(supermarket_sains$own_brand)
#right_skewed
supermarket_sains %>%
  filter(category == "pets") %>% 
  summarise(
    ortalama = mean(prices, na.rm = TRUE),
    medyan = median(prices, na.rm = TRUE),
    Urun_sayisi = n()
  )