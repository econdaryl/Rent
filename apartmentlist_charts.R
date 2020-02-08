library(readr)
library(dplyr)
library(tidyr)
library(rlist)

setwd("/href/scratch3/m1dbl01/Personal/rent/")

download.file(url = "https://apartmentlist.com/rentonomics/wp-content/uploads/2020/01/Apartment-List-Rent-Data-National_2020-1.csv", "apartmentlist_national_historical.csv")
download.file(url = "https://apartmentlist.com/rentonomics/wp-content/uploads/2020/01/Apartment-List-Rent-Data-City_2020-1.csv", "apartmentlist_city_historical.csv")

data <- read_csv("apartmentlist_national_historical.csv")

data <- data %>%
  pivot_longer(cols = contains("Price"))

studio <- tis(data$value[data$Bedroom_Size=='Studio'], start = 20140101, frequency = 12)
br1 <- tis(data$value[data$Bedroom_Size=='1br'], start = 20140101, frequency = 12)
br2 <- tis(data$value[data$Bedroom_Size=='2br'], start = 20140101, frequency = 12)
br3 <- tis(data$value[data$Bedroom_Size=='3br'], start = 20140101, frequency = 12)
br4 <- tis(data$value[data$Bedroom_Size=='4br'], start = 20140101, frequency = 12)

rplot.line(list(studio, br1, br2, br3, br4))

data2 <- read_csv("apartmentlist_city_historical.csv")
data2 <- data2 %>%
  pivot_longer(cols = contains("Price"))

studio_msa <- data2[data2$Bedroom_Size=='Studio',] %>%
  group_by(name) %>%
  summarize(median = median(value, na.rm = TRUE),
            p25    = quantile(value, probs = .25, na.rm = TRUE),
            p75    = quantile(value, probs = .75, na.rm = TRUE)) %>%
  mutate(year = substr(name, start = 7, stop = 10),
         month =substr(name, start = 12, stop = 13),
         date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")) %>%
  select(median, p25, p75, date)

plotlist <- df2tislist(studio_msa, "date", 12)
plotlist <- list.append(plotlist, studio)

pinfo <- rplot.line(plotlist,
           Title = "Studio Apartments",
           Y2lab = "Dollars",
           Freqlab2 = "Monthly",
           footvec = c("Source: ApartmentList"))

text(x = 2015, y = 990, labels = "75th Percentile", cex = 0.7, col = "blue")
text(x = 2015, y = 675, labels = "Median", cex = 0.7, col = "black")
text(x = 2015, y = 550, labels = "25th Percentile", cex = 0.7, col = "red")
text(x = 2016, y = 840, labels = "National Average", cex = 0.7, col = "darkgreen")
