library(tidyverse)
library(here)
library(gss)
library(Sleuth3)

crashes <- read_csv("https://opendata.arcgis.com/datasets/70248b73c20f46b0a5ee895fc91d6222_25.csv")
head(crashes)

ggplot(filter(crashes, AGE > 16 & AGE < 100 & FATAL == "Y")) +
  geom_bar(aes(AGE, fill=LICENSEPLATESTATE))

ggplot(ex1222) +
  geom_point(aes(x))

threeoneone <- read_csv("https://datagate.dc.gov/search/open/311requests?daterange=8years&details=true&format=csv")
head(threeoneone)

trafficrequests <- filter(threeoneone, SERVICECODEDESCRIPTION == c("roadway signs","streetlight repair investigation","pothole"))
head(trafficrequests)

crashes <- read_csv("https://opendata.arcgis.com/datasets/70392a096a8e431381f1f692aaa06afd_24.csv")
na.omit(crashes)
#crashes$FROMDATE <- as.Date(crashes$FROMDATE,
# format = "%y-%m/%d")

crashes2012 <- crashes %>% filter(as.Date(FROMDATE) >= "2012-01-01")

#trafficrequests %>%
# filter(YEAR >= as.Date("2012"))

#View(trafficrequests)


ggplot() +
  geom_point(data = trafficrequests, aes(x = ADDDATE), stat = "bin") +
  geom_smooth(trafficrequests, mapping = aes(x = ADDDATE), stat = "bin") +
  geom_point(data = crashes2012, aes(x = FROMDATE), stat = "bin") +
  geom_smooth(crashes2012, mapping = aes(x = FROMDATE), stat = "bin") +
  facet_wrap(~ WARD, nrow=3)

#anaemia <- read.table("http://jeffgill.org/files/jeffgill/files/anaemia.dat_.txt", header=TRUE, row.names=1)
#head(anaemia)