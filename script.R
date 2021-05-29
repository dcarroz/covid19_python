{
  if(!require(EpiEstim)) install.packages("EpiEstim")
  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(ggpmisc)) install.packages("ggpmisc")
  if(!require(ggrepel)) install.packages("ggrepel")
  if(!require(caret)) install.packages("caret")
  if(!require(data.table)) install.packages("data.table")
  if(!require(lubridate)) install.packages("lubridate")
  if(!require(tools)) install.packages("tools")
  if(!require(scales)) install.packages("scales")
  if(!require(RCurl)) install.packages("RCurl")
  if(!require(utils)) install.packages("utils")
  if(!require(gridExtra)) install.packages("gridExtra")
  if(!require(maps)) install.packages("maps")
  if(!require(mapdata)) install.packages("mapdata")
  if(!require(RColorBrewer)) install.packages("RColorBrewer")
  if(!require(countrycode)) install.packages("countrycode")
  if(!require(tidytext)) install.packages("tidytext")
  if(!require(readxl)) install.packages("readxl")
  if(!require(corrplot)) install.packages("corrplot")
  if(!require(zoo)) install.packages("zoo")
}

# -------------
# INTRODUCTION
# -------------

## Datasets Description
## ---------------------

### Pandemic Data

#### Johns Hopkins University - Center for Systems Science and Engineering (CSSE)

csse_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", fileEncoding = "UTF-8-BOM")
head(csse_data[,1:8],5)

rm(csse_data)


### Demographic data

#### Population

# Get wpp2019 data (United Nations)
pop.2020 <- read.csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv",
                     fileEncoding = "UTF-8-BOM") %>%
  filter(Time == 2020 & VarID == 2)

head(pop.2020)


#### Geographical Position

##### Countries
Isocode <- read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv",
                    fileEncoding = "UTF-8-BOM") %>%
  mutate(country.name = as.character(Country),
         country.alphacode = str_remove(as.character(Alpha.3.code)," ")) %>%
  rename(country.isocode = Numeric.code,
         latitude = Latitude..average.,
         longitude = Longitude..average.) %>%
  select(country.name, country.alphacode, country.isocode, latitude, longitude)

head(Isocode)


##### Continents and Regions

library(countrycode)
countrycode(sourcevar = c("USA", "ITA", "DZA", "IND", "AUS"), 
            origin = "iso3c", destination = "continent")

countrycode(sourcevar = c("USA", "ITA", "DZA", "IND", "AUS"), 
            origin = "iso3c", destination = "region23")


### Measures data

#### ACAPS Dataset

tmp <- tempfile(fileext = ".xlsx")
download.file(url = "https://www.acaps.org/sites/acaps/files/resources/files/acaps_covid19_government_measures_dataset_0.xlsx", 
              destfile = tmp,
              mode='wb')
measures <- read_xlsx(path = tmp, sheet = "Dataset")

head(measures[1:8])

unique(measures$CATEGORY) 
unique(measures$MEASURE) 

rm(tmp)


### Test Positivity Rate

#### Our World Data Dataset

owd_data <- read.csv("https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv",
                     fileEncoding = "UTF-8-BOM") %>%
  mutate(date = as.Date(date)) %>%
  rename(country.alphacode = iso_code)

tail(select(owd_data, country.alphacode, date, positive_rate))


# --------- 
# ANALYSIS
# ---------

## Data Cleaning
## --------------

### CSSE: Covid data

# Read CSSE data from github
read_csse <- function(file, firstcols, name_col) {
  df_data <- read.csv(str_c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series",
                            "/",file), fileEncoding = "UTF-8-BOM")
  # change de wide shape to long shape
  df_data <- reshape(data = df_data, 
                     direction = "long", 
                     varying = list(firstcols:ncol(df_data)), 
                     times = names(df_data)[firstcols:ncol(df_data)], 
                     timevar = "date", 
                     v.names = name_col) %>%
    mutate(date = mdy(str_remove(date,"X"))) %>%
    select(-id) 
  row.names(df_data) <- NULL
  df_data
}

# get current CSSE data
covid19 <-
  # Acumulated Infected cases (I.cum)
  read_csse("time_series_covid19_confirmed_global.csv", 5, "I.cum") %>% 
  # Acumulated Deaths (D.cum)
  left_join(read_csse("time_series_covid19_deaths_global.csv", 5, "D.cum")) %>%
  # Acumulated Recovered (R.c)
  left_join(read_csse("time_series_covid19_recovered_global.csv", 5, "R.cum"))

# only countries
covid19 <- covid19 %>%
  group_by(Country.Region, date) %>%
  summarise(I.cum = sum(I.cum, na.rm = T), 
            D.cum = sum(D.cum, na.rm = T), 
            R.cum = sum(R.cum, na.rm = T)) %>%
  ungroup() 

# I, D, R
covid19 <- covid19 %>%
  group_by(Country.Region) %>%
  mutate(I = ifelse(date == first(date), I.cum, 
                    I.cum - lag(I.cum, order_by = date)),
         D = ifelse(date == first(date), D.cum, 
                    D.cum - lag(D.cum, order_by = date)),
         R = ifelse(date == first(date), R.cum, 
                    R.cum - lag(R.cum, order_by = date))) %>%
  mutate(country.name = str_remove(as.character(Country.Region),"[*]")) %>%
  ungroup() %>%
  select(country.name, date, 
         I, D, R, I.cum, D.cum, R.cum)

covid19 %>% 
  filter(country.name %in% c("Diamond Princess", "MS Zaandam")) %>%
  distinct(country.name) 

covid19 <- covid19 %>%
  filter(!(country.name %in% c("Diamond Princess", "MS Zaandam")))

head(covid19)
tail(covid19)


### Iso Codes

# Countries with other names
covid19 %>%
  anti_join(Isocode, by = "country.name") %>%
  distinct(country.name) 

# Update countries' names
covid19 <- covid19 %>% 
  mutate(country.name = case_when(
    country.name == "Cabo Verde" ~ "Cape Verde",
    country.name == "Congo (Brazzaville)" ~ "Congo",
    country.name == "Congo (Kinshasa)" ~ "Congo, the Democratic Republic of the",
    country.name == "Cote d'Ivoire" ~ "Ivory Coast",
    country.name == "Czechia" ~ "Czech Republic",
    country.name == "Eswatini" ~ "Swaziland",
    country.name == "Holy See" ~ "Holy See (Vatican City State)",
    country.name == "Iran" ~ "Iran, Islamic Republic of",
    country.name == "Korea, South" ~ "South Korea",
    country.name == "Laos" ~ "Lao People's Democratic Republic",
    country.name == "Moldova" ~ "Moldova, Republic of",
    country.name == "North Macedonia" ~ "Macedonia, the former Yugoslav Republic of",
    country.name == "Syria" ~ "Syrian Arab Republic",
    country.name == "Tanzania" ~ "Tanzania, United Republic of",
    country.name == "US" ~ "United States",
    country.name == "West Bank and Gaza" ~ "Palestinian Territory, Occupied",
    country.name == "Micronesia" ~ "Micronesia, Federated States of",
    TRUE ~ country.name)
  )

Isocode <- rbind(Isocode, list("Kosovo","XXK", 383, 42.6675, 21.1662)) 


### ACAPS: Measure data

read_acaps <- function() {
  tmp <- tempfile(fileext = ".xlsx")
  download.file(url = "https://www.acaps.org/sites/acaps/files/resources/files/acaps_covid19_government_measures_dataset_0.xlsx", 
                destfile = tmp,
                mode='wb')
  read_xlsx(path = tmp, sheet = "Dataset") %>%
    mutate(date = as.Date(DATE_IMPLEMENTED)) %>%
    rename(country.alphacode = ISO, 
           measure.category = CATEGORY, 
           measure.name = MEASURE) %>%
    select(country.alphacode, date, measure.name, measure.category)
}

measures_category <- data.frame(measure.category = 
                                  c("None",
                                    "Humanitarian exemption",
                                    "Governance and socio-economic measures",
                                    "Public health measures",
                                    "Social distancing",
                                    "Movement restrictions",
                                    "Lockdown"
                                  ),
                                measure.level = 0:6)

measures_category

measures <- read_acaps() %>% left_join(measures_category)

head(measures)


### Population data

pop.2020 <- pop.2020 %>%
  mutate(country.name = as.character(Location),
         country.isocode = as.numeric(LocID)) %>%
  select(country.name, country.isocode, PopTotal)

head(pop.2020)


## Data Exploration
## -----------------

### Add Isocode to covid19

covid19 <- covid19 %>%
  left_join(Isocode)

head(covid19)

covid19 <- covid19 %>%
  mutate(continent.name = countrycode(sourcevar = country.alphacode, 
                                      origin = "iso3c", destination = "continent"),
         region.name = countrycode(sourcevar = country.alphacode, 
                                   origin = "iso3c", destination = "region23"))

unique(covid19$country.name[which(is.na(covid19$continent.name))])
unique(covid19$country.name[which(is.na(covid19$region.name))])

covid19$continent.name[which(covid19$country.name == "Kosovo")] <- "Europe"
covid19$region.name[which(covid19$country.name == "Kosovo")] <- "Southern Europe"
covid19$region.name[which(covid19$country.name == "Western Sahara")] <-"Sub-Saharan Africa"


### Add Population to covid19

# Add Population to covid19
covid19 <- covid19 %>%
  left_join(pop.2020, by = "country.isocode") %>% 
  mutate(country.name = country.name.x,
         N = PopTotal*1000) %>%
  select(-country.name, -country.name.y, -PopTotal) %>%
  rename(country.name = country.name.x)

filter(covid19, date == last(date)) %>%
  anti_join(pop.2020, by = "country.isocode") %>%
  .$country.name 

# Sudan and Kosovo Population
covid19$N[which(covid19$country.name == "Sudan")] <- 
  pop.2020$PopTotal[which(pop.2020$country.name == "Sudan")]
covid19$N[which(covid19$country.name == "Kosovo")] <- 1811285


### Add Measures level to covid19

covid19 <- covid19 %>%
  group_by(country.alphacode, date) %>%
  mutate(measure.level = max(0, measures$measure.level[
    which(measures$country.alphacode == country.alphacode & 
            measures$date <= date)])) %>%
  ungroup()


### Add Test Positive rate

covid19 <- covid19 %>%
  left_join(select(owd_data, country.alphacode, date, positive_rate)) %>%
  mutate(TPR = positive_rate*100) %>%
  select(-positive_rate)

rm(owd_data)


### Add Variables

#### Add S (susceptible)

# N = S + I + R; S = N - I - R
covid19 <- covid19 %>% 
  mutate(S = N - I.cum - R.cum)

#### Add IR (Incidence Rate)

covid19 <- covid19 %>% 
  mutate(IR = (I.cum / N) * 10^5)

#### Add CFR (Confirmed Fatality rate) and M (Mortality)

covid19 <- covid19 %>%
  mutate(CFR = ifelse(I.cum == 0, 0, D.cum / I.cum)*100, M = (D.cum / N)*10^5 )

head(covid19[1:10])
head(covid19[11:20])

tail(covid19[1:10])
tail(covid19[11:20])


## Visualization
## --------------

### Current situation

# Show the current situation on the world map
covid19 %>% 
  filter(date == last(date)) %>%
  select(country.name, date, I.cum, D.cum, N, IR, CFR, measure.level) %>%
  head()

# World Map
library(mapdata)

map_data("world") %>%
  rename(country.name = region) %>%
  head(20)

# Countries with different names in map.world 
filter(covid19,date == last(date)) %>%
  anti_join(
    map_data("world") %>%
      rename(country.name = region),
    by = "country.name") %>%
  distinct(country.name)

# Countries with different names in covid19
map_data("world") %>%
  rename(country.name = region) %>%
  anti_join(filter(covid19,date == last(date)), 
            by = "country.name") %>%
  distinct(country.name) 

map.world <- map_data("world") %>%
  rename(country.name = region) %>%
  mutate(country.name = case_when(
    country.name == "Democratic Republic of the Congo" ~ "Congo, the Democratic Republic of the",
    country.name == "Iran" ~ "Iran, Islamic Republic of",
    country.name == "Laos" ~ "Lao People's Democratic Republic",
    country.name == "Macedonia" ~ "Macedonia, the former Yugoslav Republic of",
    country.name == "Moldova" ~ "Moldova, Republic of",
    country.name == "Myanmar" ~ "Burma",
    country.name == "Palestine" ~ "Palestinian Territory, Occupied",
    country.name == "Republic of Congo" ~ "Congo",
    country.name == "Syria" ~ "Syrian Arab Republic",
    country.name == "Tanzania" ~ "Tanzania, United Republic of",
    country.name == "Vatican" ~ "Holy See (Vatican City State)",
    country.name == "UK" ~ "United Kingdom",        
    country.name == "USA" ~ "United States",
    country.name == "Antigua" ~ "Antigua and Barbuda",
    country.name == "Barbuda" ~ "Antigua and Barbuda",
    country.name == "Saint Kitts" ~ "Saint Kitts and Nevis",
    country.name == "Nevis" ~ "Saint Kitts and Nevis",
    country.name == "Grenadines" ~ "Saint Vincent and the Grenadines",
    country.name == "Saint Vincent" ~ "Saint Vincent and the Grenadines",
    country.name == "Trinidad" ~ "Trinidad and Tobago",
    country.name == "Tobago" ~ "Trinidad and Tobago",
    TRUE ~ country.name)
  )

# World map of Infected by Covid-19
map.world %>%
  left_join(filter(covid19,date == last(date)), by = "country.name") %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = I.cum), color = "steelblue2", size = 0.1) + 
  scale_fill_gradient(low = "white", high = "yellow3",
                      breaks = 10^(1:7), trans = "log10", 
                      na.value = "white", labels = comma) +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(
          colour= "black", fill = "lightblue", size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6, ),
        legend.title = element_text(size = 8)) +
  ggtitle("Current situation: World map of infected by Covid-19") +
  labs(fill = "Infected")

# World map of Deaths by Covid-19
map.world %>%
  left_join(filter(covid19,date == last(date)), by = "country.name") %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = D.cum), color = "steelblue2", size = 0.1) + 
  scale_fill_gradient(low = "white", high = "red3",
                      breaks = 10^(1:7), trans = "log10", 
                      na.value = "white", labels = comma) +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(
          colour= "black", fill = "lightblue", size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  ggtitle("Current situation: World map of deaths by Covid-19") +
  labs(fill = "Deaths")


### Progress of the disease

# Infected
merge(map.world,
      data.frame(date = as.Date(seq(first(covid19$date), last(covid19$date), 60)))) %>%
  left_join(select(filter(covid19, 
                          date %in% seq(first(covid19$date), last(covid19$date), 60)),
                   country.name, date, I.cum)) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = I.cum), color = "steelblue2", size = 0.1) + 
  scale_fill_gradient(low = "white", high = "yellow3",
                      breaks = 10^(1:7), trans = "log10", 
                      na.value = "white", labels = comma) +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(
          colour= "black", fill = "lightblue", size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  ggtitle("Progress of the disease: World map of Infected by Covid-19") +
  labs(fill = "Infected") +
  facet_wrap(~ month(date, label = TRUE, abbr = FALSE), ncol = 2) 

# Deaths
merge(map.world,
      data.frame(date = as.Date(seq(first(covid19$date), last(covid19$date), 60)))) %>%
  left_join(select(filter(covid19, 
                          date %in% seq(first(covid19$date), last(covid19$date), 60)),
                   country.name, date, D.cum)) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = D.cum), color = "steelblue2", size = 0.1) + 
  scale_fill_gradient(low = "white", high = "red3",
                      breaks = 10^(1:7), trans = "log10", 
                      na.value = "white", labels = comma) +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(
          colour= "black", fill = "lightblue", size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  ggtitle("Progress of the disease: World map of Deaths by Covid-19") +
  labs(fill = "Infected") +
  facet_wrap(~ month(date, label = TRUE, abbr = FALSE), ncol = 2) 

# daily Infected
map.world %>%
  ggplot() + 
  geom_polygon(aes(x= long, y = lat, group = group),
               fill = "white", color = "steelblue2") + 
  geom_point(data = filter(covid19,date %in% 
                             as.Date(seq(ceiling_date(first(covid19$date), "month"), 
                                         last(covid19$date), by = "2 month"))), 
             aes(longitude, latitude, size = I), 
             color = "yellow3", alpha = 0.8) +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(
          colour= "black", fill = "lightblue", size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  labs(title = "World map of Infected by Covid-19", size = "Amount of Infected") +
  facet_wrap(~ month(date, label = TRUE, abbr = FALSE), ncol = 2)

# daily Deaths
map.world %>%
  ggplot() + 
  geom_polygon(aes(x= long, y = lat, group = group),
               fill = "white", color = "steelblue2") + 
  geom_point(data = filter(covid19,date %in% 
                             as.Date(seq(ceiling_date(first(covid19$date), "month"), 
                                         last(covid19$date), by = "2 month"))), 
             aes(longitude, latitude, size = D), 
             color = "red3", alpha = 0.8) +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(
          colour= "black", fill = "lightblue", size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  labs(title = "World map of Deaths by Covid-19", size = "Amount of Deaths") +
  facet_wrap(~ month(date, label = TRUE, abbr = FALSE), ncol = 2)


# Progress of infections and deaths from the pandemic.
grid.arrange(
  # Infected
  covid19 %>% 
    group_by(date) %>%
    summarise(I = sum(I)) %>%
    ggplot() +
    geom_area(aes(x = date, y = I), size = 1, colour = "yellow3", 
              fill = "yellow", alpha = 0.5) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = "daily Infected Values", x = "Months", y = "Infected") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title.x = element_text(size = 10, color = "black", face = "bold"),
          axis.title.y = element_text(size = 10, color = "black", face = "bold"),
          title = element_text(size = 12, color = "yellow4", face = "bold")) , 
  # Deaths    
  covid19 %>% 
    group_by(date) %>%
    summarise(D = sum(D)) %>%
    ggplot() +
    geom_area(aes(x = date, y = D), size = 1, colour = "red3", 
              fill = "red", alpha = 0.5) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = "daily Deaths Values", x = "Months", y = "Deaths") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title.x = element_text(size = 10, color = "black", face = "bold"),
          axis.title.y = element_text(size = 10, color = "black", face = "bold"),
          title = element_text(size = 12, color = "red4", face = "bold")),
  # IR
  covid19 %>% 
    group_by(date) %>%
    summarise(IR = (sum(I.cum) / sum(N))*10^5) %>%
    ggplot() +
    geom_area(aes(x = date, y = IR), size = 1, colour = "yellow3", 
              fill = "yellow", alpha = 0.5) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = "IR Values", x = "Months", y = "IR") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title.x = element_text(size = 10, color = "black", face = "bold"),
          axis.title.y = element_text(size = 10, color = "black", face = "bold"),
          title = element_text(size = 12, color = "yellow4", face = "bold")) , 
  # CFR    
  covid19 %>% 
    group_by(date) %>%
    summarise(CFR = (sum(D.cum) / sum(I.cum))*100) %>%
    ggplot() +
    geom_area(aes(x = date, y = CFR), size = 1, colour = "red3", 
              fill = "red", alpha = 0.5) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(title = "CFR Values", x = "Months", y = "CFR") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title.x = element_text(size = 10, color = "black", face = "bold"),
          axis.title.y = element_text(size = 10, color = "black", face = "bold"),
          title = element_text(size = 12, color = "red4", face = "bold")) )


### The pandemic by continent

# daily Infected
covid19 %>%
  group_by(continent.name, date) %>%
  summarise(I = sum(I)) %>%
  ggplot(aes(x = date, y = I)) +
  geom_col(col = "yellow3", fill = "yellow", size = 0.5, alpha = 0.4) +
  facet_wrap(~ continent.name, ncol = 1, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        strip.text.x = element_text(size = 10, color="black", face ="bold"),
        title = element_text(size = 12, color = "yellow4", face = "bold")) +
  labs(title = "daily Infected Values", x = "Months", y = "Infected")

# Acumulated Infected
covid19 %>%
  group_by(continent.name, date) %>%
  summarise(I.cum = sum(I.cum)) %>%
  ggplot() +
  geom_col(aes(x = date, y = I.cum),
           col = "yellow3", fill = "yellow", size = 0.5, alpha = 0.4) +
  facet_wrap(~ continent.name, ncol = 1, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        strip.text.x = element_text(size = 10, color="black", face ="bold"),
        title = element_text(size = 12, color = "yellow4", face = "bold")) +
  labs(title = "Acumulated Infected Values", x = "Months", y = "Infected")


# daily Deaths
covid19 %>%
  group_by(continent.name, date) %>%
  summarise(D.dairy = sum(D)) %>%
  ggplot() +
  geom_col(aes(x = date, y = D.dairy),
           col = "red3", fill = "red", size = 0.5, alpha = 0.4) +
  facet_wrap(~ continent.name, ncol = 1, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        strip.text.x = element_text(size = 10, color="black", face ="bold"),
        title = element_text(size = 12, color = "red4", face = "bold")) +
  labs(title = "daily Deaths Values", x = "Months", y = "Deaths")

# Acumulated Deaths
covid19 %>%
  group_by(continent.name, date) %>%
  summarise(D.cum = sum(D.cum)) %>%
  ggplot() +
  geom_col(aes(x = date, y = D.cum),
           col = "red3", fill = "red", size = 0.5, alpha = 0.4) +
  facet_wrap(~ continent.name, ncol = 1, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        strip.text.x = element_text(size = 10, color="black", face ="bold"),
        title = element_text(size = 12, color = "red4", face = "bold")) +
  labs(title = "Acumulated Deaths Values", x = "Months", y = "Deaths")


### The pandemic by Regions

# Daily Infected
covid19 %>%
  group_by(region.name, date) %>%
  summarise(I = sum(I)) %>%
  ggplot(aes(x = date, y = I)) +
  geom_col(col = "yellow3", fill = "yellow", size = 0.5, alpha = 0.4) +
  facet_wrap(~ region.name, ncol = 4, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"),
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        strip.text.x = element_text(size = 8, color="black", face ="bold"),
        title = element_text(size = 12, color = "yellow4", face = "bold")) +
  labs(title = "Daily Infected Values", x = "Months", y = "Infected")

# Acumulated Infected
covid19 %>%
  group_by(region.name, date) %>%
  summarise(I.cum = sum(I.cum)) %>%
  ggplot() +
  geom_col(aes(x = date, y = I.cum),
           col = "yellow3", fill = "yellow", size = 0.5, alpha = 0.4) +
  facet_wrap(~ region.name, ncol = 4, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"),
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        strip.text.x = element_text(size = 8, color="black", face ="bold"),
        title = element_text(size = 12, color = "yellow4", face = "bold")) +
  labs(title = "Acumulated Infected Values", x = "Months", y = "Infected")


# Daily Deaths
covid19 %>%
  group_by(region.name, date) %>%
  summarise(D.dairy = sum(D)) %>%
  ggplot() +
  geom_col(aes(x = date, y = D.dairy),
           col = "red3", fill = "red", size = 0.5, alpha = 0.4) +
  facet_wrap(~ region.name, ncol = 4, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"),
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        strip.text.x = element_text(size = 8, color="black", face ="bold"),
        title = element_text(size = 12, color = "red4", face = "bold")) +
  labs(title = "Daily Deaths Values", x = "Months", y = "Deaths")

# Acumulated Deaths
covid19 %>%
  group_by(region.name, date) %>%
  summarise(D.cum = sum(D.cum)) %>%
  ggplot() +
  geom_col(aes(x = date, y = D.cum),
           col = "red3", fill = "red", size = 0.5, alpha = 0.4) +
  facet_wrap(~ region.name, ncol = 4, scale = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black"),
        axis.title.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title.y = element_text(size = 8, color = "black", face = "bold"),
        strip.text.x = element_text(size = 8, color="black", face ="bold"),
        title = element_text(size = 12, color = "red4", face = "bold")) +
  labs(title = "Acumulated Deaths Values", x = "Months", y = "Deaths")


### Latitudes

grid.arrange(nrow = 1, 
             map.world %>%
               left_join(Isocode) %>%
               mutate(latitude = ifelse(is.na(latitude), lat, latitude)) %>%
               ggplot(aes(long, lat, group = group,
                          fill = ifelse(latitude > 0, "North", "Sourth"))) + 
               geom_polygon(color = "steelblue2",  size = 0.1) +
               scale_fill_manual(values = c("blue2","tan2")) +
               scale_y_continuous(breaks = c(-50, -23.27, 0, 23.27, 50)) +
               theme(axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 8, color = "red", face ="bold"),
                     legend.text = element_text(size = 8, color = "black", face ="bold"),
                     legend.title = element_blank(),
                     legend.position = "bottom",
                     panel.grid = element_blank(),
                     panel.grid.major.y = element_line(colour = "red"),
                     panel.background = element_rect(colour= "black", 
                                                     fill = "lightblue", 
                                                     size = 0.5)) +
               ggtitle("Hemispheres") + labs(fill = "Hemispheres") ,
             map.world %>%
               left_join(Isocode) %>%
               mutate(latitude = ifelse(is.na(latitude), lat, latitude)) %>%
               ggplot(aes(long, lat, group = group,
                          fill = ifelse(!is.na(latitude) & latitude < 23.27 & latitude > -23.27, 
                                        "Tropical", "No Tropical"))) + 
               geom_polygon(color = "steelblue2",  size = 0.1) +
               scale_fill_manual(values = c("lightgray","gold")) +
               scale_y_continuous(breaks = c(-50, -23.27, 0, 23.27, 50)) +
               theme(axis.title = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 8, color = "red", face ="bold"),
                     legend.text = element_text(size = 8, color = "black", face ="bold"),
                     legend.title = element_blank(),
                     legend.position = "bottom",
                     panel.grid = element_blank(),
                     panel.grid.major.y = element_line(colour = "red"),
                     panel.background = element_rect(colour= "black", 
                                                     fill = "lightblue", 
                                                     size = 0.5)) +
               ggtitle("Intertropical Zone") + labs(fill = "Zones") )


#### Hemispheres
# North: latitude > 0, Sourth: latitude < 0

grid.arrange(ncol = 3,
             # Popultation by Hemisphere
             filter(covid19, date == last(date)) %>%
               mutate(hemisphere = ifelse(latitude > 0, "North", "Sourth")) %>%
               group_by(hemisphere) %>%
               summarise(N = sum(N)) %>%
               ggplot(aes(x = hemisphere, y = N, fill = hemisphere)) +
               geom_col() + 
               scale_y_continuous(labels = label_number_si()) +
               scale_fill_manual(values = c("blue2","tan2")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 8, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.text = element_text(size = 6, color = "black"),
                     legend.title = element_blank(),
                     legend.position = "left",
                     title = element_text(size = 9, color = "blue4", face = "bold")) +
               labs(title = "Population", x = element_blank(), y = "Population"),
             # Infected by Hemisphere
             covid19 %>%
               mutate(hemisphere = ifelse(latitude > 0, "North", "Sourth")) %>%
               group_by(hemisphere, date) %>%
               summarise(I.cum = sum(I.cum)) %>%
               ggplot(aes(x = date, y = I.cum, color = hemisphere, fill = hemisphere)) +
               geom_line(size = 1) +
               geom_area(position = "dodge") +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("blue2","tan2")) +
               scale_fill_manual(values = c("blue2","tan2")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "none",
                     title = element_text(size = 9, color = "yellow4", face = "bold")) +
               labs(title = "Infected", x = element_blank(), y = "Infected") ,
             # Deaths by Hemisphere 
             covid19 %>%
               mutate(hemisphere = ifelse(latitude > 0, "North", "Sourth")) %>%
               group_by(hemisphere, date) %>%
               summarise(D.cum = sum(D.cum)) %>%
               ggplot(aes(x = date, y = D.cum, color = hemisphere, fill = hemisphere)) +
               geom_line(size = 1) +
               geom_area(position = "dodge") +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("blue2","tan2")) +
               scale_fill_manual(values = c("blue2","tan2")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "none",
                     title = element_text(size = 9, color = "red4", face = "bold")) +
               labs(title = "Deaths", x = element_blank(), y = "Deaths") )

# IR and CFR by Hemisphere
grid.arrange(ncol = 2,
             # Infected rate by Hemisphere
             covid19 %>%
               mutate(Hemisphere = ifelse(latitude > 0, "North", "Sourth")) %>%
               group_by(Hemisphere, date) %>%
               summarise(IR = (sum(I.cum)/sum(N))*10^5) %>%
               ggplot(aes(x = date, y = IR, color = Hemisphere, fill = Hemisphere)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = comma) +
               scale_color_manual(values = c("blue2","tan2")) +
               scale_fill_manual(values = c("blue2","tan2")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Infected Rate", x = element_blank(), y = "Infected") ,
             # Infected rate by Hemisphere
             covid19 %>%
               mutate(Hemisphere = ifelse(latitude > 0, "North", "Sourth")) %>%
               group_by(Hemisphere, date) %>%
               summarise(IR = mean(IR)) %>%
               ggplot(aes(x = date, y = IR, color = Hemisphere, fill = Hemisphere)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("blue2","tan2")) +
               scale_fill_manual(values = c("blue2","tan2")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Infected Rate: mean(IR)", x = element_blank(), y = "Infected"),
             # Deaths rate by Hemisphere
             covid19 %>%
               mutate(Hemisphere = ifelse(latitude > 0, "North", "Sourth")) %>%
               group_by(Hemisphere, date) %>%
               summarise(CFR = (sum(D.cum)/sum(I.cum))*100) %>%
               ggplot(aes(x = date, y = CFR, color = Hemisphere, fill = Hemisphere)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("blue2","tan2")) +
               scale_fill_manual(values = c("blue2","tan2")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Deaths Rate", x = element_blank(), y = "Infected"),
             # Deaths rate by Hemisphere
             covid19 %>%
               mutate(Hemisphere = ifelse(latitude > 0, "North", "Sourth")) %>%
               group_by(Hemisphere, date) %>%
               summarise(CFR = mean(CFR)) %>%
               ggplot(aes(x = date, y = CFR, color = Hemisphere, fill = Hemisphere)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("blue2","tan2")) +
               scale_fill_manual(values = c("blue2","tan2")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Deaths Rate: mean(CFR)", x = element_blank(), y = "Infected"))

# Influence of Brazil and Indonesia 
filter(covid19, date == last(date) & latitude < 0) %>%
  mutate(countries = ifelse(N > 10^8, "Brazil and Indonesia", "Others")) %>%
  group_by(countries) %>% 
  summarise(quantity = n(), Infected = sum(I.cum), Population = sum(N), 
            IR.mean = mean(IR), IR = sum(I.cum)/sum(N)*10^5)


#### Intertropical Zone
# Intertropical Zone: latitude < 23.27 and > -23.27

grid.arrange(ncol = 3,
             # Popultation by zone
             filter(covid19, date == last(date)) %>%
               mutate(zone = ifelse(latitude < 23.27 & latitude > -23.27, 
                                    "Tropical", "No Tropical")) %>%
               group_by(zone) %>%
               summarise(N = sum(N)) %>%
               ggplot(aes(x = zone, y = N, fill = zone)) +
               geom_col() + 
               scale_y_continuous(labels = label_number_si()) +
               scale_fill_manual(values = c("lightgrey","gold")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.text = element_text(size = 6, color = "black"),
                     legend.title = element_blank(),
                     legend.position = "left",
                     title = element_text(size = 9, color = "blue4", face = "bold")) +
               labs(title = "Population", x = element_blank(), y = "Population"),
             # Infected by zone
             covid19 %>%
               mutate(zone = ifelse(latitude < 23.27 & latitude > -23.27, 
                                    "Tropical", "No Tropical")) %>%
               group_by(zone, date) %>%
               summarise(I.cum = sum(I.cum)) %>%
               ggplot(aes(x = date, y = I.cum, color = zone, fill = zone)) +
               geom_line(size = 1) +
               geom_area(position = "dodge") +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("lightgrey","gold")) +
               scale_fill_manual(values = c("lightgrey","gold")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "none",
                     title = element_text(size = 9, color = "yellow4", face = "bold")) +
               labs(title = "Infected", x = element_blank(), y = "Infected"),
             # Deaths by zone 
             covid19 %>%
               mutate(zone = ifelse(latitude < 23.27 & latitude > -23.27, 
                                    "Tropical", "No Tropical")) %>%
               group_by(zone, date) %>%
               summarise(D.cum = sum(D.cum)) %>%
               ggplot(aes(x = date, y = D.cum, color = zone, fill = zone)) +
               geom_line(size = 1) +
               geom_area(position = "dodge") +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("lightgrey","gold")) +
               scale_fill_manual(values = c("lightgrey","gold")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "none",
                     title = element_text(size = 9, color = "red4", face = "bold")) +
               labs(title = "Deaths", x = element_blank(), y = "Deaths") )

grid.arrange(ncol = 2,
             # Infected rate by zone
             covid19 %>%
               mutate(Zone = ifelse(latitude < 23.27 & latitude > -23.27, 
                                    "Tropical", "No Tropical")) %>%
               group_by(Zone, date) %>%
               summarise(IR = (sum(I.cum)/sum(N))*10^5) %>%
               ggplot(aes(x = date, y = IR, color = Zone, fill = Zone)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("lightgrey","gold")) +
               scale_fill_manual(values = c("lightgrey","gold")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Infected Rate", x = element_blank(), y = "Infected") ,
             # Infected rate by zone
             covid19 %>%
               mutate(Zone = ifelse(latitude < 23.27 & latitude > -23.27, 
                                    "Tropical", "No Tropical")) %>%
               group_by(Zone, date) %>%
               summarise(IR = mean(IR)) %>%
               ggplot(aes(x = date, y = IR, color = Zone, fill = Zone)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("lightgrey","gold")) +
               scale_fill_manual(values = c("lightgrey","gold")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Infected Rate: mean(IR)", x = element_blank(), y = "Infected"),
             # Deaths rate by zone
             covid19 %>%
               mutate(Zone = ifelse(latitude < 23.27 & latitude > -23.27, 
                                    "Tropical", "No Tropical")) %>%
               group_by(Zone, date) %>%
               summarise(CFR = (sum(D.cum)/sum(I.cum))*100) %>%
               ggplot(aes(x = date, y = CFR, color = Zone, fill = Zone)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("lightgrey","gold")) +
               scale_fill_manual(values = c("lightgrey","gold")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Deaths Rate", x = element_blank(), y = "Infected"),
             # Deaths rate by zone
             covid19 %>%
               mutate(Zone = ifelse(latitude < 23.27 & latitude > -23.27, 
                                    "Tropical", "No Tropical")) %>%
               group_by(Zone, date) %>%
               summarise(CFR = mean(CFR)) %>%
               ggplot(aes(x = date, y = CFR, color = Zone, fill = Zone)) +
               geom_line(size = 1) +
               geom_area(position = "dodge", alpha = 0.5) +
               scale_x_date(date_breaks = "1 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si()) +
               scale_color_manual(values = c("lightgrey","gold")) +
               scale_fill_manual(values = c("lightgrey","gold")) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_blank(),
                     legend.position = "bottom",
                     title = element_text(size = 9, face = "bold")) +
               labs(title = "Deaths Rate: mean(CFR)", x = element_blank(), y = "Infected"))


#### Seasons

#               North               South
# Spring:   21/03 - 20/06       21/09 - 20/19
# Summer:   21/06 - 20/09       21/12 - 20/03
# Fall:     21/09 - 20/12       21/03 - 20/06
# Winter:   21/12 - 20/03       21/06 - 20/09

covid19 <- covid19 %>%
  mutate(md = format(date, "%m-%d"),
         season = ifelse(latitude > 0, 
                         case_when(between(md, "01-01", "03-20") ~ "winter",
                                   between(md, "03-21", "06-20") ~ "spring",
                                   between(md, "06-21", "09-20") ~ "summer",
                                   between(md, "09-21", "12-20") ~ "fall",
                                   between(md, "12-21", "12-31") ~ "winter"),
                         case_when(between(md, "01-01", "03-20") ~ "summer",
                                   between(md, "03-21", "06-20") ~ "fall",
                                   between(md, "06-21", "09-20") ~ "winter",
                                   between(md, "09-21", "12-20") ~ "spring",
                                   between(md, "12-21", "12-31") ~ "winter"))) %>%
  select(-md)

# Infected by season
covid19 %>%
  group_by(season, continent.name) %>%
  summarise(I.cum = sum(I)) %>%
  ggplot(aes(x = season, y = I.cum, 
             fill = continent.name)) +
  geom_col() +
  scale_y_continuous(labels = label_number_si()) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "Infected Summary by season", 
       x = "Seasons", y = "Infected", fill = "")

# Deaths by season
covid19 %>%
  group_by(season, continent.name) %>%
  summarise(D.cum = sum(D)) %>%
  ggplot(aes(x = season, y = D.cum, 
             fill = continent.name)) +
  geom_col() +
  scale_y_continuous(labels = label_number_si()) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "Deaths Summary by season", 
       x = "Seasons", y = "Deaths", fill = "")


N.total <- sum(filter(covid19, date == last(date))$N)

covid19 %>%
  group_by(season, continent.name) %>%
  summarise(I.cum = sum(I),
            D.cum = sum(D),
            IR = (I.cum / N.total) * 10^5,
            CFR = (D.cum / I.cum) * 100) %>%
  select(-I.cum, -D.cum) %>%
  gather("Type", "Value", -season, -continent.name) %>%
  ggplot(aes(x = season, y = Value, fill = continent.name)) +
  geom_col() +
  facet_wrap(. ~ Type, ncol = 1, scales = "free_y", 
             labeller = as_labeller(c(
               'I.cum' = "Infected Acumulated",
               'D.cum' = "Deaths Acumulated",
               'IR' = "Infected Rate",
               'CFR' = "Fatality Rate"))) +
  scale_x_reordered() +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, color = "black", face = "bold"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 8, color = "black", face = "bold"),
        legend.text = element_text(size = 8, color="black"),
        legend.title = element_text(size = 10, color="black", face = "bold"),
        strip.text.x = element_text(size = 10, color="black", face = "bold")) +
  labs(title = "Indicators by Season", 
       x = "", y = "Indicators Value", fill = "Continents")


#### Test Positive Rate

# Comparison of daily cases and proportion of positive tests in some countries.
covid19.example <- filter(covid19,
                          country.alphacode %in% c("CAN","CHL","KOR","QAT","ISR","ITA")) %>%
  select(country.name, date, I, TPR)

coef <- max(covid19.example$I, na.rm = T) / max(covid19.example$TPR, na.rm = T)

covid19.example %>%
  mutate(TPR = TPR * coef) %>%
  gather("Type", "Value",-country.name, -date) %>%
  ggplot(aes(x = date, y = Value, color = Type)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = label_number_si(), 
                     sec.axis = sec_axis(~ . / coef, 
                                         name = "Test Positive Rate (TPR)",
                                         labels = label_number_si())) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = c("yellow3","blue3"),
                     labels = c("daily Infected","Positive Test Rate")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y.right = element_text(size = 8, color = "black", angle = 90),
        legend.position = "bottom",
        title = element_text(size = 12, color = "black", face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white", 
                                        linetype = "blank")) + 
  labs(title = "Daily infected and TPR", x = "Months", y = "Daily Infected", color = "") +
  facet_wrap(. ~ country.name, ncol = 2, scale = "free_y")

# Comparison of daily cases and proportion of positive tests for each continent.
covid19.example <- group_by(covid19, continent.name, date) %>%
  summarise(I = mean(I, na.rm = T), 
            TPR = mean(TPR, na.rm = T))

coef <- max(covid19.example$I, na.rm = T) / max(covid19.example$TPR, na.rm = T)

covid19.example %>%
  mutate(TPR = TPR * coef) %>%
  gather("Type", "Value",-continent.name, -date) %>%
  ggplot(aes(x = date, y = Value, color = Type)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = label_number_si(), 
                     sec.axis = sec_axis(~ . / coef, 
                                         name = "Test Positive Rate (TPR)",
                                         labels = label_number_si())) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = c("yellow3","blue3"),
                     labels = c("daily Infected","Positive Test Rate")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y.right = element_text(size = 8, color = "black", angle = 90),
        legend.position = "bottom",
        title = element_text(size = 12, color = "black", face = "bold"),
        strip.background = element_rect(colour = "black", fill = "white", 
                                        linetype = "blank")) + 
  labs(title = "Daily infected and TPR", x = "Months", y = "Daily Infected", color = "") +
  facet_wrap(. ~ continent.name, ncol = 2, scale = "free_y")


## Insights Gained
## ----------------

### CFR outline

CFR.mean = mean(filter(covid19,date == last(date))$CFR)
CFR.mean

# Global progress of CFR from the pandemic.
covid19 %>% 
  group_by(date) %>%
  summarise(CFR = (sum(D.cum) / sum(I.cum))*100) %>%
  ggplot() +
  geom_area(aes(x = date, y = CFR), size = 1, colour = "red3", 
            fill = "red", alpha = 0.5) +
  scale_y_continuous(labels = label_number_si()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "CFR Values", x = "Months", y = "CFR") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 12, color = "red4", face = "bold"))

# Countries with CFRs above average
filter(covid19, date == last(date) &
         country.alphacode %in% c("SDN","EGY","TCD","MEX","ECU","BOL",
                                  "YEM","SYR","CHN","BGR","BIH","ITA")) %>% 
  ggplot() +
  geom_col(aes(x = CFR, y = country.name), size = 1, colour = "red3", 
           fill = "red", alpha = 0.2) +
  geom_vline(xintercept = CFR.mean, size = 1.5, color = "red4") +
  scale_x_continuous(labels = label_number_si(),n.breaks = 20)  +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 12, color = "red4", face = "bold")) +
  labs(title = "Countries with CFRs above average", x = "Countries", y = "CFR")


# Relationship of the CFR with respect to the average of each Country
filter(covid19,date == last(date)) %>% 
  ggplot(aes(x = CFR, y = reorder(country.alphacode, CFR), 
             fill = CFR < CFR.mean)) +
  geom_col() +
  geom_vline(xintercept = CFR.mean)  +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black", hjust = 1),
        axis.text.y = element_text(size = 6, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        legend.position = "none",
        title = element_text(size = 12, color = "yellow3", face = "bold")) +
  labs(title = "CFR Values", x = "CFR", y = "Countries") +
  facet_wrap(~ continent.name, nrow = 1, scale = "free_y")

rm(CFR.mean)


### Calculation of the effective Reporductive Number (Re)

date_i <- last(covid19$date)

covid19.example <-  
  filter(covid19, between(date, date_i-7, date_i)) %>%
  group_by(date) %>%
  summarise(I = sum(I.cum)) %>%
  mutate(dates = date, I = ifelse(I < 0, 0, I)) 

Re <- estimate_R(incid = covid19.example, method = "parametric_si",
                 config = make_config(list(mean_si = 1.96, std_si = 0.51)))

Re$R$`Mean(R)`

rm(covid19.example, date_i, Re)

# Calculate Re for one country and one date
covid19.calculate.Re <- function(data, c.alphacode, date_i) {
  dat <- filter(data, country.alphacode == c.alphacode & 
                  between(date, date_i - 7, date_i)) %>%
    select(date, I.cum) %>%
    mutate(dates = date, I = ifelse(I.cum < 0, 0, I.cum))
  
  options(warn = -1)
  Re <- suppressMessages(estimate_R(incid = dat, method = "parametric_si",
                                    config = make_config(list(mean_si = 1.96, std_si = 0.51))))
  options(warn = 0)
  
  Re$R$`Mean(R)`
} 

date_i <- as.Date("2020-12-18")
c.alphacode <- "USA"
Re <- covid19.calculate.Re(covid19, c.alphacode, date_i)

covid19.Re <- data.frame(country.alphacode = c.alphacode,
                         date = as.Date(date_i), Re = Re)
covid19.Re

rm(Re, date_i, c.alphacode, covid19.Re)

# Calculate Re for All countries and one date
covid19.calculate.Re.country <- function(data, d) {
  Re <- sapply(data$country.alphacode[which(data$date == d)],
               function(i) covid19.calculate.Re(data, i, d) )
  
  Re <- as.data.frame(Re)
  df <- data.frame(country.alphacode = rownames(Re),
                   date = d, Re = Re)
  rownames(df) <- NULL
  df
}

covid19.Re <- covid19.calculate.Re.country(covid19, as.Date("2020-12-18"))
tail(covid19.Re, 20)

# Calculate Re for all countries and all dates
covid19.calculate.Re.all <- function(data) {
  data.Re <- data.frame(country.alphacode = NULL, date = NULL, Re = NULL)
  dates <- seq(first(data$date)+7, last(data$date), 1)
  nds <- length(dates)
  message("Dates range: [", first(dates), " - ", last(dates), "] ", nds, " days")
  for (d in dates) {
    date_i <- as.Date(d, origin = "1970-01-01")
    ds <- date_i - first(dates) + 1
    message('\r', date_i, " - ", ds, "/", nds, " (",
            round(ds*100/nds, 1), "%)    ", appendLF = FALSE)
    
    Re <- covid19.calculate.Re.country(data, date_i)
    data.Re <- rbind(data.Re, Re)
  }
  data.Re
}

covid19.Re <- covid19.calculate.Re.all(covid19)
tail(covid19.Re, 20)


### Add Re to Covid19

covid19 <- covid19 %>% left_join(covid19.Re)

# Plot Re
covid19 %>%
  group_by(date) %>%
  summarise(Re = mean(Re, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = Re)) +
  geom_line(color = "yellow3", size = 1) +
  geom_area(fill = "yellow3", alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 12, color = "yellow4", face = "bold")) +
  labs(title = "Average Re Progression", x = "Months", y = "Average Re")

covid19.example <- group_by(covid19, date) %>%
  summarise(I.cum = sum(I.cum)) %>%
  mutate(country.alphacode = "GLB", dates = date, I.cum = ifelse(I.cum < 0, 0, I.cum))

covid19.Re <- covid19.calculate.Re.all(covid19.example)
mean(covid19.Re$Re)

# Plot Global Re
covid19.Re %>%
  ggplot(aes(x = date, y = Re)) +
  geom_line(color = "yellow3", size = 1) +
  geom_area(fill = "yellow3", alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 12, color = "yellow4", face = "bold")) +
  labs(title = "Global Re Progression", x = "Months", y = "Global Re")

rm(covid19.Re)


### Correlations

covid19 <- mutate(covid19, 
                  season.num = case_when(season == "winter" ~ 1,
                                         season == "spring" ~ 2,
                                         season == "summer" ~ 3,
                                         season == "fall" ~ 4))

correlations <- round(cor(select(na.omit(covid19), 
                                 D.cum, I.cum, D, I, measure.level, IR, CFR, 
                                 Re, TPR, latitude, season.num)),2)
correlations

corrplot.mixed(correlations, 
               upper = "color", 
               order = "FPC", 
               lower.col = "black", 
               number.cex = 0.7, 
               tl.cex = 0.5)

sort(correlations[1, -1], decreasing = T)


## Modeling Approaches 
## --------------------

### Smoothing

#### Negative Infected Values

filter(covid19, I < 0) %>% 
  arrange(I) %>% 
  select(country.name, date, I) %>% 
  head(20)

filter(covid19, country.alphacode %in% c("ESP","ECU","LUX","FRA")) %>%
  ggplot(aes(x = date, y = I, color = ifelse(I < 0, 0, 1))) +
  geom_point(size = 1) +
  geom_line(size = 0.5, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = label_number_si(), n.breaks = 10) +
  scale_color_gradient(low = "black", high = "yellow2") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"),
        legend.position = "none") +
  labs(title = "Negatives Infected", color = "", x = "Date", y = "Infected") +
  facet_wrap(. ~ country.name, scale = "free_y")


#### Negative Deaths Values

filter(covid19, D < 0) %>% 
  arrange(D) %>% 
  select(country.name, date, D) %>% 
  head(20)

filter(covid19, country.alphacode %in% c("ESP","KGZ","FRA","BEL")) %>%
  ggplot(aes(x = date, y = D, color = ifelse(D < 0, 0, 1))) +
  geom_point(size = 1) +
  geom_line(size = 0.5, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = label_number_si(), n.breaks = 10) +
  scale_color_gradient(low = "black", high = "red2") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"),
        legend.position = "none") +
  labs(title = "Negatives Deaths", color = "", x = "Date", y = "Deaths") +
  facet_wrap(. ~ country.name, scale = "free_y")

covid19 %>%
  group_by(date) %>%
  summarise(D = sum(D)) %>%
  mutate(D.mm = rollmean(D, 7, fill = list(0, 0, 0)))


#### Smoothing daily Deaths Group by date (rollmean)

covid19 %>%
  group_by(date) %>%
  summarise(D = sum(D)) %>%
  mutate(D.mm = rollmean(D, 7, fill = list(0, 0, 0), align = "right")) %>%
  gather("Type", "Value", -date) %>%
  ggplot(aes(x = date, y = Value, color = Type)) +
  geom_line(size = 1, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(n.breaks = 10) +
  scale_color_manual(values = c("red","black"),
                     labels = c("Daily Deaths","Moving Average Daily Deaths")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.position = "bottom") +
  labs(title = "Smoothing Daily Deaths Group by date (rollmean)", color = "",
       x = "date", y = "D")


#### Smoothing daily Deaths Group by date (geom_smooth)

covid19 %>%
  group_by(date) %>%
  summarise(D = sum(D)) %>%
  ggplot(aes(x = date, y = D)) +
  geom_line(size = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.2, 
              color = "blue", alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "Smoothing Daily Deaths Group by date (geom_smooth)",
       x = "date", y = "D")


#### Smoothing by Week

covid19 %>%
  mutate(W = (year(date)-year(first(date)))*52 + week(date)) %>%
  select(country.name, date, W)

# Smoothing daily Deaths Group by week (rollmean)
covid19 %>%
  mutate(W = (year(date)-year(first(date)))*52 + week(date)) %>%
  group_by(W) %>%
  summarise(D = sum(D)) %>%
  mutate(D.mm = rollmean(D, 7, fill = list(0, 0, 0), align = "right")) %>%
  gather("Type", "Value", -W) %>%
  ggplot(aes(x = W, y = Value, color = Type)) +
  geom_line(size = 1, alpha = 0.5) +
  scale_y_continuous(n.breaks = 10) +
  scale_color_manual(values = c("red","black"),
                     labels = c("Deaths Weekly","Moving Average Deaths")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.position = "bottom") +
  labs(title = "Smoothing Weekly Deaths Group by week (rollmean)", color = "",
       x = "Weeks", y = "D")

# Smoothing daily Deaths Group by week (geom_smooth)
covid19 %>%
  mutate(W = (year(date)-year(first(date)))*52 + week(date)) %>%
  group_by(W) %>%
  summarise(D = sum(D)) %>%
  ggplot(aes(x = W, y = D)) +
  geom_line(size = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = 'loess', formula = 'y ~ x', span = 0.2, 
              color = "blue", alpha = 0.5) +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "Smoothing Daily Deaths Group by week (geom_smooth)",
       x = "Week", y = "D")


### Smoothing covid19 data: covid19.s

covid19.s <- covid19 %>%
  mutate(Y = year(date), W = (year(date)-year(first(date)))*52 + week(date)) %>%
  group_by(country.alphacode, W) %>%
  summarise(I = sum(I), D = sum(D), R = sum(R), N = max(N), 
            TPR = mean(TPR, na.rm = TRUE), 
            measure.level = max(measure.level),
            season = last(season),
            Re = mean(Re, na.rm = TRUE)) %>%
  group_by(country.alphacode) %>%
  mutate(I.cum = cumsum(I), D.cum = cumsum(D), R.cum = cumsum(R),
         S = N - I.cum - R.cum,
         IR = (I.cum / N) * 10^5,
         CFR = ifelse(I.cum == 0, 0, D.cum / I.cum)*100, 
         M = (D.cum / N)*10^5,
         season.num = case_when(season == "winter" ~ 1,
                                season == "spring" ~ 2,
                                season == "summer" ~ 3,
                                season == "fall" ~ 4)) %>%
  left_join(distinct(covid19, country.name, country.isocode, country.alphacode, 
                     latitude, longitude, continent.name, region.name)) %>%
  ungroup()

# Smoothing Infected comparisson
grid.arrange(
  filter(covid19, country.alphacode %in% c("ESP","ECU","LUX","FRA")) %>%
    ggplot(aes(x = date, y = I, color = ifelse(I < 0, 0, 1))) +
    geom_point(size = 1) +
    geom_line(size = 0.5, alpha = 0.5) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    scale_color_gradient(low = "black", high = "yellow2") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"),
          legend.position = "none") +
    labs(title = "Infected by date", color = "", x = "Date", y = "Infected") +
    facet_wrap(. ~ country.name, ncol = 4, scale = "free_y")
  ,
  filter(covid19.s, country.alphacode %in% c("ESP","ECU","LUX","FRA")) %>%
    ggplot(aes(x = W, y = I, color = ifelse(I < 0, 0, 1))) +
    geom_point(size = 1) +
    geom_line(size = 0.5, alpha = 0.5) +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    scale_color_gradient(low = "black", high = "yellow2") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"),
          legend.position = "none") +
    labs(title = "Infected by Weeks", color = "", x = "Week", y = "Infected") +
    facet_wrap(. ~ country.name, ncol = 4, scale = "free_y")
)


### Smoothing covid19 data using Media Movil: covid19.s.mm

covid19.s.mm <- covid19.s %>%
  group_by(country.alphacode) %>%
  mutate(I = rollmean(I, 3, fill = list(0, 0, 0), align = "right"), 
         D = rollmean(D, 3, fill = list(0, 0, 0), align = "right"), 
         R = rollmean(R, 3, fill = list(0, 0, 0), align = "right"),
         TPR = rollmean(TPR, 3, fill = list(0, 0, 0), na.rm = TRUE, align = "right"),
         Re = rollmean(Re, 3, fill = list(0, 0, 0), align = "right"), 
         I.cum = cumsum(I), 
         D.cum = cumsum(D), 
         R.cum = cumsum(R),
         S = N - I.cum - R.cum,
         IR = (I.cum / N) * 10^5,
         CFR = ifelse(I.cum == 0, 0, D.cum / I.cum)*100, 
         M = (D.cum / N)*10^5) %>%
  ungroup()

# Smoothing Infected comparisson
grid.arrange(ncol = 1,
             filter(covid19, country.alphacode %in% c("ESP","ECU","LUX","FRA")) %>%
               ggplot(aes(x = date, y = I, color = ifelse(I < 0, 0, 1))) +
               geom_point(size = 1) +
               geom_line(size = 0.5, alpha = 0.5) +
               scale_x_date(date_breaks = "3 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
               scale_color_gradient(low = "black", high = "yellow2") +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 8, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title = element_text(size = 10, color = "black", face = "bold"),
                     legend.position = "none") +
               labs(title = "Infected by date", color = "", x = "Date", y = "Infected") +
               facet_wrap(. ~ country.name, ncol = 4, scale = "free_y")
             ,
             filter(covid19.s, country.alphacode %in% c("ESP","ECU","LUX","FRA")) %>%
               ggplot(aes(x = W, y = I, color = ifelse(I < 0, 0, 1))) +
               geom_point(size = 1) +
               geom_line(size = 0.5, alpha = 0.5) +
               scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
               scale_color_gradient(low = "black", high = "yellow2") +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 8, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title = element_text(size = 10, color = "black", face = "bold"),
                     legend.position = "none") +
               labs(title = "Infected by Weeks", color = "", x = "Week", y = "Infected") +
               facet_wrap(. ~ country.name, ncol = 4, scale = "free_y")
             ,
             filter(covid19.s.mm, country.alphacode %in% c("ESP","ECU","LUX","FRA")) %>%
               group_by(country.alphacode) %>%
               mutate(I = rollmean(I, 7, fill = list(NA, NULL, NA))) %>%
               ungroup() %>%
               ggplot(aes(x = W, y = I)) +
               geom_point(color = "yellow2", size = 1) +
               geom_line(color = "yellow2", size = 0.5, alpha = 0.5) +
               scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 8, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title = element_text(size = 10, color = "black", face = "bold"),
                     legend.position = "none") +
               labs(title = "Media Movil Infected by Weeks", color = "", x = "Week", y = "Infected") +
               facet_wrap(. ~ country.name, ncol = 4, scale = "free_y")
)

# Smoothing Indicators comparisson
grid.arrange(nrow = 1,
             filter(covid19, country.alphacode == "ESP") %>%
               select(date, I, D, IR, CFR, TPR) %>%
               gather("Types", "Values", -date) %>%
               ggplot(aes(x = date, y = Values, color = Types)) +
               geom_line(size = 1) +
               scale_x_date(date_breaks = "3 month", date_labels = "%b") +
               scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
               scale_color_manual(values = c("red2","red2","yellow2","yellow2","green2")) +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 8, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title = element_text(size = 10, color = "black", face = "bold"),
                     strip.text = element_blank(),
                     legend.position = "none") +
               labs(title = "covid19", x = "Date", y = "") +
               facet_wrap(. ~ Types, ncol = 1, scale = "free_y")
             ,
             filter(covid19.s, country.alphacode == "ESP") %>%
               select(W, I, D, IR, CFR, TPR) %>%
               gather("Types", "Values", -W) %>%
               ggplot(aes(x = W, y = Values, color = Types)) +
               geom_line(size = 1) +
               scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
               scale_color_manual(values = c("red2","red2","yellow2","yellow2","green2")) +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 8, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title = element_text(size = 10, color = "black", face = "bold"), 
                     strip.text = element_blank(),
                     legend.position = "none") +
               labs(title = "covid19.s", x = "Weeks", y = "") +
               facet_wrap(. ~ Types, ncol = 1, scale = "free_y")
             ,
             filter(covid19.s.mm, country.alphacode == "ESP") %>%
               select(W, I, D, IR, CFR, TPR) %>%
               gather("Types", "Values", -W) %>%
               ggplot(aes(x = W, y = Values, color = Types)) +
               geom_line(size = 1) +
               scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
               scale_color_manual(values = c("red2","red2","yellow2","yellow2","green2")) +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 8, color = "black"),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title = element_text(size = 10, color = "black", face = "bold"), 
                     strip.text = element_text(size = 10, color = "black", face = "bold"),
                     legend.position = "none") +
               labs(title = "covid19.s.mm", x = "Weeks", y = "") +
               facet_wrap(. ~ Types, ncol = 1, scale = "free_y",
                          strip.position = "right",
                          labeller = as_labeller(c('I' = "Infected",'D' = "Deaths",
                                                   'IR' = "IR", 'CFR' = "CFR",'TPR'="TPR")))
)


#### Epidemiological Weeks (EW) 

covid19 <- covid19 %>%
  left_join(
    filter(covid19, I.cum > 0) %>%
      group_by(country.alphacode) %>%
      summarise(date = date,
                EW = (year(date)-year(first(date)))*52 + week(date) - week(first(date)) + 1)) %>%
  mutate(EW = ifelse(is.na(EW),0,EW))

# First EW by Country
filter(covid19, EW == 1) %>%
  group_by(country.alphacode) %>%
  summarise(date = min(date), n = n()) %>%
  arrange(date) %>% head(25) %>%
  ggplot(aes(x = reorder(country.alphacode, date), y = date)) +
  geom_point() +
  scale_y_date(date_breaks = "1 day", date_labels = "%d %b") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold")) +
  labs(title = "First EW by Country", x = "Countries")

# Number of countries in the first EW
filter(covid19, EW == 1) %>%
  mutate(date = floor_date(date, unit = "month")) %>%
  group_by(country.alphacode) %>%
  summarise(date = min(date)) %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = date, y = n, fill = -n)) +
  geom_col() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"),
        legend.position = "none") +
  labs(title = "Number of countries in the first EW", 
       x = "Months", y = "Number of countries")

# IR progression in Weeks vs. EW
grid.arrange(
  covid19 %>%
    mutate(W = (year(date)-year(first(date)))*52 + week(date)) %>%
    group_by(W) %>%
    summarise(IR = mean(IR)) %>%
    ggplot(aes(x = W, y = IR)) +
    geom_col(fill = "yellow2", alpha = 0.8) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "IR by Weeks", x = "Weeks", y = "IR")
  ,
  covid19 %>%
    group_by(EW) %>%
    summarise(IR = mean(IR)) %>%
    ggplot(aes(x = EW, y = IR)) +
    geom_col(fill = "yellow2", alpha = 0.8) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "IR by Epidemiological Weeks (EW)", x = "EW", y = "IR")
)

# CFR progression in Weeks vs. EW
grid.arrange(
  covid19 %>%
    mutate(W = (year(date)-year(first(date)))*52 + week(date)) %>%
    group_by(W) %>%
    summarise(CFR = mean(CFR)) %>%
    ggplot(aes(x = W, y = CFR)) +
    geom_col(fill = "red", alpha = 0.5) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "CFR by Weeks", x = "Weeks", y = "CFR")
  ,
  covid19 %>%
    group_by(EW) %>%
    summarise(CFR = mean(CFR)) %>%
    ggplot(aes(x = EW, y = CFR)) +
    geom_col(fill = "red", alpha = 0.5) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "CFR by Epidemiological Weeks (EW)", x = "EW", y = "CFR")
)

# TPR progression in Weeks vs. EW
grid.arrange(
  covid19 %>%
    mutate(W = (year(date)-year(first(date)))*52 + week(date)) %>%
    group_by(W) %>%
    summarise(TPR = mean(TPR, na.rm = TRUE)) %>%
    ggplot(aes(x = W, y = TPR)) +
    geom_col(fill = "green", alpha = 0.5) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "TPR by Weeks", x = "Weeks", y = "TPR")
  ,
  covid19 %>%
    group_by(EW) %>%
    summarise(TPR = mean(TPR, na.rm = TRUE)) %>%
    ggplot(aes(x = EW, y = TPR)) +
    geom_col(fill = "green", alpha = 0.5) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "TPR by Epidemiological Weeks (EW)", x = "EW", y = "CFR")
)


#### Smoothing EW

covid19.s <- covid19.s %>%
  left_join(
    filter(covid19.s, I.cum > 0) %>%
      group_by(country.alphacode) %>%
      summarise(W = W,
                EW = W - first(W) + 1)) %>%
  mutate(EW = ifelse(is.na(EW),0,EW))

covid19.s.mm <- covid19.s.mm %>%
  left_join(
    filter(covid19.s.mm, I.cum > 0) %>%
      group_by(country.alphacode) %>%
      summarise(W = W,
                EW = W - first(W) + 1)) %>%
  mutate(EW = ifelse(is.na(EW),0,EW))

# Smoothing Infected in Weeks vs. EW
grid.arrange(
  covid19.s %>%
    group_by(W) %>%
    summarise(I = sum(I)) %>%
    ggplot(aes(x = W, y = I)) +
    geom_col(fill = "yellow2", alpha = 0.8) +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "Smoothing Infected by Weeks", x = "Weeks", y = "Infected")
  ,
  covid19.s %>%
    group_by(EW) %>%
    summarise(I = sum(I)) %>%
    ggplot(aes(x = EW, y = I)) +
    geom_col(fill = "yellow2", alpha = 0.8) +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "Smoothing Infected by EW", x = "EW", y = "Infected")
  ,
  covid19.s.mm %>%
    group_by(EW) %>%
    summarise(I = sum(I)) %>%
    ggplot(aes(x = EW, y = I)) +
    geom_col(fill = "yellow2", alpha = 0.8) +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "Smoothing Media Movil Infected by EW", x = "EW", y = "Infected")
)

# Smoothing Fatality in Weeks vs. EW
grid.arrange(
  covid19.s %>%
    group_by(W) %>%
    summarise(CFR = mean(CFR)) %>%
    ggplot(aes(x = W, y = CFR)) +
    geom_line(color = "red2", size = 1) +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "Smoothing Fatality by Weeks", x = "Weeks", y = "Infected")
  ,
  covid19.s %>%
    group_by(EW) %>%
    summarise(CFR = mean(CFR)) %>%
    ggplot(aes(x = EW, y = CFR)) +
    geom_line(color = "red2", size = 1) +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "Smoothing Fatality by EW", x = "EW", y = "Infected")
  ,
  covid19.s.mm %>%
    group_by(EW) %>%
    summarise(CFR = mean(CFR)) %>%
    ggplot(aes(x = EW, y = CFR)) +
    geom_line(color = "red2", size = 1) +
    scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black", face = "bold"), 
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    labs(title = "Smoothing Media Movil Fatality by EW", x = "EW", y = "Infected")
)

# Smoothing Indicators
grid.arrange(ncol = 1, 
             # Smoothing Group by Date
             covid19 %>%
               group_by(EW) %>%
               summarise(I = sum(I),
                         D = sum(D),
                         I.cum = sum(I.cum),
                         D.cum = sum(D.cum),
                         IR = (I.cum / N.total)*10^5,
                         CFR = (D.cum / I.cum)*100) %>%
               gather("Types", "Values", -EW) %>%
               ggplot(aes(x = EW, y = Values, fill = Types)) +
               geom_area(size = 1, alpha = 0.5) +
               scale_y_continuous(labels = label_number_si()) +
               scale_fill_manual(values = c("red4","red3","red2",
                                            "yellow3","yellow2","yellow4")) +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 6, color = "black"),
                     strip.text = element_text(size = 6, color = "black", face = "bold"),
                     title = element_text(size = 8, color = "black", face = "bold"), 
                     legend.title = element_blank(),
                     legend.position = "none") +
               labs(title = "Group by EW",
                    x = "", y = "") +
               facet_wrap(.~ Types, ncol = 6, scale = "free",
                          labeller = as_labeller(c('I' = "Infected",'D' = "Deaths",
                                                   'I.cum' = "Accumulated Infected", 
                                                   'D.cum' = "Accumulated Deaths",
                                                   'IR' = "IR", 'CFR' = "CFR")))
             ,
             covid19.s %>%
               group_by(EW) %>%
               summarise(I = sum(I),
                         D = sum(D),
                         I.cum = sum(I.cum),
                         D.cum = sum(D.cum),
                         IR = (I.cum / N.total)*10^5,
                         CFR = (D.cum / I.cum)*100) %>%
               gather("Types", "Values", -EW) %>%
               ggplot(aes(x = EW, y = Values, fill = Types)) +
               geom_area(size = 1, alpha = 0.5) +
               scale_y_continuous(labels = label_number_si()) +
               scale_fill_manual(values = c("red4","red3","red2",
                                            "yellow3","yellow2","yellow4")) +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 6, color = "black"),
                     strip.text = element_text(size = 6, color = "black", face = "bold"),
                     title = element_text(size = 8, color = "black", face = "bold"), 
                     legend.title = element_blank(),
                     legend.position = "none") +
               labs(title = "Smoothing Group by EW",
                    x = "", y = "") +
               facet_wrap(.~ Types, ncol = 6, scale = "free",
                          labeller = as_labeller(c('I' = "Infected",'D' = "Deaths",
                                                   'I.cum' = "Accumulated Infected", 
                                                   'D.cum' = "Accumulated Deaths",
                                                   'IR' = "IR", 'CFR' = "CFR")))
             ,
             covid19.s.mm %>%
               group_by(EW) %>%
               summarise(I = sum(I),
                         D = sum(D),
                         I.cum = sum(I.cum),
                         D.cum = sum(D.cum),
                         IR = (I.cum / N.total)*10^5,
                         CFR = (D.cum / I.cum)*100) %>%
               gather("Types", "Values", -EW) %>%
               ggplot(aes(x = EW, y = Values, fill = Types)) +
               geom_area(size = 1, alpha = 0.5) +
               scale_y_continuous(labels = label_number_si()) +
               scale_fill_manual(values = c("red4","red3","red2",
                                            "yellow3","yellow2","yellow4")) +
               theme_minimal() +
               theme(axis.text.x = element_text(size = 6, color = "black"),
                     axis.text.y = element_text(size = 6, color = "black"),
                     strip.text = element_text(size = 6, color = "black", face = "bold"),
                     title = element_text(size = 8, color = "black", face = "bold"), 
                     legend.title = element_blank(),
                     legend.position = "none") +
               labs(title = "Smoothing Media Movil Group by EW",
                    x = "", y = "") +
               facet_wrap(.~ Types, ncol = 6, scale = "free",
                          labeller = as_labeller(c('I' = "Infected",'D' = "Deaths",
                                                   'I.cum' = "Accumulated Infected", 
                                                   'D.cum' = "Accumulated Deaths",
                                                   'IR' = "IR", 'CFR' = "CFR")))
)


### Prediction of number of Infected

# Using Re to predict the number of infected
covid19.s.mm %>% 
  group_by(country.alphacode) %>%
  mutate(I.prev = lag(I, order_by = W, default = 0),
         Re.prev = lag(Re, order_by = W, default = 0)) %>%
  ungroup() %>%
  mutate(I.est = I.prev * Re.prev) %>%
  select(country.alphacode, W, I, I.prev, Re, Re.prev, I.est)

filter(covid19.s.mm) %>%
  group_by(country.alphacode) %>%
  mutate(I.prev = lag(I, order_by = W, default = 0),
         Re.prev = lag(Re, order_by = W, default = 0)) %>%
  ungroup() %>%
  mutate(I.est = I.prev * Re.prev) %>%
  group_by(W) %>%
  summarize(I = sum(I), I.est = sum(I.est)) %>%
  gather("Types", "Values", -W) %>%
  ggplot(aes(x = W, y = Values, color = Types)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("grey","yellow3"),
                     labels = c("Registered infected", "Infected Estimates")) +
  scale_x_continuous(labels = label_number_si(), n.breaks = 10) +
  scale_y_continuous(labels = label_number_si(), n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8, color = "black"),
        legend.position = "bottom",
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(x = "Week", y = "Infected")


### Prediction of number of Deaths

#### Infected ~ Deaths

m <- cor(covid19.s.mm$I.cum, covid19.s.mm$D.cum) * 
  sd(covid19.s.mm$D.cum) / sd(covid19.s.mm$I.cum)
b <- mean(covid19.s.mm$D.cum) - m * mean(covid19.s.mm$I.cum)

covid19.s.mm %>%
  ggplot(aes(x = I.cum, y = D.cum, color = country.alphacode)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = b, slope = m, size = 1) +
  scale_x_continuous(labels = label_number_si(), n.breaks = 10) +
  scale_y_continuous(labels = label_number_si(), n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        legend.position = "none",
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(x = "Infected", y = "Deaths")


#### Lalitude ~ Deaths

m <- cor(covid19.s.mm$latitude, covid19.s.mm$D.cum) * 
  sd(covid19.s.mm$D.cum) / sd(covid19.s.mm$latitude)
b <- mean(covid19.s.mm$D.cum) - m*mean(covid19.s.mm$latitude)

covid19.s.mm %>%
  group_by(latitude) %>%
  summarise(D.cum = mean(D.cum)) %>%
  ggplot(aes(x = latitude, y = D.cum)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = b, slope = m, size = 1) +
  scale_x_continuous(labels = label_number_si(), n.breaks = 10) +
  scale_y_continuous(labels = label_number_si(), n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        legend.position = "none",
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(x = "Latitudes", y = "Deaths")


#### Measure ~ Deaths

m <- cor(covid19.s.mm$measure.level, covid19.s.mm$D.cum) * 
  sd(covid19.s.mm$D.cum) / sd(covid19.s.mm$measure.level)
b <- mean(covid19.s.mm$D.cum) - m*mean(covid19.s.mm$measure.level)

covid19.s.mm %>%
  group_by(measure.level) %>%
  summarise(D.cum = mean(D.cum)) %>%
  ggplot(aes(x = measure.level, y = D.cum)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(intercept = b, slope = m, size = 1) +
  scale_x_continuous(labels = label_number_si(), n.breaks = 7) +
  scale_y_continuous(labels = label_number_si(), n.breaks = 5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        legend.position = "none",
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(x = "Measures", y = "Deaths")

rm(m, b)


### Predictive Model of Deaths by Convid19

#### Train and Test Set

# Create Train set and test set
# -----------------------------
create_sets <- function(data) {
  
  # test set will be 10% of data
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead
  test_index <- createDataPartition(y = data$country.alphacode, 
                                    times = 1, p = 0.1, list = FALSE)
  train_set <- data[-test_index,]
  test_set <- data[test_index,]
  
  return(list(train = train_set,test = test_set))
}  

sets <- create_sets(covid19)
train_set <- sets$train
test_set <- sets$test
rm(sets)

#### Daily Infected ~ Daily Deaths

death.model <- lm(D ~ I, data = train_set)

D.predict <- predict(death.model, newdata = test_set)

RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

rmse <- RMSE(test_set$D, D.predict)
rmse 

deaths.avg7 <- round(sum(filter(covid19, date > last(date)-7)$D)/7,0)
deaths.avg7

rmse / deaths.avg7*100

data.frame(test_set, D.predict = D.predict)  %>%
  group_by(date) %>%
  summarise(D = sum(D), D.predict = sum(D.predict)) %>%
  ggplot() +
  geom_area(aes(x = date, y = D), 
            fill = "red", size = 1, alpha = 0.5) +
  geom_line(aes(x = date, y = D.predict), 
            color = "black", size = 1, linetype = "solid") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = label_number_si(), n.breaks = 10) +
  theme_bw()


#### Daily Infected + latitude + measure.level +  season.num ~ Daily Deaths

death.model <- lm(D ~ I + latitude + measure.level + season.num, data = train_set)
D.predict <- predict(death.model, newdata = test_set)
rmse <- RMSE(test_set$D, D.predict)
rmse 

rmse / deaths.avg7*100


#### Daily Infected ~ Daily Deaths - Smoothing

sets <- create_sets(covid19.s.mm)
train_set <- sets$train
test_set <- sets$test
rm(sets)

death.model <- lm(D ~ I + latitude + measure.level + season.num, data = train_set)

D.predict <- predict(death.model, newdata = test_set)

rmse <- RMSE(test_set$D, D.predict)
rmse

deaths.avg7 <- round(sum(filter(covid19.s.mm)$D),0)
deaths.avg7

rmse / deaths.avg7*100

data.frame(test_set, D.predict = D.predict)  %>%
  group_by(W) %>%
  summarise(D = sum(D), 
            D.predict = sum(D.predict)) %>%
  ggplot() +
  geom_area(aes(x = W, y = D), 
            fill = "red", size = 1, alpha = 0.5) +
  geom_line(aes(x = W, y = D.predict), 
            color = "black", size = 0.5, linetype = "solid") +
  scale_x_continuous(labels = label_number_si(), n.breaks = 10) +
  scale_y_continuous(labels = label_number_si(), n.breaks = 10) +
  theme_bw()


### Second Waves

tweek <- last(covid19.s.mm$W)-1
nweeks <- 6
pTPR <- 5

covid19.stages <- 
  filter(covid19.s.mm, W %in% seq(tweek-(nweeks*3 - 1), tweek, 1)) %>%
  select(country.alphacode, W, I, TPR) %>%
  mutate(stage = ceiling(3 + (W-tweek) / nweeks)) %>%
  group_by(country.alphacode, stage) %>%
  summarise(I.max = max(I), TPR.max = max(TPR, na.rm = TRUE))

filter(covid19.stages, country.alphacode %in% c("CHL","USA","ESP","GBR"))

covid19.phases <-
  select(covid19.stages, -TPR.max) %>%
  spread(stage, I.max) %>%
  rename(I.max_1 = `1`, I.max_2 = `2`, I.max_3 = `3`) %>%
  left_join(select(covid19.stages, -I.max) %>%
              spread(stage, TPR.max) %>%
              rename(TPR.max_1 = `1`, TPR.max_2 = `2`, TPR.max_3 = `3`)
  ) %>%
  mutate(phase = case_when(
    # Uncertain
    is.infinite(TPR.max_3) 
    ~ "5. Uncertain", 
    
    # Containment
    (I.max_1 >= I.max_2 & I.max_2 >= I.max_3 & TPR.max_3 < pTPR) |
      (I.max_1 >= I.max_2 & max(TPR.max_2, TPR.max_3) < pTPR) |
      (max(TPR.max_1, TPR.max_2, TPR.max_3) < pTPR)
    ~ "4. Containment",
    
    # Pre-containment
    (I.max_1 < I.max_2 & I.max_2 >= I.max_3) |
      (I.max_1 >= I.max_2 & I.max_2 >= I.max_3 & TPR.max_3 >= pTPR) |
      (I.max_1 >= I.max_2 & I.max_2 < I.max_3 & TPR.max_3 < pTPR)
    ~ "3. Pre-containment",
    
    # Second waves
    I.max_1 > I.max_2 & I.max_3 > I.max_2 & TPR.max_3 >= pTPR
    ~ "2. Second waves",
    
    # Next to peak
    I.max_3 >= I.max_2 & I.max_2 >= I.max_1 & TPR.max_3 >= pTPR
    ~ "1. Next to peak"
  ))

# Covid19 by Countries Phases
map.world %>%
  left_join(left_join(covid19.phases, Isocode)) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = phase), color = "steelblue2", size = 0.1) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(colour= "black", 
                                        fill = "lightblue", 
                                        size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  ggtitle("Covid19 by Countries Phases") +
  labs(fill = "Phase")

rm(tweek, nweeks, pTPR, covid19.stages, covid19.phases)

# Calculate Phases 
covid19.calculate.phases <- function(data, 
                                     tweek = last(data$W)-1, 
                                     nweeks = 6, pTPR = 3) {
  
  covid19.stages <- filter(data, W %in% seq(tweek-(nweeks*3 - 1), tweek, 1)) %>%
    select(country.alphacode, W, I, TPR) %>%
    mutate(stage = ceiling(3 + (W-tweek) / nweeks)) %>%
    group_by(country.alphacode, stage) %>%
    summarise(I.max = max(I), TPR.max = max(TPR, na.rm = TRUE))
  
  select(covid19.stages, -TPR.max) %>%
    spread(stage, I.max) %>%
    rename(I.max_1 = `1`, I.max_2 = `2`, I.max_3 = `3`) %>%
    left_join(select(covid19.stages, -I.max) %>%
                spread(stage, TPR.max) %>%
                rename(TPR.max_1 = `1`, TPR.max_2 = `2`, TPR.max_3 = `3`)
    ) %>%
    mutate(
      phase = case_when(
        # Uncertain
        is.infinite(TPR.max_3) 
        ~ "5. Uncertain", 
        
        # Containment
        (I.max_1 >= I.max_2 & I.max_2 >= I.max_3 & TPR.max_3 < pTPR) |
          (I.max_1 >= I.max_2 & max(TPR.max_2, TPR.max_3) < pTPR) |
          (max(TPR.max_1, TPR.max_2, TPR.max_3) < pTPR)
        ~ "4. Containment",
        
        # Pre-containment
        (I.max_1 < I.max_2 & I.max_2 >= I.max_3) |
          (I.max_1 >= I.max_2 & I.max_2 >= I.max_3 & TPR.max_3 >= pTPR) |
          (I.max_1 >= I.max_2 & I.max_2 < I.max_3 & TPR.max_3 < pTPR)
        ~ "3. Pre-containment",
        
        # Second waves
        I.max_1 > I.max_2 & I.max_3 > I.max_2 & TPR.max_3 >= pTPR
        ~ "2. Second waves",
        
        # Next to peak
        I.max_3 >= I.max_2 & I.max_2 >= I.max_1 & TPR.max_3 >= pTPR
        ~ "1. Next to peak"
      )
    )
}

# Phases in July
covid19.phases <- covid19.calculate.phases(data = covid19.s.mm,
                                           tweek = week("2020-07-31"))

# Covid19 by Countries Phases
map.world %>%
  left_join(left_join(covid19.phases, Isocode)) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = phase), color = "steelblue2", size = 0.1) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(colour= "black", 
                                        fill = "lightblue", 
                                        size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  ggtitle("Covid19 by Countries Phases") +
  labs(fill = "Phase")

# Current phases
covid19.phases <- covid19.calculate.phases(covid19.s.mm)

# Covid19 by Countries Phases with function
map.world %>%
  left_join(left_join(covid19.phases, Isocode)) %>%
  ggplot(aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = phase), color = "steelblue2", size = 0.1) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_rect(colour= "black", 
                                        fill = "lightblue", 
                                        size = 0.5),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) +
  ggtitle("Covid19 by Countries Phases") +
  labs(fill = "Phase")

group_by(covid19.phases, phase) %>%
  summarise(n = n())

# Current phase of the countries
filter(covid19.phases, phase != "5. Uncertain") %>%
  mutate(continent.name = countrycode(sourcevar = country.alphacode, 
                                      origin = "iso3c", destination = "continent")) %>%
  ggplot(aes(x = I.max_3, 
             y = reorder_within(country.alphacode, desc(country.alphacode), phase),
             color = continent.name, size = TPR.max_3)) +
  geom_point() +
  scale_y_reordered() +
  scale_x_continuous(labels = label_number_si()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        legend.text = element_text(size = 8, color = "black"),
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(title = "Current phase of the countries", 
       x = "Max Infected (7 days mm)", y = "Countries",
       size = "Positive Rate", color = "Continent") +
  facet_wrap(. ~ phase, nrow = 1, scale = "free")


# --------
# RESULTS
# --------

### Indicators Tops 

# IR Top 10
library(gridExtra)
library(grid)
grid.arrange(ncol = 1, 
             top = textGrob("IR Top", gp = gpar(fontsize = 12, font = 2)),
             filter(covid19, date == last(date)) %>%
               filter(I.cum > mean(I.cum)) %>% 
               top_n(10, wt = IR) %>%
               ggplot(aes(x = reorder(country.alphacode, IR), y = IR, fill = IR)) +
               geom_col() +
               coord_flip() +
               scale_y_continuous(labels = comma) +
               scale_fill_gradientn(colours = brewer.pal(3, "YlOrRd" )) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 8, color = "black", hjust = 1),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_text(size = 10, color = "black", face = "bold"),
                     title = element_text(size = 12, color = "orange3", face = "bold")) +
               labs(y = "IR", x = element_blank()),
             filter(covid19, date == last(date)) %>%
               filter(I.cum > mean(I.cum)) %>% 
               top_n(10, wt = IR) %>%
               ggplot(aes(x = reorder(country.alphacode, IR), y = N/10^6, fill = N)) +
               geom_col() +
               coord_flip() +
               scale_y_continuous(labels = comma) +
               scale_fill_gradientn(colours = brewer.pal(3, "Blues" )) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 8, color = "black", hjust = 1),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_text(size = 10, color = "black", face = "bold"),
                     title = element_text(size = 12, color = "blue3", face = "bold")) +
               labs(y = "Population (Millions)", x = element_blank()))

# Deaths Top 10
grid.arrange(ncol = 1, 
             top = textGrob("Deaths Top", gp = gpar(fontsize = 12, font = 2)),
             filter(covid19, date == last(date)) %>%
               filter(I.cum > mean(I.cum)) %>% 
               top_n(10, wt = CFR) %>%
               ggplot(aes(x = reorder(country.alphacode, CFR), y = CFR, fill = CFR)) +
               geom_col() +
               coord_flip() +
               scale_y_continuous(labels = comma) +
               scale_fill_gradientn(colours = brewer.pal(3, "Reds" )) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 8, color = "black", hjust = 1),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_text(size = 10, color = "black", face = "bold"),
                     title = element_text(size = 12, color = "Red3", face = "bold")) +
               labs(y = "CFR", x = element_blank()), 
             filter(covid19, date == last(date)) %>%
               filter(I.cum > mean(I.cum)) %>% 
               top_n(10, wt = M) %>%
               ggplot(aes(x = reorder(country.alphacode, M), y = M, fill = M)) +
               geom_bar(stat = "identity") +
               coord_flip() +
               scale_y_continuous(labels = comma) +
               scale_fill_gradientn(colours = brewer.pal(3, "Reds" )) +
               theme_bw() +
               theme(axis.text.x = element_text(size = 8, color = "black", hjust = 1),
                     axis.text.y = element_text(size = 8, color = "black"),
                     axis.title.x = element_text(size = 10, color = "black", face = "bold"),
                     axis.title.y = element_text(size = 10, color = "black", face = "bold"),
                     title = element_text(size = 12, color = "Red3", face = "bold")) +
               labs(y = "Mortality", x = element_blank()) )


## Population of Countries with higher than average number of Infected

# N Top 10: Countries with higher than average number of Infected
filter(covid19, date == last(date)) %>%
  filter(I.cum > mean(I.cum)) %>% 
  top_n(10, wt = N) %>%
  ggplot(aes(x = reorder(country.alphacode, N), y = N, fill = N)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_gradientn(colours = brewer.pal(3, "Blues" )) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 12, color = "Blue3", face = "bold")) +
  labs(title = "Population Top 10", y = "Population", x = element_blank()) 

# All together
filter(covid19, date == last(date)) %>%
  filter(I.cum > mean(I.cum)) %>% 
  top_n(10, wt = IR) %>%
  select(country.name, country.alphacode, IR, CFR, M, N) %>%
  gather("Type", "Value", -country.name, -country.alphacode) %>%
  group_by(Type) %>%
  top_n(10, wt = Value) %>%
  ungroup() %>%
  mutate(alphacode = reorder_within(country.alphacode, Value, Type)) %>%
  ggplot(aes(x = reorder(alphacode, Value), y = Value, 
             fill = country.name)) +
  geom_col() +
  facet_wrap(Type ~ ., ncol = 2, scales = "free", 
             labeller = as_labeller(c(
               'CFR' = "Fatality Rate", 
               'IR' = "Infected Rate", 
               'M' = "Mortality", 
               'N' = "Population"))) +
  scale_x_reordered() +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, color = "black"),
        axis.text.y = element_text(size = 6, color = "black", face ="bold"),
        axis.title = element_text(size = 8, color = "black", face ="bold"),
        legend.text = element_text(size = 6, color="black", face ="bold"),
        legend.title = element_text(size = 8, color="black", face ="bold"),
        strip.text.x = element_text(size = 8, color="black", face ="bold")) +
  labs(title = "Indicators Top 10", 
       x = "", y = "Indicators Value", fill = "Countries")


### Rankig Indicators progression

# IR Ranking by month
filter(covid19, 
       date %in% as.Date(seq(ceiling_date(first(covid19$date), "month"), 
                             last(covid19$date), by = "1 month"))) %>%
  group_by(date) %>%
  mutate(I.rate = mean(I.cum),
         IR.rk = rank(IR)) %>%
  filter(I.cum > I.rate) %>%
  top_n(10, wt = IR.rk) %>%
  arrange(date, IR.rk) %>%
  ggplot(aes(x = IR.rk, y = reorder_within(country.alphacode, IR.rk, date), fill = IR.rk)) +
  geom_col() +
  scale_x_continuous(labels = label_number_si()) +
  scale_y_reordered() +
  scale_fill_gradientn(colours = brewer.pal(3, "YlOrRd" )) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black", hjust = 1),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 12, color = "yellow3", face = "bold")) +
  labs(title = "Rankig IR progression", x = "IR Ranking", y = "Countries") +
  facet_wrap(~ month(date, label = TRUE, abbr = FALSE), 
             ncol = 4, scales = "free")

# Current Top 10 Ranking IR
covid19.IR.Top10 <- 
  filter(covid19, date == last(covid19$date)) %>%
  mutate(I.rate = mean(I.cum),
         IR.rk = rank(IR)) %>%
  filter(I.cum > I.rate) %>%
  top_n(10, wt = IR.rk) %>%
  arrange(-IR.rk) %>%
  mutate(IR.rk =  row_number()) %>%
  select(country.alphacode, IR, IR.rk) %>%
  arrange(IR.rk)

covid19.IR.Top10 

# Pregression of Top Ranking IR countries
filter(covid19, 
       country.alphacode %in% covid19.IR.Top10$country.alphacode &
         date %in% as.Date(seq(ceiling_date(first(covid19$date), "month"), 
                               last(covid19$date), by = "1 month")-1)) %>%
  group_by(date) %>%
  mutate(I.rate = mean(I.cum),
         IR.rk = rank(IR)) %>%
  top_n(10, wt = IR.rk) %>%
  arrange(-IR.rk) %>%
  mutate(IR.rk =  row_number()) %>%
  ggplot(aes(x = date, y = IR.rk, color = country.alphacode)) +
  geom_line(position = "identity", size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_reverse(labels = label_number_si(), breaks = 1:10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black", hjust = 1),
        axis.text.y = element_text(size = 6, color = "black"),
        axis.title.x = element_text(size = 10, color = "black", face = "bold"),
        axis.title.y = element_text(size = 10, color = "black", face = "bold"),
        legend.position = "none",
        title = element_text(size = 12, color = "yellow3", face = "bold")) +
  labs(title = "Rankig IR progression", x = "Months", y = "Ranking") +
  facet_wrap(. ~ country.name, ncol = 2)


#### Indicators Comparisson by Measures

# Lockdown: measure.level == 6 

# IR group by Measure and EW 
covid19.s.mm %>%
  mutate(lockdown = ifelse(measure.level == 6, "Yes", "No")) %>%
  group_by(EW, lockdown) %>%
  summarise(IR.week = mean(IR)) %>%
  ggplot(aes(x = EW, y = IR.week, color = lockdown)) +
  geom_line(size = 1.5) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  scale_color_manual(values = c("grey","yellow2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "IR by Measure", x = "EW", y = "IR")

# CFR group by Measure and EW 
covid19.s.mm %>%
  mutate(lockdown = ifelse(measure.level == 6, "Yes", "No")) %>%
  group_by(EW, lockdown) %>%
  summarise(CFR.week = mean(CFR)) %>%
  ggplot(aes(x = EW, y = CFR.week, color = lockdown)) +
  geom_line(size = 1.5) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  scale_color_manual(values = c("grey","red2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "CFR by Measure", x = "EW", y = "CFR")

# After median week:
filter(covid19.s.mm, EW > median(covid19.s.mm$EW)) %>%
  mutate(lockdown = ifelse(measure.level == 6, "Yes", "No")) %>%
  group_by(EW, lockdown) %>%
  summarise(IR.week = mean(IR)) %>%
  ggplot(aes(x = EW, y = IR.week, color = lockdown)) +
  geom_line(size = 1.5) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  scale_color_manual(values = c("grey","yellow2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "IR by Measure", x = "EW", y = "CFR")

filter(covid19.s.mm, EW > median(covid19.s.mm$EW)) %>%
  mutate(lockdown = ifelse(measure.level == 6, "Yes", "No")) %>%
  group_by(EW, lockdown) %>%
  summarise(CFR.week = mean(CFR)) %>%
  ggplot(aes(x = EW, y = CFR.week, color = lockdown)) +
  geom_line(size = 1.5) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  scale_color_manual(values = c("grey","red2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "CFR by Measure", x = "EW", y = "CFR")

filter(covid19.s.mm, W == last(W)) %>%
  mutate(lockdown = ifelse(measure.level == 6, "Yes", "No")) %>%
  group_by(lockdown) %>%
  summarise(quantity = n(), CFR.mean = mean(CFR), population = sum(N))

filter(covid19.s.mm, W == last(W) & measure.level == 6) %>%
  ggplot(aes(x = CFR, y = reorder(country.alphacode, N))) +
  geom_col(color = "red2", fill = "red2", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(title = "Countries without lockdown", x = "Fatality", y = "Countries") +
  facet_wrap(. ~ continent.name, nrow = 1, scale = "free_y")

filter(covid19.s.mm, W == last(W) & measure.level != 6) %>%
  ggplot(aes(x = CFR, y = reorder(country.alphacode, N))) +
  geom_col(color = "red2", fill = "red2", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(title = "Countries with no lockdown", x = "Fatality", y = "Countries") +
  facet_wrap(. ~ continent.name, nrow = 1, scale = "free_y")


## Predict infected for the next few weeks

# Predict Infected for next week
covid19.I.nextweek <- function(data, nweeks = 1) {
  data <- mutate(data, Type = "Real") %>%
    select(country.alphacode, W, I, latitude, measure.level, season.num, I.cum, Re, Type)
  lastweek <- last(data$W) 
  
  for(i in 1:nweeks) {
    pred <- group_by(data, country.alphacode) %>%
      mutate(I = I*(1+Re/100), 
             I.cum = I.cum + I,
             rollmean(Re, 3, fill = c(0,0,0), align = "right")) %>%
      filter(W == lastweek) %>%
      mutate(W = W + 1, Type = "Predict") %>%
      select(country.alphacode, W, I, latitude, measure.level, season.num, I.cum, Re, Type)
    
    lastweek <- lastweek + 1
    data <- rbind(data, pred)
  }
  data
}

# Predict the next 3 weeks Infected
covid19.predict <- covid19.I.nextweek(data = covid19.s.mm, nweeks = 3)

filter(covid19.predict, 
       country.alphacode %in% c("USA", "BRA", "IND", "ESP","JPN")) %>%
  left_join(Isocode) %>%
  mutate(datew = as.Date(paste0(2020,"-01-01"))+W*7-1) %>%
  ggplot(aes(x = datew, y = I, color = Type)) +
  geom_line(color = "yellow3") +
  geom_point() +
  scale_x_date(date_breaks = "4 week", date_labels = "%d/%b") +
  scale_y_continuous(labels = label_number_si()) +
  scale_color_manual(values = c("black", "yellow3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        legend.position = "bottom",
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(title = "Predict Infected", x = "Weeks", y = "Weekly Infected") +
  facet_wrap(. ~ country.name, ncol = 1, scale = "free_y")


## Predict deaths for the next few weeks

# Predict Deaths for next week
covid19.D.nextweek <- function(data, model, nweeks = 1) {
  data.next <- mutate(data, Type = "Real") %>%
    select(country.alphacode, W, I, latitude, measure.level, season.num, I.cum, Re, Type, D)
  
  lastweek <- last(data.next$W) 
  
  for(i in 1:nweeks) {
    pred <- covid19.I.nextweek(filter(data.next, W == lastweek), nweeks = 1) %>%
      mutate(W = lastweek + 1, Type = "Predict") %>%
      select(country.alphacode, W, I, latitude, measure.level, season.num, I.cum, Re, Type)
    
    pred$D <- predict(model, newdata = pred)
    lastweek <- lastweek + 1
    data.next <- rbind(data.next, pred)
  }
  data.next
}

# Predict the next 3 weeks deaths
covid19.predict <- covid19.D.nextweek(data = filter(covid19.s.mm, W < last(W)), 
                                      model = death.model, 
                                      nweeks = 3)

filter(covid19.predict, 
       country.alphacode %in% c("USA", "BRA", "IND", "ESP","JPN")) %>%
  left_join(Isocode) %>%
  mutate(datew = as.Date(paste0(2020,"-01-01"))+W*7-1) %>%
  ggplot(aes(x = datew, y = D, color = Type)) +
  geom_line(color = "red2") +
  geom_point() +
  scale_x_date(date_breaks = "4 week", date_labels = "%d/%b") +
  scale_y_continuous(labels = label_number_si()) +
  scale_color_manual(values = c("black", "red2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        strip.text = element_text(size = 10, color = "black", face = "bold"),
        legend.position = "bottom",
        title = element_text(size = 10, color = "black", face = "bold")) +
  labs(title = "Predict Deaths", x = "Weeks", y = "Weekly Deaths") +
  facet_wrap(. ~ country.name, ncol = 1, scale = "free_y")


# -----
# OTROS
# -----

filter(covid19, country.alphacode %in% c("CHL", "ESP", "GBR", 
                                              "SWE","USA", "NOR")) %>%
  mutate(lockdown = ifelse(measure.level == 6, "Yes", "No")) %>%
  group_by(date, country.alphacode) %>%
  summarise(CFR.week = mean(CFR)) %>%
  rbind(group_by(covid19, date) %>%
          summarise(country.alphacode = "WORLD", CFR.week = mean(CFR))) %>%
  ggplot(aes(x = date, y = CFR.week, 
             color = country.alphacode)) +
  geom_line() +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "Lockdown and CFR", x = "EW", y = "CFR", color = "Countries") 

filter(covid19.s.mm, country.alphacode %in% c("CHL", "ESP", "GBR", 
                                              "SWE","USA", "NOR")) %>%
  mutate(lockdown = ifelse(measure.level == 6, "Yes", "No")) %>%
  group_by(EW, country.alphacode) %>%
  summarise(CFR.week = mean(CFR)) %>%
  rbind(group_by(covid19.s.mm, EW) %>%
          summarise(country.alphacode = "WORLD", CFR.week = mean(CFR))) %>%
  ggplot(aes(x = EW, y = CFR.week, color = country.alphacode)) +
  geom_line(size = 1) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10, color = "black", face = "bold"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(title = "Lockdown and CFR", x = "EW", y = "CFR", color = "Countries") 
