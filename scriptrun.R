# Clear Enviroment
rm(list = ls())

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

# Get wpp2019 data (United Nations)
pop.2020 <- read.csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv",
                     fileEncoding = "UTF-8-BOM") %>%
    filter(Time == 2020 & VarID == 2)

##### Countries
Isocode <- read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv",
                    fileEncoding = "UTF-8-BOM") %>%
    mutate(country.name = as.character(Country),
           country.alphacode = str_remove(as.character(Alpha.3.code)," ")) %>%
    rename(country.isocode = Numeric.code,
           latitude = Latitude..average.,
           longitude = Longitude..average.) %>%
    select(country.name, country.alphacode, country.isocode, latitude, longitude)

##### Continents and Regions

library(countrycode)

#### Our World Data Dataset

owd_data <- read.csv("https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv",
                     fileEncoding = "UTF-8-BOM") %>%
    mutate(date = as.Date(date)) %>%
    rename(country.alphacode = iso_code)

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

covid19 <- covid19 %>%
    filter(!(country.name %in% c("Diamond Princess", "MS Zaandam")))

### Iso Codes

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

measures <- read_acaps() %>% left_join(measures_category)

### Population data

pop.2020 <- pop.2020 %>%
    mutate(country.name = as.character(Location),
           country.isocode = as.numeric(LocID)) %>%
    select(country.name, country.isocode, PopTotal)

### Add Isocode to covid19

covid19 <- covid19 %>%
    left_join(Isocode)

covid19 <- covid19 %>%
    mutate(continent.name = countrycode(sourcevar = country.alphacode, 
                                        origin = "iso3c", destination = "continent"),
           region.name = countrycode(sourcevar = country.alphacode, 
                                     origin = "iso3c", destination = "region23"))

covid19$continent.name[which(covid19$country.name == "Kosovo")] <- "Europe"
covid19$region.name[which(covid19$country.name == "Kosovo")] <- "Southern Europe"
covid19$region.name[which(covid19$country.name == "Western Sahara")] <-"Sub-Saharan Africa"

# Add Population to covid19
covid19 <- covid19 %>%
    left_join(pop.2020, by = "country.isocode") %>% 
    mutate(country.name = country.name.x,
           N = PopTotal*1000) %>%
    select(-country.name, -country.name.y, -PopTotal) %>%
    rename(country.name = country.name.x)

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

# World Map
library(mapdata)

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

N.total <- sum(filter(covid19, date == last(date))$N)

### Calculation of the effective Reporductive Number (Re)

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

### Add Re to Covid19

covid19 <- covid19 %>% left_join(covid19.Re)

rm(covid19.Re)

### Correlations

covid19 <- mutate(covid19, 
                  season.num = case_when(season == "winter" ~ 1,
                                         season == "spring" ~ 2,
                                         season == "summer" ~ 3,
                                         season == "fall" ~ 4))

### Smoothing

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

#### Epidemiological Weeks (EW) 

covid19 <- covid19 %>%
    left_join(
        filter(covid19, I.cum > 0) %>%
            group_by(country.alphacode) %>%
            summarise(date = date,
                      EW = (year(date)-year(first(date)))*52 + week(date) - week(first(date)) + 1)) %>%
    mutate(EW = ifelse(is.na(EW),0,EW))

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

### Prediction of number of Infected

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

RMSE <- function(true, predicted){
    sqrt(mean((true - predicted)^2))
}

#### Daily Infected ~ Daily Deaths - Smoothing

sets <- create_sets(covid19.s.mm)
train_set <- sets$train
test_set <- sets$test
rm(sets)

death.model <- lm(D ~ I + latitude + measure.level + season.num, data = train_set)

D.predict <- predict(death.model, newdata = test_set)

### Second Waves

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

# Current phases
covid19.phases <- covid19.calculate.phases(covid19.s.mm)

### Indicators Tops 

# IR Top 10
library(gridExtra)
library(grid)

### Rankig Indicators progression

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

#### Indicators Comparisson by Measures

# Lockdown: measure.level == 6 

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
