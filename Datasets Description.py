import csv
import pandas as pd

### Pandemic Data

#### Johns Hopkins University - Center for Systems Science and Engineering (CSSE)

csse_data = pd.read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
print(csse_data[list(csse_data.columns[0:4])+list(csse_data.columns[-4:])].head(5))
del csse_data

### Demographic data

#### Population

# Get wpp2019 data (United Nations)
pop2020 = pd.read_csv('https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv'
                      ).query('Time == 2020 and VarID == 2')
print(pop2020.head())

#### Geographical Position

##### Countries
Isocode = pd.read_csv('https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv')
Isocode[['country_alphacode','country_isocode','latitude','longitude']] = \
    Isocode[['Alpha-3 code','Numeric code','Latitude (average)','Longitude (average)']].\
        apply(lambda x: x.str.replace('"','').str.strip())
Isocode = Isocode.rename(columns={'Country':'country_name'}
                         )[['country_name','country_alphacode','country_isocode','latitude','longitude']].\
    astype({'country_isocode':'int','latitude':'float64','longitude':'float64'})
print(Isocode.head())

##### Continents and Regions

countrycode = pd.read_csv('venv//Lib//site-packages//countrycode//data//countrycode_data.csv'
                       ).rename(columns={'iso3c':'country_alphacode'}
                                )[['country_name','country_alphacode','continent','region']]
print(countrycode.head())

### Measures data

#### ACAPS Dataset

measures = pd.read_excel('https://www.acaps.org/sites/acaps/files/resources/files/acaps_covid19_government_measures_dataset_0.xlsx',
                         engine='openpyxl', sheet_name='Dataset')
print(measures[:8].head())
print(pd.unique(measures['CATEGORY']))
print(pd.unique(measures['MEASURE']))

### Test Positivity Rate

#### Our World Data Dataset
owd_data = pd.read_csv('https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv'
              ).rename(columns={'iso_code':'country_alphacode'}
                       ).astype({'date':'datetime64[ns]','positive_rate':'float64'})
print(owd_data[['country_alphacode', 'date', 'positive_rate']].tail())
