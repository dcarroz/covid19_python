import covid19_functions as cf

# ---------
# ANALYSIS
# ---------
print('\n==== ANALYSIS ====\n')

### CSSE: Covid data
print('\n==== CSSE data ====\n')
covid19 = cf.get_cssedata()
print(covid19.head())
print(covid19.tail())

### Iso Codes
print('\n==== ISO code data ====\n')
# Countries with other names
Isocode = cf.get_Isocodes()
# antijoin
print(covid19.merge(Isocode, how='left', on='country_name', indicator=True
                    ).query("_merge == 'left_only'").drop(columns='_merge').drop_duplicates(subset ="country_name"))
# Update countries' names
covid19 = cf.update_countries_names(covid19)

# Continents and Regions
countrycode = cf.get_countrycode()

### ACAPS: Measure data
print('\n==== Measure Category data ====\n')
measures_category = cf.measures_category()
print(measures_category)
print('\n==== Measure data ====\n')
measures = cf.get_measures()
print(measures.head())

### Test Positivity Rate
owd_data = cf.get_test_data()
print(owd_data.head())

### Population data
print('\n==== Population data ====\n')
pop2020 = cf.get_population()
print(pop2020.head())

### Add Isocode to covid19
covid19 = covid19.merge(Isocode, how='left')
print(covid19.head())

### Add Continents and Regions
covid19 = covid19.merge(countrycode.drop(columns=['country_name']), how='left')
covid19.loc[covid19['country_name'] == 'Kosovo','continent_name'] = 'Europe'
covid19.loc[covid19['country_name'] == 'Kosovo','region_name'] = 'Southern Europe'
covid19.loc[covid19['country_name'] == 'Western Sahara','region_name'] = 'Sub-Saharan Africa'

### Add Population
covid19 = covid19.merge(pop2020.drop(columns=['country_name']), how='left', on='country_isocode'
                        ).assign(N=lambda x: x['PopTotal']*1000).drop(columns=['PopTotal'])
covid19.loc[covid19['country_name'] == 'Kosovo','N'] = 1811285

### Add Measures level
measures = cf.get_measures()
covid19 = covid19.merge(measures.drop(columns=['measure_category']), how='left', on=['country_alphacode','date'])\
    .assign(ml_ant=lambda x: x.groupby(['country_alphacode'])['measure_level'].shift(1).fillna(0))
covid19 = covid19.assign(measure_level=covid19[['measure_level','ml_ant']].max(axis=1)).drop(columns=['ml_ant'])

### Add Test data
covid19 = covid19.merge(cf.get_test_data(), how='left').assign(TPR = lambda x: x['positive_rate']*100).\
    drop(columns=['positive_rate'])

#### Add S (susceptible)
covid19['S'] = covid19['N'] - covid19['I_cum'] - covid19['R_cum']

#### Add IR (Incidence Rate)
covid19['IR'] = (covid19['I_cum']/covid19['N'])*10**5

#### Add CFR (Confirmed Fatality rate) and M (Mortality)
covid19['CFR'] = (covid19['D_cum']/covid19['I_cum']*100).fillna(0)
covid19['M'] = (covid19['D_cum']/covid19['N'])*10**5

print(covid19.iloc[:10,:10])
print(covid19.iloc[:10,10:])
print(covid19.iloc[:-10,:10])
print(covid19.iloc[:-10,10:])