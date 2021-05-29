import pandas as pd

# Update countries' names
def update_countries_names(df):
    return df.replace({
        "Cabo Verde":"Cape Verde",
        "Congo (Brazzaville)":"Congo",
        "Congo (Kinshasa)":"Congo, the Democratic Republic of the",
        "Cote d'Ivoire":"Ivory Coast",
        "Czechia":"Czech Republic",
        "Eswatini":"Swaziland",
        "Holy See":"Holy See (Vatican City State)",
        "Iran":"Iran, Islamic Republic of",
        "Korea, South":"South Korea",
        "Laos":"Lao People's Democratic Republic",
        "Moldova":"Moldova, Republic of",
        "North Macedonia":"Macedonia, the former Yugoslav Republic of",
        "Syria":"Syrian Arab Republic",
        "Tanzania":"Tanzania, United Republic of",
        "US":"United States",
        "West Bank and Gaza":"Palestinian Territory, Occupied",
        "Micronesia":"Micronesia, Federated States of"
    })

# Read CSSE data from github
def read_csse(file, numvars, namecol):
    df = pd.read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/'+
                          'csse_covid_19_data/csse_covid_19_time_series/'+file)
    # change de wide shape to long shape
    df = df.melt(id_vars=list(df.columns[0:numvars]),var_name='date',value_name=namecol
                 ).astype({'date':'datetime64[ns]', namecol:'int64'})
    return df

# get current CSSE data
def get_cssedata(updatenames = False):
    df = read_csse('time_series_covid19_confirmed_global.csv', 4, 'I_cum').\
        merge(read_csse('time_series_covid19_deaths_global.csv', 4, 'D_cum'),how='left').\
        merge(read_csse('time_series_covid19_recovered_global.csv', 4, 'R_cum'),how='left')
    df['country_name'] = df['Country/Region'].apply(lambda x: x.replace('*', ''))
    df['date'] = pd.to_datetime(df['date']).apply(lambda x: x.date())
    # only countries
    df = df.groupby(['country_name','date'],as_index=False).agg({'I_cum':'sum','D_cum':'sum','R_cum':'sum'})
    # I, D, R
    df = df.assign(I=lambda x: x['I_cum']-x.groupby(['country_name'])['I_cum'].shift(1),
                   D=lambda x: x['D_cum']-x.groupby(['country_name'])['D_cum'].shift(1),
                   R=lambda x: x['R_cum']-x.groupby(['country_name'])['R_cum'].shift(1)
                   )[['country_name', 'date', 'I', 'D', 'R', 'I_cum', 'D_cum', 'R_cum']]
    # not ships
    df = df.fillna(0).query('country_name not in ["Diamond Princess", "MS Zaandam"]')
    if updatenames: df = update_countries_names(df)
    return df

# get Isocodes
def get_Isocodes():
    df = pd.read_csv('https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv')
    df[['country_alphacode','country_isocode','latitude','longitude']] = \
    df[['Alpha-3 code','Numeric code','Latitude (average)','Longitude (average)']].\
        apply(lambda x: x.str.replace('"','').str.strip())
    df = df.rename(columns={'Country':'country_name'}
                         )[['country_name','country_alphacode','country_isocode','latitude','longitude']].\
        astype({'country_isocode':'int','latitude':'float64','longitude':'float64'})
    # Sudan Iso code
    df.loc[df['country_name'] == 'Sudan', 'country_isocode'] = 729
    # Insert Kosovo
    df = df.append({'country_name': 'Kosovo', 'country_alphacode': 'XXK', 'country_isocode': 383,
                    'latitude': 42.6675, 'longitude': 21.1662}, ignore_index=True)
    return df

# Population
def get_population(year=2020):
    return pd.read_csv('https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv'
                      ).query('Time == '+str(year)+' and VarID == 2'
                              ).rename(columns={'Location':'country_name', 'LocID':'country_isocode'}
                                       ).astype({'country_isocode':'int', 'PopTotal':'float64'}
                                                )[['country_name', 'country_isocode', 'PopTotal']]

# Continents and Regions
def get_countrycode():
    df = pd.read_csv('venv//Lib//site-packages//countrycode//data//countrycode_data.csv'
                       ).rename(columns={'iso3c':'country_alphacode',
                                         'continent':'continent_name',
                                         'region':'region_name'}
                                )[['country_name', 'country_alphacode', 'continent_name', 'region_name']]
    df['country_name'] = df['country_name'].str.capitalize()
    return df

# read ACAPS data
def read_acaps():
    return pd.read_excel(
        'https://www.acaps.org/sites/acaps/files/resources/files/acaps_covid19_government_measures_dataset_0.xlsx',
        engine='openpyxl', sheet_name='Dataset'
    ).rename(columns={'DATE_IMPLEMENTED':'date', 'ISO':'country_alphacode',
                      'CATEGORY':'measure_category', 'MEASURE':'measure_name'}
             ).astype({'date':'datetime64[ns]'})[['country_alphacode', 'date', 'measure_name', 'measure_category']]

# Measures Category
def measures_category():
    return pd.DataFrame({'measure_category': ['None',
                                       'Humanitarian exemption',
                                       'Governance and socio-economic measures',
                                       'Public health measures',
                                       'Social distancing',
                                       'Movement restrictions',
                                       'Lockdown'],
                  'measure_level': range(0, 7)})

# Measures
def get_measures():
    df = read_acaps().merge(measures_category(), how='left')
    df['date'] = pd.to_datetime(df['date']).apply(lambda x: x.date())
    return df

# Test Positivity Rate
def get_test_data():
    df = pd.read_csv('https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv'
                       ).rename(columns={'iso_code':'country_alphacode'}
                                ).astype({'date':'datetime64[ns]', 'positive_rate':'float64'}
                                         )[['country_alphacode', 'date', 'positive_rate']]
    df['date'] = pd.to_datetime(df['date']).apply(lambda x: x.date())
    return df

# Covid data
def get_covid19_data(level=9):
    # === level 1:
    # Read CSSE data
    df = get_cssedata(updatenames=True).\
        merge(get_Isocodes(), how='left').\
        merge(get_countrycode().drop(columns=['country_name']), how='left').\
        merge(get_population().drop(columns=['country_name']), how='left', on='country_isocode').\
        assign(N=lambda x: x['PopTotal'] * 1000).drop(columns=['PopTotal'])
    # Add Kosovo data
    df.loc[df['country_name'] == 'Kosovo', 'continent_name'] = 'Europe'
    df.loc[df['country_name'] == 'Kosovo', 'region_name'] = 'Southern Europe'
    df.loc[df['country_name'] == 'Kosovo', 'N'] = 1811285
    # Taiwan
    df.loc[df['country_name'] == 'Taiwan', 'continent_name'] = 'Asia'
    df.loc[df['country_name'] == 'Taiwan', 'region_name'] = 'Eastern Asia'
    # Add Region Western Sahara
    df.loc[df['country_name'] == 'Western Sahara', 'region_name'] = 'Sub-Saharan Africa'

    if level < 2: return df

    # === level 2:
    # Add S (susceptible)
    df['S'] = df['N'] - df['I_cum'] - df['R_cum']
    # Add IR (Incidence Rate)
    df['IR'] = (df['I_cum'] / df['N']) * 10 ** 5

    #### Add CFR (Confirmed Fatality rate) and M (Mortality)
    df['CFR'] = (df['D_cum'] / df['I_cum'] * 100).fillna(0)
    df['M'] = (df['D_cum'] / df['N']) * 10 ** 5

    if level < 3: return df

    # === level 3:
    # Add Measures
    df = df.merge(get_measures().drop(columns=['measure_category']).
                       groupby(['country_alphacode', 'date'], as_index=False).agg({'measure_level': 'max'}),
                       how='left', on=['country_alphacode', 'date'])
    df = df.assign(ml_ant=lambda x: x.groupby(['country_alphacode'])['measure_level'].shift(1).fillna(0))
    df = df.assign(measure_level=df[['measure_level', 'ml_ant']].max(axis=1)).drop(columns=['ml_ant'])

    if level < 4: return df

    # === level 4:
    # Add Test
    df = df.merge(get_test_data(), how='left').assign(TPR = lambda x: x['positive_rate']*100)\
        .drop(columns=['positive_rate'])

    return df

def make_covid_map(data, locations='country_alphacode',
                   color='I_cum', color_scale='YlOrRd',
                   name='country_name', var_animation=None,
                   facet_col=None, facet_col_wrap=None,
                   title='Infected by Countries', lcolor='Infected',
                   save=True, outfile='covid19map'):
    import plotly.express as px

    if var_animation == 'date':
        data = data.assign(date=data.date.apply(lambda x: x.strftime('%b %d, %Y')))
    elif var_animation == 'week':
        data = data.assign(week=data.date.apply(lambda x: x.strftime('%Y-%WW')))

    if facet_col == 'year':
        data = data.assign(year=data.date.apply(lambda x: x.strftime('%Y')))
    elif facet_col == 'month':
        data = data.assign(month=data.date.apply(lambda x: x.strftime('%b')))
        if facet_col_wrap == None: facet_col_wrap = 4

    fig = px.choropleth(data, locations=locations, hover_name=name,
                        color=color, color_continuous_scale=color_scale,
                        title=title, labels={color: lcolor},
                        animation_frame=var_animation, animation_group=name,
                        facet_col=facet_col, facet_col_wrap=facet_col_wrap)

    if save: fig.write_html(outfile+'.html')
    return fig

def make_covid_map2(data, column='I', legend_name='',
                    fill_color='YlOrRd',
                    save=True, outfile='covid19map'):
    import folium

    url_geo = 'https://raw.githubusercontent.com/python-visualization/folium/master/examples/data/world-countries.json'
    m = folium.Map(min_zoom=2, max_bounds=True, tiles='cartodbpositron')
    folium.Choropleth(geo_data=url_geo, key_on='feature.id',
                      data=data, columns=['country_alphacode',column], name='choropleth',
                      fill_color=fill_color, fill_opacity=0.7, line_opacity=0.2,
                      legend_name=legend_name).add_to(m)
    if save: m.save(outfile=outfile+'.html')
    return m

def make_covid_bar(data, x='country_alphacode', y='I_cum', name='country_name',
                   color='continent_name', var_animation=None,
                   facet_col=None, facet_col_wrap=None,
                   title='Infected by Countries',
                   xtitle='Countries', ytitle='Infected Accumulated', ltitle='Continents',
                   save=True, outfile='covid19bars'):
    import plotly.express as px

    if var_animation == 'date':
        data = data.assign(date=data.date.apply(lambda x: x.strftime('%b %d, %Y')))
    elif var_animation == 'week':
        data = data.assign(week=data.date.apply(lambda x: x.strftime('%Y-%WW')))

    if facet_col == 'year':
        data = data.assign(year=data.date.apply(lambda x: x.strftime('%Y')))
    elif facet_col == 'month':
        data = data.assign(month=data.date.apply(lambda x: x.strftime('%b')))
        if facet_col_wrap == None: facet_col_wrap = 4

    fig = px.bar(data, x=x, y=y, range_y=[0, data[y].max()],
                 color=color, hover_name=name,
                 animation_frame=var_animation, animation_group=name,
                 facet_col=facet_col, facet_col_wrap=facet_col_wrap)
    fig.update_layout(title=title, legend_title=ltitle)
    fig.update_xaxes(title=xtitle, categoryorder='total descending', nticks=len(data[x].unique()),
                     tickfont=dict(size=8, color='black'),
                     rangeslider_visible=True)
    fig.update_yaxes(title=ytitle)
    if save: fig.write_html(outfile+'.html')
    return fig
