import covid19_functions as cf

covid19 = cf.get_covid19_data()

## Visualization

### Current situation

# Show the current situation on the world map
print(covid19[covid19['date'] == covid19['date'].max()]
      [['country_name','date','I_cum','D_cum','N','IR','CFR','measure_level']].head())

# World map of Infected by Covid-19
cm = cf.make_covid_map(covid19.query('date == @covid19.date.max()'), color='I_cum', color_scale='YlOrRd',
                  title='Infected by Countries', lcolor='Infected',
                  outfile='covid19map')

# World map of Deaths by Covid-19
cm = cf.make_covid_map(covid19.query('date == @covid19.date.max()'), color='D_cum', color_scale='Reds',
                  title='Deaths by Countries', lcolor='Deaths',
                  outfile='covid19map')

# Progress of the disease
cm = cf.make_covid_map(covid19, color='I_cum', color_scale='YlOrRd',
                  title='Progress of the disease: World map of Infected by Covid-19', lcolor='Infected',
                  outfile='covid19map', var_animation='date')

cm = cf.make_covid_map(covid19, color='D_cum', color_scale='Reds',
                  title='Progress of the disease: World map of Deaths by Covid-19', lcolor='Deaths',
                  outfile='covid19map', var_animation='date')

# daily Infected
cm = cf.make_covid_map(covid19, color='I', color_scale='YlOrRd',
                  title='Progress of the disease: World map of Daily Infected by Covid-19', lcolor='Infected',
                  outfile='covid19map', var_animation='date')

# daily Deaths
cm = cf.make_covid_map(covid19, color='D', color_scale='Reds',
                  title='Progress of the disease: World map of Daily Deaths by Covid-19', lcolor='Deaths',
                  outfile='covid19map', var_animation='date')

# Progress of infections and deaths from the pandemic.
df = covid19.groupby(['date'], as_index=False).\
    agg({'I':'sum','D':'sum','I_cum':'sum','D_cum':'sum','N':'sum'}
        ).assign(IR=lambda x: (x['I_cum'] / x['N']) * 10 ** 5,
                 CFR=lambda x: (x['D_cum'] / x['I_cum'] * 100).fillna(0))

from plotly.subplots import make_subplots

make_subplots(rows=2, cols=2, subplot_titles=('Daily Infected','Daily Deaths','IR','CFR')). \
    add_trace(go.Scatter(x=df['date'], y=df['I'],  # subplot I
                         mode='lines', line=dict(width=0.5, color='yellow'),
                         stackgroup='one'), row=1, col=1). \
    add_trace(go.Scatter(x=df['date'], y=df['D'],  # subplot D
                         mode='lines', line=dict(width=0.5, color='red'),
                         stackgroup='one'), row=1, col=2). \
    add_trace(go.Scatter(x=df['date'], y=df['IR'], # subplot IR
                         mode='lines', line=dict(width=0.5, color='yellow'),
                         stackgroup='one'), row=2, col=1).\
    add_trace(go.Scatter(x=df['date'], y=df['CFR'], # subplot CFR
                         mode='lines', line=dict(width=0.5, color='red'),
                         stackgroup='one'),row=2, col=2).\
    update_layout(showlegend=False,
                  title_text='Progress of infections and deaths from the pandemic').\
    write_html('covid19area.html')

### The pandemic by continent

# daily Infected

# Acumulated Infected

# daily Deaths

# Acumulated Deaths
