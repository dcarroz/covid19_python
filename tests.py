import pandas as pd
import numpy as np
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
import matplotlib.pyplot as plt
import webbrowser
import importlib

import covid19_functions as cf
covid19 = cf.get_covid19_data()

# Current Situation
# Maps
cm = cf.make_covid_map(covid19.query('date == @covid19.date.max()'), color='I_cum', color_scale='YlOrRd',
                  title='Infected by Countries', lcolor='Infected',
                  outfile='covid19map')

cm = cf.make_covid_map(covid19.query('date == @covid19.date.max()'), color='D_cum', color_scale='Reds',
                  title='Deaths by Countries', lcolor='Deaths',
                  outfile='covid19map')

# Bars
cb = cf.make_covid_bar(covid19.query('date == @covid19.date.max()'),
                  x='country_alphacode', y='I_cum', color='continent_name',
                  title='Infected by Countries', xtitle='Countries', ytitle='Infected Accumulated', ltitle='Continents',
                  outfile='covid19bars')

cb = cf.make_covid_bar(covid19.query('date == @covid19.date.max()'),
                  x='country_alphacode', y='D_cum', color='continent_name',
                  title='Deaths by Countries', xtitle='Countries', ytitle='Deaths Accumulated', ltitle='Continents',
                  outfile='covid19bars')

# With Time Animation
# Maps in Dates
cm = cf.make_covid_map(covid19, color='I_cum', color_scale='YlOrRd',
                  title='Infected by Countries', lcolor='Infected',
                  outfile='covid19map', var_animation='date')

cm = cf.make_covid_map(covid19, color='D_cum', color_scale='Reds',
                  title='Deaths by Countries', lcolor='Deaths',
                  outfile='covid19map', var_animation='date')

# Bars in Dates
cb = cf.make_covid_bar(covid19, x='country_alphacode', y='I_cum', color='continent_name',
                  title='Infected by Countries', ytitle='Infected Accumulated',
                  outfile='covid19bars', var_animation='date')

cb = cf.make_covid_bar(covid19, x='country_alphacode', y='D_cum', color='continent_name',
                  title='Deaths by Countries', ytitle='Deaths Accumulated',
                  outfile='covid19bars', var_animation='date')

# Maps in Weeks
cm = cf.make_covid_map(covid19, color='I_cum', color_scale='YlOrRd',
                  title='Infected by Countries', lcolor='Infected',
                  outfile='covid19map', var_animation='week')

cm = cf.make_covid_map(covid19, color='D_cum', color_scale='Reds',
                  title='Deaths by Countries', lcolor='Deaths',
                  outfile='covid19map', var_animation='week')

# Bars in Weeks
cb = cf.make_covid_bar(covid19, x='country_alphacode', y='I_cum', color='continent_name',
                  title='Infected by Countries', ytitle='Infected Accumulated',
                  outfile='covid19bars', var_animation='week')

cb = cf.make_covid_bar(covid19, x='country_alphacode', y='D_cum', color='continent_name',
                  title='Deaths by Countries', ytitle='Deaths Accumulated',
                  outfile='covid19bars', var_animation='week')

# Facet
cm = cf.make_covid_map(covid19, color='I_cum', color_scale='YlOrRd',
                  title='Infected by Countries', lcolor='Infected',
                  outfile='covid19map', facet_col='month')

cm = cf.make_covid_map(covid19, color='D_cum', color_scale='Reds',
                  title='Deaths by Countries', lcolor='Deaths',
                  outfile='covid19map', facet_col='month')

# Area Plot group by date
# India
df = covid19[covid19['country_alphacode'].isin(['IND','USA','CHL'])]
print(df.head())
px.scatter(df, x='date', y='IR', facet_col='country_name', color_discrete_sequence=['yellow']).\
    write_html('covid19area.html')

# World
df = covid19.groupby(['date'], as_index=False).\
    agg({'I':'sum','D':'sum','I_cum':'sum','D_cum':'sum','N':'sum'}
        ).assign(IR=lambda x: (x['I_cum'] / x['N']) * 10 ** 5,
                 CFR=lambda x: (x['D_cum'] / x['I_cum'] * 100).fillna(0))

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

# Using matplotlib
plt.plot([i for i in range(10)])

# Plotly.graph_objects
df = covid19.query('date == @covid19.date.max()')
df.loc[(df['D'] < 0), 'D'] = 0

go.Figure(data=go.Scattergeo(lon=df['longitude'], lat=df['latitude'], text=df['country_name'], mode='markers',
                             marker=dict(size=df['D']/100, opacity=0.8, symbol='circle',
                                         line=dict(width=1, color='black'),
                                         colorscale='Reds', color=df['D'],
                                         colorbar_title='Daily Deaths'))).write_html('covid19map.html')

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

# Bars in Dates by continents
cb = cf.make_covid_bar(covid19, x='country_alphacode', y='I_cum', color='continent_name',
                       title='Infected by Countries', ytitle='Infected Accumulated',
                       outfile='covid19bars', var_animation='date',facet_col='month')

cm = cf.make_covid_map(covid19, color='I_cum', color_scale='YlOrRd',
                  title='Infected by Countries', lcolor='Infected',
                  outfile='covid19map', facet_col='continent_name')

importlib.reload(cf)
webbrowser.open_new_tab('covid19map.html')
webbrowser.open_new_tab('covid19bars.html')
webbrowser.open_new_tab('covid19area.html')
