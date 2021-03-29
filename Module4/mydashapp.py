import pandas as pd
import numpy as np

import plotly.express as px
import dash
from dash.dependencies import Input, Output
import dash_core_components as dcc
import dash_html_components as html

url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json'
trees = pd.read_json(url)
pd.set_option('display.max_columns', None)

#print(trees.head())


soql_url = (
        'https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,count(tree_id)' +\
        '&$group=spc_common'
    ).replace(" ", "%20")

spc_list = pd.read_json(soql_url)['spc_common'].dropna().to_list()
#print(spc_list)

boro_url = ('https://data.cityofnewyork.us/resource/uvpi-gqnh.json?' +\
        '$select=spc_common,boroname,steward,health,count(tree_id)' +\
        '&$group=spc_common,boroname,steward,health' ).replace(" ", "%20")

boro_list = pd.read_json(boro_url)['boroname'].unique().tolist()

#print(boro_list)

#-----------------------------------------------------------------------------------
## APP
external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

server = app.server


app.layout = html.Div(children=[
    html.H4(className='page-title', children='DATA 608 - Module 4', style={'color': 'steelblue'}),
    html.H5(className='date', children='03/28/2021', style={'color': 'steelblue'}),
    html.H5(className='author', children='Subhalaxmi Rout', style={'color': 'steelblue'}),
    html.H5(className='page-subtitle', children='New York Tree Health',
            style={'color': 'steelblue', 'fontSize': 36, 'textAlign': 'center'}),
    html.H6(className='question-text',
            children='Que 1: What proportion of trees are in good, fair, or poor health according to the ‘health’ variable?',
            style={'color':'darkgreen'}),

    html.Div([
        dcc.Dropdown(
            id='species-selection1',
            options=[{'label': i, 'value': i} for i in spc_list],
            value='American beech',
            className='dropdown'
        ),
    ]),

    html.Div(
        dcc.Graph(id='health-graphic', className='graph1')
    ),

    html.H6(className='question-text',
            children='Que 2: Are stewards (steward activity measured by the ‘steward’ variable) having an impact on the health of trees?',
            style={'color':'darkgreen'}),

    html.Div([
        html.Div([
            dcc.Dropdown(
                id='boro-selection',
                options=[{'label': j, 'value': j} for j in boro_list],
                value='Bronx',
                className='dropdown2',
                placeholder='Please select a Borough'
            ),
    html.Br(),
        ]),

        html.Div([
            dcc.Dropdown(
                id='species-selection2',
                options=[{'label': i, 'value': i} for i in spc_list],
                value='American beech',
                className='dropdown2',
                placeholder='Please select a species'
            ),
        ])
    ], className='dropdown-wrapper'),


    html.Div(
        dcc.Graph(id='steward-graph', className='graph2')
    )
])

# App Connection from input to output---------------------------------------------------------------------------------------

@app.callback(
    Output(component_id='health-graphic', component_property='figure'),
    [Input(component_id='species-selection1', component_property='value')]
)

def update_graph(species):
    final_df = pd.DataFrame()
    for i in boro_list:
        soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
                '$select=spc_common,health' +\
                '&$where=boroname=\'' + i + '\'' + '&spc_common=\'' + species +\
                '\'&$limit=100000').replace(' ', '%20')

        soql_trees = pd.read_json(soql_url)
        soql_trees = soql_trees.groupby('health').count()['spc_common'].reset_index()
        soql_trees['Borough'] = i
        final_df = final_df.append(soql_trees)

    fig = px.bar(final_df, x='Borough', y='spc_common', color='health', barmode="group",
                 labels=dict(Borough="Borough", spc_common="Number of Trees"))

    return fig

@app.callback(
    Output(component_id='steward-graph', component_property='figure'),
    [Input(component_id='boro-selection', component_property='value'),
     Input(component_id='species-selection2', component_property='value')]
)

def get_steward_graph_data(borough, species):
    soql_url2 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
            '$select=steward,health,count(tree_id)' +\
            '&$where=boroname=\'' + borough + '\'' + '&spc_common=\'' + species +\
            '\'&$group=steward,health').replace(' ', '%20')

    df = pd.read_json(soql_url2).dropna().rename(columns={"count_tree_id": "trees_count"})
    df["type"] = df['steward'].map(lambda x : "no steward" if x == 'None' else "steward")


    df = df.groupby(["type", "health"])["trees_count"].sum().reset_index()
    df2 = df.groupby("type")["trees_count"].sum().reset_index().rename(columns={"trees_count": "total"})
    df = pd.merge(df, df2)
    df["share"] = df.trees_count / df.total * 100

    fig = px.bar(df, x='type', y='share', color='health', barmode="group",
                 labels=dict(type="Impact of stewards on trees health", share="Proportion of trees (%)"))
    return fig

if __name__ == '__main__':
    app.run_server(debug=True)








