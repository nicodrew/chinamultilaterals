import os
import pandas as pd 

script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

df = pd.read_excel("AidDatasGlobalChineseDevelopmentFinanceDataset_v3.0.xlsx", sheet_name="GCDF_3.0")
#Receiving country name is in df['Recipient']
df = df[df['Intent'] == "Development"].copy()
keep_sectors = [140, 210, 220, 230, 240, 250, 310, 320, 330]
df = df[df['Sector Code'].isin(keep_sectors)].copy()
df = df[df['Amount (Constant USD 2021)'].notna()].copy()
df = df[df['Actual Implementation Start Date (MM/DD/YYYY)'].notna()].copy()
df = df.rename(columns={'Actual Implementation Start Date (MM/DD/YYYY)': 'Start Date',
                         'Amount (Constant USD 2021)': 'Disbursed'})
df['Start Date'] = pd.to_datetime(df['Start Date'], errors='coerce')
df['Year'] = df['Start Date'].dt.year
df["Disbursed (millions)"] = df["Disbursed"] / 1e6
#print(df.groupby('Year')['Disbursed (billions)'].sum())
df.to_csv('cleaned AidData.csv', index=False)

country_year_matrix = df.pivot_table(
    index='Recipient',
    columns='Year', 
    values='Disbursed (millions)',
    aggfunc='sum',
    fill_value=0
)
country_year_matrix.to_csv('china_aid_flows_country_year.csv')
# VALUES IN MILLIONS 2021 USD