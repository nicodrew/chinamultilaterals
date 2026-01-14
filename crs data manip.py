import os
import pandas as pd


script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

df = pd.read_parquet("CRS.parquet")
df = df[[
    'year',
    'donor_code',           # was 'donorcode'
    'donor_name',           # was 'donorname'
    'agency_code',          # was 'agencycode'
    'recipient_code',       # was 'recipientcode'
    'recipient_name',       # was 'recipientname'
    'region_name',          # was 'regionname'
    'incomegroup_name',     # was 'incomegroupname'
    'bi_multi',
    'category',
    'finance_t',
    'usd_commitment',
    'usd_disbursement',
    'usd_disbursement_defl',
    'grant_equiv',
    'purpose_code',         # was 'purposecode'
    'purpose_name',         # was 'purposename'
    'sector_code',          # was 'sectorcode'
    'sector_name',          # was 'sectorname'
    'channel_code'          # was 'channelcode'
]].copy()
print(df.columns.to_list())

print(df['sector_name'].unique())
exclude_sectors = [
    'I. Social Infrastructure & Services',  # All social sectors
    'VI. Commodity Aid / General Programme Assistance',
    'VII. Action Relating to Debt',
    'VIII. Humanitarian Aid', 'VIII.1. Emergency Response', 
    'VIII.2. Reconstruction Relief & Rehabilitation',
    'VIII.3. Disaster Prevention & Preparedness',
    'IX. Unallocated / Unspecified',
    'Sectors not specified',
    'Administrative Costs of Donors',
    'Refugees in Donor Countries'
]

df = df[~df['sector_name'].isin(exclude_sectors)].copy()
#df.to_csv('extra_cleaned CRS parquet.csv', index=False)

#keepRegions = ['South of Sahara', 'North of Sahara']
keep_donors = [
    'International Development Association',
    'International Bank for Reconstruction and Development',
    'Inter-American Development Bank', 
    'African Development Bank',
    'Asian Development Bank',
    'African Development Fund',
    'European Bank for Reconstruction and Development',
    'International Finance Corporation',
    'Council of Europe Development Bank',
    'Green Climate Fund',
    'IFAD'
]
#df = df[df['region_name'].isin(keepRegions)].copy()
df = df[df['donor_name'].isin(keep_donors)].copy()
# Keep only debt-creating flows
keep_financet = [421, 423, 425, 431]  # All loan/bond/equity instruments
df = df[df['category'] == 21].copy()
df = df[df['finance_t'].isin(keep_financet)].copy()
keep_sectors = [140, 210]
keep_sectors.extend(range(210, 339))
#print(keep_sectors)
df = df[df['sector_code'].isin(keep_sectors)].copy()
# Keep only rows that have a disbursement value
df = df[df['usd_disbursement_defl'].notna()].copy()

df.to_csv('final_cleaned CRS parquet.csv', index=False)
print(df.groupby('year')['usd_disbursement_defl'].sum())

crs_country_year_matrix = df.pivot_table(
    index='recipient_name',  # Country name column
    columns='year', 
    values='usd_disbursement_defl',
    aggfunc='sum',
    fill_value=0
)
crs_country_year_matrix.to_csv('multilateral_aid_flows_country_year.csv')
# VALUES IN MILLIONS 2023 USD
