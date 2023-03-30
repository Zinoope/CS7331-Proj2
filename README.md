# CS7331-Proj2
Project 02 for Data Mining calss to apply Covid plus Census dataset analysis in Ohio State

# Retreived Data
The data was retreived on 3/22/2023 from Google cloud website, using the following 3 diffrent datasets:

1. USAFacts US Coronavirus Database:
   Publisher: USAFacts
You can review it using the following link:
https://console.cloud.google.com/marketplace/details/usafacts-public-data/covid19-us-cases?filter=solution-type:dataset&filter=category:covid19&id=3eaff9c5-fbaf-47bb-a441-89db1e1395ab

2. US Census Data  
   Publisher: United States Census Bureau  
You can review it using the following link:  
https://console.cloud.google.com/marketplace/details/united-states-census-bureau/us-census-data

3. Census Bureau US Boundaries:  
   Publisher: United States Census Bureau  
You can review it using the following link:
https://console.cloud.google.com/marketplace/details/united-states-census-bureau/us-geographic-boundaries

The dataset was pulled dataset (located in the data folder) correspands to 3/5/2023 updates of covid-19 data combined with USA Census dataset (2020 [5 years average]) and US boundries attributes.

To make sure that the pulled results corresponds only to Ohio counties, a state condition was added to the query.

The final query:
```
SELECT *
FROM `bigquery-public-data.covid19_usafacts.summary` covid19
JOIN `bigquery-public-data.census_bureau_acs.county_2020_5yr` acs
ON covid19.county_fips_code = acs.geo_id
JOIN bigquery-public-data.geo_us_boundaries.counties geo
ON geo.county_fips_code=acs.geo_id
WHERE date = DATE_SUB(CURRENT_DATE(), INTERVAL 17 day) and state="OH"
```
Note: The interval of days can be increased/decreased to find previous/newer dates records if desired.
