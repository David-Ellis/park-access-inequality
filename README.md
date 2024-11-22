# Park Deprivation

In this project we calculate an average IMD deprivation score for each park in Birmingham based on postcodes within a 10 minute walk to each park.

## Method 
### Deprivation
- Straight-line distance to each Birmingham postcode calculated for each postcode
- Postcodes within a 10 minute walking distance identified (0.83km assuming a walking speed of 5km per hour)
- The number of residents within a 10-minute radius estimated using LSOA-level 2021 census data. For LSOAs on the boundary of the 10-minute radius, the percentage of postcodes within 10 minutes is used to estimate the percentage of residents in the LSOA inside the radius.
- Finally, a weighted deprivation score is calculated using the 2019 Index of Multiple Deprivation. This is done on both a National and Birmingham-specific IMD scale.

### Ethnicity
- Coming soon

## License 

This repository is dual licensed under the [Open Government v3]([https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) & MIT. All code can outputs are subject to Crown Copyright.
