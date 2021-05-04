# DTSC 2302 - Project 2
## Topic: Immigration
Group Members: Rishi Challa, Khem Khadka, Joshua Hernandez, Aryan Anerao, Beau Tate

Main R script file for analysis is located in `Project 2.r`

## Data
All data used for actual analysis is in the `CleanedData/project2CompleteDataset.csv`. Everything else was just used in cleaning. Correlates codebook is located within the `Data/` folder. We also are using stepwise regression in order to choose certain variables to keep and to remove.

## Stepwise Regression Results

### All Variables

| Step | Variable          | R-Square | Adj. R-Square | C(p)      | AIC       | RMSE   |
|------|-------------------|----------|---------------|-----------|-----------|--------|
| 1    | `evangelical_pop` | 0.191    | 0.189         | 1263.7780 | 1988.7664 | 0.9988 |
| 2    | `region`          | 0.233    | 0.231         | 1162.2700 | 1952.7828 | 0.9727 |
| 3    | `hs_rep_in_sess`  | 0.265    | 0.261         | 866.6940  | 1599.5557 | 0.9381 |
| 4    | `rep_unified`     | 0.291    | 0.287         | 816.5810  | 1579.9839 | 0.9219 |
| 5    | `valueofagsect`   | 0.300    | 0.294         | 801.3050  | 1574.6542 | 0.9169 |
| 6    | `dem_unified`     | 0.305    | 0.298         | 793.4080  | 1572.4138 | 0.9144 |
| 7    | `mood`            | 0.294    | 0.284         | 686.5870  | 1425.5100 | 0.8996 |
| 8    | `nonwhite`        | 0.299    | 0.289         | 679.1940  | 1424.3081 | 0.8970 |

### Finance Variables

| Step | Variable             | R-Square | Adj. R-Square | C(p)      | AIC       | RMSE   |
|------|----------------------|----------|---------------|-----------|-----------|--------|
| 1    | `pc_inc_ann`         | 0.041    | 0.039         | 44.3670   | 1817.2938 | 1.0776 |
| 2    | `povrate`            | 0.074    | 0.071         | 24.0210   | 1797.9712 | 1.0597 |
| 3    | `budget_surplus_gsp` | 0.087    | 0.082         | 17.3330   | 1791.4748 | 1.0532 |

## Variables
> Note: All variables and analysis was conducted on a state level

All variables listed below can be found in the file `CleanedData/project2CompleteDataset.csv`.

| Dependent/Independent | Variable Name | CSV/Data Frame Variable Name | Included By Stepwise Regression? |
|-----------------------|---------------|------------------------------|----------------------------------|
| Independent | State | `State` | &times; |
| Independent | Year | `Year` | &times; |
| Independent | Mood Index | `mood` | &check; |
| Independent | Right to Work | `right2work` | &times; |
| Independent | Per Capital Annual Income | `pc_inc_ann` | &check; |
| Independent | State Poverty Rate | `povrate` | &check; |
| Independent | Consumer Price Index | `ccpi` | &times; |
| Independent | Evangelical Population | `evangelical_pop` | &check; |
| Independent | Non-White Population | `nonwhite` | &check; |
| Independent | Value added to Economy by Agriculture Sector | `valueofagsect` | &check; |
| Independent | U.S Region | `region` | &check; |
| Independent | Budget surplus as % of GSP | `budget_surplus_gsp` | &check; |
| Independent | Is there a unified Democratic Government? | `dem_unified` | &check; |
| Independent | Is there a unified Republican Government? | `rep_unified` | &check; |
| Independent | Percentage of Latino Legislators | `pctlatinoleg` | &times; |
| Independent | Number of Democratic Representatives | `hs_dem_in_sess` | &times; |
| Independent | Number of Republican Representatives | `hs_rep_in_sess` | &check; |
| Independent | Number of Nonmajor-Party Representatives | `hs_ind_in_sess` | &times; |
| Independent | Number of Democratic Senators | `sen_dem_in_sess` | &times; |
| Independent | Number of Republican Senators | `sen_rep_in_sess` | &times; |
| Independent | Number of Nonmajor-Party Senators | `sen_ind_in_sess` | &times; |
| Dependent | Task Force Agreement | `Task.Force.Agreement` | N/A |
| Dependent | Cooperation WIth Detainers | `Cooperation.With.Detainers` | N/A |
| Dependent | EVerify Mandates | `EVerify.Mandates` | N/A |
| Dependent | Year English Was Declared as Official Lang | `Year.English.Declared` | N/A |
| Dependent | Is English Declared as the Official Lang? | `English.Declared` | N/A |
| Dependent | Insurance for Unauthorized Children | `Insurance.for.Unauth.Kids` | N/A |
| Dependent | Insurance for Unauthorized Adults | `Insurance.for.Unauth.Adults` | N/A |
| Dependent | Food Assistance for Unauthorized Children | `Food.Assist.for.Immigrant.Kids` | N/A |
| Dependent | Food Assistance for Unauthorized Adults | `Food.Assist.for.Immigrant.Adults` | N/A |
| Dependent | Immigration Composite Score | `Immigration.Composite.Score` | N/A |
