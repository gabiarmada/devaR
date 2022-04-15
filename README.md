# devaR
R package - DEVA


This package contains convenient functions used to process + clean EPA NEI Original data for analyses and spatial modeling.

Install using `devtools`:
```
devtools::install_github("gabiarmada/devaR")

```

## get EPA NEI original data 
Go to the [EPA NEI databases](https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei) and use the following criteria: 

* State/county &rarr; Virginia &rarr; SO2, and leave all other options as default.
* Download and save as a .csv file.
* Repeat the last 2 steps for CO, NOX, PM2.5, and VOC.
