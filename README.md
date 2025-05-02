# The plastic drift

...

# Repository structure

```sh
├── data-cleaning # preparation of datasets
├── explorative-data-analysis
├── plastic-drift-app # shiny app itself
│   ├── datasources # cleaned datasets used for shiny app
│   │   ├── binned_currents.parquet
│   │   ├── cleaned_microplastics.parquet
|   |   ├── currents_by_buoy_time.parquet
│   │   └── currents_with_microplastics.parquet
│   ├── helpers-visualize-attributes.R
│   ├── server.R
│   └── ui.R
└── README.md
```

# Run shiny app

In the R shell do:

```r
setwd("plastic-drift-app")
shiny::runApp()
```
