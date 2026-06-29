# Make crops per plot table

Make crops per plot table

## Usage

``` r
crops_per_plot(crop_df, cases, parcel_id_var, plot_id_var, group_var)
```

## Arguments

- crop_df:

  Data frame of parcel-plot-crops

- cases:

  Data frame of cases to include in analysis.

- parcel_id_var:

  Character. Name of parcel ID column.

- plot_id_var:

  Character. Name of plot ID column.

- group_var:

  Character. Name of grouping variable column (e.g., team, region,
  etc.).

## Value

gt table object
