# Create permanent crop harvest table

Create permanent crop harvest table

## Usage

``` r
perm_crop_harvest(
  parcel_plot_crop_df,
  cases,
  crop_id_var,
  crop_vals,
  produce_var,
  harvest_var,
  harvest_val = 1,
  group_var
)
```

## Arguments

- parcel_plot_crop_df:

  Data frame of parcel-plot-crop-level observations.

- cases:

  Data frame of cases to include in analysis. Data frame must contain
  `interview__id` and the grouping variable indicated in the `group_var`
  parameter

- crop_id_var:

  Atomic character vector. Name of crop ID variable as a character.

- crop_vals:

  Numeric vector. Codes of the crops that are temporary crops.

- produce_var:

  Atomic character vector. Name of variable indicating whether the crop
  was produced.

- harvest_var:

  Atomic character vector. Name of variable indicating whether the crop
  was harvested.

- harvest_val:

  Atomic numeric vector. Value of `harvest_var` that indicates the crop
  was harvested.

- group_var:

  Atomic character vector. Name of the grouping variable

## Value

gt table object
