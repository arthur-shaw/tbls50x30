# Create temporary crop harvest table

Create temporary crop harvest table

## Usage

``` r
temp_crop_harvest(
  parcel_plot_crop_df,
  cases,
  categories_df,
  crop_id_var,
  crop_vals,
  harvest_var,
  harvest_val = 1,
  why_not_harvest_var,
  group_var,
  json_qnr_path
)
```

## Arguments

- parcel_plot_crop_df:

  Data frame of parcel-plot-crop-level observations.

- cases:

  Data frame of cases to include in analysis. Data frame must contain
  `interview__id` and the grouping variable indicated in the `group_var`
  parameter

- categories_df:

  Data frame of reusable categories exported by susometa.

- crop_id_var:

  Atomic character vector. Name of crop ID variable as a character.

- crop_vals:

  Numeric vector. Codes of the crops that are temporary crops.

- harvest_var:

  Atomic character vector. Name of variable indicating whether the crop
  was harvested.

- harvest_val:

  Atomic numeric vector. Value of `harvest_var` that indicates the crop
  was harvested.

- why_not_harvest_var:

  Atomic character vector. Name of variable, as it appears in Designer,
  that captures the reason(s) the crop was not harvested.

- group_var:

  Atomic character vector. Name of the grouping variable

- json_qnr_path:

  Character. Path to JSON file that describes the questionnaire.

## Value

gt table object
