# Make crop types tables

Make crop types tables

## Usage

``` r
crop_types(
  crop_df,
  cases,
  crop_type_var,
  temp_crop_val = 1,
  perm_crop_val = 2,
  group_var
)
```

## Arguments

- crop_df:

  Data frame of parcel-plot-crops

- cases:

  Data frame of cases to include in analysis.

- crop_type_var:

  Charcter. Name of crop type column.

- temp_crop_val:

  Numeric. Value of temporary crop option.

- perm_crop_val:

  Numeric. Value of permanent crop option.

- group_var:

  Character. Name of grouping variable column (e.g., team, region,
  etc.).

## Value

gt table object
