# Make parcel GPS measurement table

Make parcel GPS measurement table

## Usage

``` r
parcel_gps(
  parcel_df,
  cases,
  gps_var,
  not_measured_val = -9999,
  why_no_gps_var = NULL,
  group_var
)
```

## Arguments

- parcel_df:

  Data frame of parcels.

- cases:

  Data frame of cases to include in analysis

- gps_var:

  Character. Name of column that includes GPS-measured area.

- not_measured_val:

  Numeric. Special value that indicates area was not measured.

- why_no_gps_var:

  Character. Name of column that captures reason why no GPS measurement
  was done.

- group_var:

  Character. Name of grouping variable column (e.g., team, region,
  etc.).

## Value

gt table object
