# Make plot GPS measurement table

Make plot GPS measurement table

## Usage

``` r
plot_gps(
  plot_df,
  cases,
  gps_var,
  not_measured_val = -9999,
  why_no_gps_var,
  group_var
)
```

## Arguments

- plot_df:

  Data frame of plots.

- cases:

  Data frame of cases to include in analysis.

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
