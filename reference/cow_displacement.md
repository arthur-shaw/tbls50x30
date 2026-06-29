# Create cow displacement table

Create cow displacement table

## Usage

``` r
cow_displacement(
  hhold_df,
  cases,
  animal_var,
  bull_val = 10,
  cow_val = 12,
  steer_heifer_val = 13,
  calf_val = 14,
  group_var
)
```

## Arguments

- hhold_df:

  Data frame. All household-level variables.

- cases:

  Data frame. Case to include in analysis

- animal_var:

  Atomic character vector. Name of the mulit-select livestock ownership
  variable, as it appears in Designer.

- bull_val:

  Atomic numeric vector. Value of bull answer option.

- cow_val:

  Atomic numeric vector. Value of cow answer option.

- steer_heifer_val:

  Atomic numeric vector. Value of steer/heifer answer option.

- calf_val:

  Atomic numeric vector. Value of calf answer option.

- group_var:

  Atomic character vector. Name of the grouping variable

## Value

gt table object
