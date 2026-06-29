# Create hen displacement table

Create hen displacement table

## Usage

``` r
hen_displacement(
  hhold_df,
  cases,
  animal_var,
  cock_val = 51,
  hen_val = 52,
  pullet_val = 53,
  group_var
)
```

## Arguments

- hhold_df:

  Data frame. All household-level variables.

- cases:

  Data frame. Case to include in analysis

- animal_var:

  Atomic character vector. Name of multi-select livestock ownership
  variable, as it appears in Designer.

- cock_val:

  Atomic numeric vector. Value of cock/broiler answer option.

- hen_val:

  Atomic numeric vector. Value of hen answer option.

- pullet_val:

  Atomic numeric vector. Value of pullet answer option.

- group_var:

  Atomic character vector. Name of the grouping variable

## Value

gt table object
