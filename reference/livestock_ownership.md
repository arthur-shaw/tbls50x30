# Create livestock ownership table

Create livestock ownership table

## Usage

``` r
livestock_ownership(hhold_df, animal_var, cases, group_var)
```

## Arguments

- hhold_df:

  Data frame.

- animal_var:

  Atomic character vector. Name of multi-select yes/no livestock
  ownership variable, as it appears in Designer.

- cases:

  Data frame. Case to include in analysis

- group_var:

  Atomic character vector. Name of the grouping variable

## Value

gt table object
