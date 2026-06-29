# Create table for sector labor

Create table for sector labor

## Usage

``` r
sector_labor(
  hhold_df,
  produce_var,
  produce_val = 1,
  cases,
  labor_var,
  hhold_labor_vals = c(1, 2, 3),
  free_labor_val = 4,
  paid_labor_val = 5,
  group_var,
  table_title,
  sector_label
)
```

## Arguments

- hhold_df:

  Data frame. Household-level data.

- produce_var:

  Character. Indicator variable: whether engaged in sector.

- produce_val:

  Numeric. Value: engaged in sector.

- cases:

  Data frame. Cases to include in table computations.

- labor_var:

  Character. Name of labor categories used. Name of multi-select
  question as in Designer.

- hhold_labor_vals:

  Numeric vector. Codes of answers corresponding to household labor.

- free_labor_val:

  Numeric. Code of free labor.

- paid_labor_val:

  Numeric. Code of paid labor.

- group_var:

  Character. Name of group variable.

- table_title:

  Character. Title of table.

- sector_label:

  Character. Label of first column that counts households engaged in
  sector.

## Value

gt table object
