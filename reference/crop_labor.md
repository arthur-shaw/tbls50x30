# Create crop labor table

Create crop labor table

## Usage

``` r
crop_labor(
  hhold_df,
  cases,
  grew_crops_var,
  grew_crops_val = 1,
  paid_var,
  free_var,
  members_df,
  member_worked_var,
  member_worked_val = 1,
  group_var
)
```

## Arguments

- hhold_df:

  Data frame. Household-level variables.

- cases:

  Data frame. Case to include in analysis.

- grew_crops_var:

  Atomic character vector. Name of variable that indicates whether not
  grew crops.

- grew_crops_val:

  Atomic numeric vector. Value of `grew_crops_var` to indicate crops are
  grown.

- paid_var:

  Atomic character vector. Name of variable for categories of paid crop
  labor, as it appears in Designer.

- free_var:

  Atomic character vector. Name of variable for categories of
  free/exchange crop labor, as it appears in Designer.

- members_df:

  Data frame. Household member-level data.

- member_worked_var:

  Atomic character vector. Name of variable that indicates whether
  member worked to grow crops.

- member_worked_val:

  Atomic numeric vector. Value of `member_worked_var` indicating that
  the member worked.

- group_var:

  Atomic character vector. Name of the grouping variable
