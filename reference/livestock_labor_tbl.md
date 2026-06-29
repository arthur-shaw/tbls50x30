# Create livestock labor table

Create livestock labor table

## Usage

``` r
livestock_labor_tbl(
  hhold_df,
  cases,
  have_anim_var,
  have_anim_val = 1,
  anim_labor_df,
  anim_labor_id_var,
  anim_labor_var,
  anim_labor_none_val = 0,
  hhold_labor_vals = c(1, 2, 3),
  free_labor_val = 4,
  paid_labor_val = 5,
  group_var
)
```

## Arguments

- hhold_df:

  Data frame. Household-level data.

- cases:

  Data frame. Cases to include in analysis.

- have_anim_var:

  Character. Indicator variable: whether raise livestock.

- have_anim_val:

  Numeric. Indicator value: raise livestock

- anim_labor_df:

  Data frame. Roster of livestock labor.

- anim_labor_id_var:

  Character. Labor ID variable in livestock labor roster.

- anim_labor_var:

  Character. Number of workers variable.

- anim_labor_none_val:

  Numeric. Value indicatoing no workers.

- hhold_labor_vals:

  Numeric vector. Codes for household labor categories.

- free_labor_val:

  Numeric. Value of free labor.

- paid_labor_val:

  Numeric. Value of paid labor.

- group_var:

  Character. Name of group variable.

## Value

gt table object
