# Produces table for sales and production of milk or eggs

Produces table for sales and production of milk or eggs

## Usage

``` r
anim_prod_sales(
  livestock_df,
  cases,
  anim_id_var,
  anim_vals,
  produced_var,
  produced_val = 1,
  sold_var,
  sold_val = 1,
  amt_sold_vars,
  amt_sold_dk_val,
  group_var,
  table_title,
  anim_type_label
)
```

## Arguments

- livestock_df:

  Data frame. Livestock ownership and egg and milk production and sales.

- cases:

  Data frame. Case to include in analysis

- anim_id_var:

  Atomic character vector. Name of ID variable for livestock roster.

- anim_vals:

  Numeric vector. Values of `anim_id_var` that identify milk- or
  egg-producing animals, respectively.

- produced_var:

  Atomic character vector. Variable for egg/milk production.

- produced_val:

  Atomic numeric vector. Value to `produced_var` to indicate production.

- sold_var:

  Atomic character vector. Variable for egg/milk sales.

- sold_val:

  Atomic numeric vector. Value to `sold_var` to indicate sales.

- amt_sold_vars:

  Character vector. Name variable(s) that capture sales revenue. In some
  cases, a single variable; in others, one variable for total value,
  another for unit value.

- amt_sold_dk_val:

  Atomic numeric vector. Value of "do not know" (DK) option for sales
  revenue.

- group_var:

  Atomic character vector. Name of the grouping variable

- table_title:

  Atomic character vector. Title of the table.

- anim_type_label:

  Atomic character vector. Column label of count of milk/egg producing
  animals.

## Value

gt table object
