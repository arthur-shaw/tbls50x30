# Create table on product production and sales

Meant for fisheries, aquaculture, and forestry products.

## Usage

``` r
other_prod_sales(
  hhold_df,
  product_df,
  cases,
  practice_var,
  practice_val = 1,
  products_var,
  sold_var,
  sold_val = 1,
  amt_sold_vars,
  amt_sold_dk_val,
  group_var,
  table_title
)
```

## Arguments

- hhold_df:

  Data frame. Household-level data.

- product_df:

  Data frame. Product-level data.

- cases:

  Data frame. Case to include in analysis

- practice_var:

  Atomic character vector. Name of ID variable for whether practice
  production.

- practice_val:

  Atomic numeric vector. Value of `practice_var` to indicate practicing.

- products_var:

  Atomic character vector. Name of variable for items produced, as it
  appears in Designer.

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

## Value

gt table object
