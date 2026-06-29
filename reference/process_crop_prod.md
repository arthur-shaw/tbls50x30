# Create stats on crop processing

Create stats on crop processing

## Usage

``` r
process_crop_prod(
  hhold_df,
  temp_crops_var,
  perm_crops_var,
  processed_var,
  processed_val = 1,
  products_var,
  cases,
  product_df,
  sold_var,
  sold_val = 1,
  amt_sold_vars,
  amt_sold_dk_val,
  group_var
)
```

## Arguments

- hhold_df:

  Household data frame

- temp_crops_var:

  Character. Indicator variable: any temporary crop harvested.

- perm_crops_var:

  Character. Indicator variable: any permanent crop harvested.

- processed_var:

  Character. Indicator variable: any crops processed.

- processed_val:

  Numeric. Value of indicator: any crops processed.

- products_var:

  Character. Name of variable as in Designer: which processed products.

- cases:

  Data frame. Case to include in analysis

- product_df:

  Data frame. roster of processed crops.

- sold_var:

  Character. Indicator variable: whether product sold.

- sold_val:

  Numeric. Value of indicator: product sold.

- amt_sold_vars:

  Character vector. Name(s) of variables with sales values.

- amt_sold_dk_val:

  Numeric. Value of "do not know" value.

- group_var:

  Character. Name of group variable.

## Value

gt table object
