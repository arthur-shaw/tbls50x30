# Create permanent crop sales table

Create permanent crop sales table

## Usage

``` r
perm_crop_sales(
  crops_sales_df,
  cases,
  crop_id_var,
  crop_vals,
  sold_var,
  sold_val = 1,
  amt_sold_vars,
  amt_sold_dk_val,
  group_var
)
```

## Arguments

- crops_sales_df:

  Data frame. Crop sales data.

- cases:

  Data frame of cases to include in analysis. Data frame must contain
  `interview__id` and the grouping variable indicated in the `group_var`
  parameter

- crop_id_var:

  Atomic character vector. Name of crop ID variable as a character.

- crop_vals:

  Numeric vector. Codes that identify permanent crops.

- sold_var:

  Atomic character vector. Name of variable that indicates whether crop
  was sold.

- sold_val:

  Atomic numeric vector. Value of `sold_var` that indicates the crop was
  sold.

- amt_sold_vars:

  Character vector. Name variable(s) that capture sales revenue. In some
  cases, a single variable; in others, one variable for total value,
  another for unit value.

- amt_sold_dk_val:

  Atomic numeric vector. Value of "do not know" (DK) option for sales
  revenue.

- group_var:

  Atomic character vector. Name of the grouping variable

## Value

gt table object
