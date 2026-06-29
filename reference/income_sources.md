# Create table of income sources

Create table of income sources

## Usage

``` r
income_sources(
  hhold_df,
  crop_var,
  livestock_var,
  temp_crop_df,
  temp_crop_var,
  temp_crop_val = 1,
  perm_crop_df,
  perm_crop_var,
  perm_crop_val = 1,
  processed_df,
  processed_var,
  processed_val = 1,
  anim_df,
  sold_live_anim_var,
  slaughter_anim_var,
  slaughter_anim_val = 1,
  sold_live_poultry_var,
  slaughter_poultry_var,
  slaughter_poultry_val = 1,
  milk_var,
  milk_val = 1,
  eggs_var,
  eggs_val = 1,
  oth_anim_prod_df,
  oth_anim_var,
  oth_anim_val = 1,
  cases,
  group_var
)
```

## Arguments

- hhold_df:

  Data frame. Household-level data.

- crop_var:

  Character. Indicator variable: grow crops.

- livestock_var:

  Character. Indicator variable: raise livestock.

- temp_crop_df:

  Data frame. Temporary crop disposition data frame.

- temp_crop_var:

  Character. Indicator variable: sell temporary crops.

- temp_crop_val:

  Numeric. Value: sell crops.

- perm_crop_df:

  Data frame. Permanent crop disposition data frame.

- perm_crop_var:

  Character. Indicator variable: sell permanent crops.

- perm_crop_val:

  Numeric. Value: sell crops.

- processed_df:

  Data frame. Processed crops.

- processed_var:

  Character. Indicator variable: sell processed crops.

- processed_val:

  Numeric. Value: sell processed crop.

- anim_df:

  Data frame. Livestock data frame.

- sold_live_anim_var:

  Character. Indicator variable: sold live animals.

- slaughter_anim_var:

  Character. Indicator variable: sold slaughtered animal.

- slaughter_anim_val:

  Numeric. Value: sold slaughtered animal.

- sold_live_poultry_var:

  Character. Indicator variable: sold live poultry.

- slaughter_poultry_var:

  Character. Indicator variable: sold slaughtered poultry.

- slaughter_poultry_val:

  Numeric. Value: sold slaughtered poultry.

- milk_var:

  Character. Indicator variable: sold milk.

- milk_val:

  Numeric. Value: sold milk.

- eggs_var:

  Character. Indicator variable: sold eggs.

- eggs_val:

  Numeric. Value: sold eggs.

- oth_anim_prod_df:

  Data frame. Roster of other animal products.

- oth_anim_var:

  Character. Indicator variable: sold other animal products.

- oth_anim_val:

  Numeric. Value: sold other animal products.

- cases:

  Data frame. Cases to include in table analysis.

- group_var:

  Character. Name of group variable.

## Value

gt table object
