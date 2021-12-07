
# create data frames:
# parcel
# parcel_plot
# parcel_plot_crop
# crop


#' Extract variable values from value labels
#' 
#' First, extract the 'labels' attribute from the column of interest. 
#' Then, returns a numeric vector of possible values
#' Next, remove names of each vector element
#' 
#' @param df Data frame
#' @param var Variable of interest
#' 
#' @return Numeric vector. Values of a labelled variable.
#' 
#' @importFrom dplyr `%>%` pull
extract_var_values <- function(
    df,
    var
) {

    base::attr({{df}} %>% dplyr::pull({{var}}), which = "labels") %>% base::unname()
    # Note: it appears that one doesn't need to import `{{`

}

# first, convert labelled columns to factors, replacing numeric values with characters
# then, extract the character levels, which correspond
# source: Senegal moduleResponseRate.R
#' Extract labels from value labels
#' 
#' First, extract the 'labels' attribute from the column of interest.
#' Then, returns a numeric vector of possible values
#' Next, keep the names of each vector element--that is, the string value label
#' 
#' @param df Data frame.
#' @param var Bare variable name.
#' 
#' @return Numeric vector. Values of a labelled variable.
#' 
#' @return Character vector. Character labels of a labelled variable.
#' 
#' @importFrom dplyr `%>%` pull
extract_var_labels <- function(
    df,
    var
) {

    base::attr({{df}} %>% dplyr::pull({{var}}), which = "labels") %>% base::names()
    # Note: it appears that one doesn't need to import `{{`

}


# TODO: write this with inspiration from lines 188 and 189 of moduleResponseRate.R
#' Create column labels
#' 
#' Create {gt} column labels that combine labels and values in in the following format: `"Label (value)"`
#' 
#' @param labels Character vector. Value labels.
#' @param values Numeric vector. Values corresponding to labels.
#' @param df Data frame with target varibles.
#' @param pattern Character. Pattern to identify variables to label.
#' 
#' @importFrom glue glue
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_subset
#' @importFrom purrr map
#' @importFrom gt html
create_col_labels <- function(
    labels,
    values,
    df, 
    pattern
) {

    # compose the column label
    column_lbl_text <- glue::glue("{labels}<br>({values})")

    column_names <- names(df) %>% stringr::str_subset(pattern = pattern)

    column_lbls_list <- stats::setNames(as.list(column_lbl_text), column_names)

    column_lbls_html <- purrr::map(.x = column_lbls_list, .f = ~ gt::html(.x))

    return(column_lbls_html)

}

#' Create dummies from a variable
#' 
#' Create a dummy for each level of a labelled variable
#' 
#' @param df Data frame
#' @param var Bare variable name of a 
#' 
#' @return Data frame with dummies added
#' 
#' @importFrom purrr map_dfc
#' @importFrom dplyr transmute
#' @importFrom stringr str_replace
#' @importFrom rlang `:=`
create_dummies <- function(
    df,
    var
) {

    # determine values
    values <- extract_var_values(df = {{df}}, var = {{var}})


    dummies <- purrr::map_dfc(
        .x = values,
        .f = ~ dplyr::transmute(
            {{df}}, 
            "{{var}}__{stringr::str_replace(.x, '-', 'n')}" := {{var}} == .x
        )
    )

    cbind(df, dummies)

}

# file_name -> ID var

#' Replace NaN with NA_real_
#' 
#' Replace NaN, which results from division by 0, with NA. This makes data destined for tables easier to handle.
#' 
#' @param df Data frame
#' 
#' @importFrom dplyr `%>%` mutate across 
replace_nan <- function(df) {

    df %>%
    dplyr::mutate(
        dplyr::across(
            .cols = where(is.numeric),
            # note: using base::ifelse() rather than dplyr::if_else() to avoid type issues
            .fns = ~ ifelse(
                test = is.nan(.x), 
                yes = NA_real_,
                no = .x
            )
        )
    )

}

#' Construct a quosure for multi-select questions
#' 
#' Creates a quosure of the form: `varname__1 == 1 | varname__7 == 1`.
#' 
#' @param varname Character. Variable name, as it appears in SuSo Designer, for the multi-select question of interest.
#' @param vals Numeric vector. Answer options
#' 
#' @return Quosure
#' 
#' @importFrom rlang parse_quo global_env
make_multi_select_quos <- function(
    varname,
    vals
) {

    # construct expression as string
    # of the form `varname__1 == 1 | varname__7 == 1`
    expr_string <- paste0(
        varname, "__", 
        ifelse(vals > 0, vals, gsub(x = vals, pattern = "-", replacement = "n")), " == 1", 
        collapse = " | "
    )

    # parse string as quosure
    rlang::parse_quo(expr_string, env = rlang::global_env())

}

# TODO: consider "passing the dots" to `gt::tab_options()`

#' Apply style to tables
#' 
#' Apply styles from `gt::tab_options()` to tables
#' 
#' @param df Data frame
#' @param heading_color Character. Hex color for table header background color.
#' @param column_label_color Character. Hex color for column label background color.
#' @param row_group_color  Character. Hex color for row group background color.
#' 
#' @export 
style_table <- function(
    df, 
    heading_color = "#0F2B1D",
    column_label_color = "#264535",
    row_group_color = "#516A5D"
) {

    df %>%
    # apply colors to header, column labels, and row groups
    gt::tab_options(
        heading.background.color = heading_color,
        column_labels.background.color = column_label_color,
        row_group.background.color = row_group_color
    ) %>%
    # replace NA with ---
    gt::fmt_missing(columns = gt::everything())

}