# TODO: think about generalizing to harvest_stats() and somehow indicating the crop type--maybe through a list of crops
    # if cover permanent crops, decide whether to include number of plants that produced (s02a_q06)
# TODO: provide method to inject truncated column labels to replace longer text drawn from qnr

#' Create temporary crop harvest table
#' 
#' @param parcel_plot_crop_df Data frame of parcel-plot-crop-level observations.
#' @param cases Data frame of cases to include in analysis. Data frame must contain `interview__id` and the grouping variable indicated in the `group_var` parameter
#' @param crop_id_var Atomic character vector. Name of crop ID variable as a character.
#' @param crop_vals Numeric vector. Codes of the crops that are temporary crops.
#' @param harvest_var Atomic character vector. Name of variable indicating whether the crop was harvested.
#' @param harvest_val Atomic numeric vector. Value of `harvest_var` that indicates the crop was harvested.
#' @param why_not_harvest_var Atomic character vector. Name of variable, as it appears in Designer, that captures the reason(s) the crop was not harvested.
#' @param group_var Atomic character vector. Name of the grouping variable
#' @param json_qnr_path Character. Path to JSON file that describes the questionnaire.
#' 
#' @return {gt} table object
#' 
#' @importFrom rlang sym list2 .data
#' @importFrom dplyr `%>%` left_join filter mutate group_by summarise across starts_with ungroup select
#' @importFrom susometa parse_questionnaire get_ms_answers_as_var_labels
#' @importFrom gt gt html tab_header cols_label tab_spanner fmt_number
#' 
#' @export 
temp_crop_harvest <- function(
    parcel_plot_crop_df,
    cases,
    crop_id_var,
    crop_vals,
    harvest_var,
    harvest_val = 1,
    why_not_harvest_var,
    group_var,
    json_qnr_path
) {

    # make group variable into symbol for later evaluation
    crop_id_var <- rlang::sym(crop_id_var)
    harvest_var_txt <- harvest_var
    harvest_var <- rlang::sym(harvest_var)
    why_not_harvest_var_txt <- why_not_harvest_var
    why_not_harvest_var <- rlang::sym(why_not_harvest_var)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)    

    # compute statistics
    harvest_stats <- cases %>%
        dplyr::left_join(parcel_plot_crop_df, by = "interview__id") %>%
        dplyr::filter(!!crop_id_var %in% crop_vals) %>%
        dplyr::mutate(
            planted = 1,
            harvested = !!harvest_var == harvest_val,
            n_why_not_harvest = rowSums(dplyr::select(., dplyr::starts_with(paste0(why_not_harvest_var_txt, "__"))), na.rm = TRUE)
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$planted, .data$harvested, .data$n_why_not_harvest,
                    dplyr::starts_with(paste0(why_not_harvest_var_txt, "__"))
                ),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            harvested = .data$harvested / .data$planted,
            dplyr::across(
                .cols = dplyr::starts_with(paste0(why_not_harvest_var_txt, "__")),
                .fns = ~ .x / .data$n_why_not_harvest                
            )
        ) %>%
        dplyr::select(-.data$n_why_not_harvest) %>%
        replace_nan()

    # compose table
    
    # compile column labels
    # ... for fixed-name vars
    col_lbls1 <- rlang::list2(
        planted = gt::html("Planted<br>(N)"),
        harvested = gt::html("Harvested<br>(%)")
    )

    # ... from multi-select labels in JSON version of qnr
    qnr <- susometa::parse_questionnaire(path = json_qnr_path)
    col_lbls2 <- susometa::get_ms_answers_as_var_labels(
        qnr_df = qnr, 
        varname = !!why_not_harvest_var
    )

# TODO: use tryCatch to attempt to get labels, but return empty labels otherwise (or labels with variable names)

    # combine labels
    col_labels <- c(col_lbls1, col_lbls2)

    harvest_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Prevalence of temporary crop harvest") %>%
    gt::cols_label(.list = col_labels) %>%
    gt::tab_spanner(
        columns = dplyr::starts_with(paste0(why_not_harvest_var_txt, "__")),
        label = "Why not harvested (%)"
    ) %>%
    gt::fmt_number(
        columns = c(.data$harvested, matches(paste0(why_not_harvest_var_txt, "__"))),
        decimals = 1,
        scale_by = 100
    ) %>%
    style_table()

}

#' Create temprorary crop sales table
#' 
#' @param crops_sales_df Data frame of parcel-plot-crop-level observations.
#' @param cases Data frame of cases to include in analysis. Data frame must contain `interview__id` and the grouping variable indicated in the `group_var` parameter
#' @param crop_id_var Atomic character vector. Name of crop ID variable as a character.
#' @param sold_var Atomic character vector. Name of variable that indicates whether crop was sold.
#' @param sold_val Atomic numeric vector. Value of `sold_var` that indicates the crop was sold.
#' @param amt_sold_vars Character vector. Name variable(s) that capture sales revenue. In some cases, a single variable; in others, one variable for total value, another for unit value.
#' @param amt_sold_dk_val Atomic numeric vector. Value of "do not know" (DK) option for sales revenue.
#' @param group_var Atomic character vector. Name of the grouping variable
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym syms parse_quo global_env .data
#' @importFrom dplyr case_when `%>%` left_join filter mutate group_by summarise across ungroup starts_with
#' @importFrom glue glue glue_collapse
#' @importFrom gt gt cols_label html tab_spanner tab_footnote fmt_number
#' 
#' @export 
temp_crop_sales <- function(
    crops_sales_df,
    cases,
    crop_id_var,
    sold_var,
    sold_val = 1,
    amt_sold_vars,
    amt_sold_dk_val,
    group_var
) {

    # make group variable into symbol for later evaluation
    crop_id_var <- rlang::sym(crop_id_var)
    sold_var_txt <- sold_var
    sold_var <- rlang::sym(sold_var)
    amt_sold_vars_txt <- amt_sold_vars
    amt_sold_vars <- rlang::syms(amt_sold_vars)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)    

    sales_miss_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt}))"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt[1]}))",
            "(haven::is_tagged_na({amt_sold_vars_txt[2]}))", 
            .sep = " | "
        )
    )
    # TODO: undersand why expr above generates 2 copies of glue; why I have to subset below
    sales_miss_quo <- rlang::parse_quo(sales_miss_expr_txt[1], env = rlang::global_env())

    sales_dk_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "({amt_sold_vars_txt} == {amt_sold_dk_val})"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue_collapse(
            glue::glue(
                "({amt_sold_vars_txt[1]} == {amt_sold_dk_val})",
                "({amt_sold_vars_txt[2]} == {amt_sold_dk_val})", 
                .sep = " | "
            )
        )
    )
    sales_dk_quo <- rlang::parse_quo(sales_dk_expr_txt[1], env = rlang::global_env())   

    # compute statistics
    sales_stats <- cases %>%
        dplyr::left_join(crops_sales_df, by = "interview__id") %>%
        dplyr::filter(!is.na(!!crop_id_var)) %>%
        dplyr::mutate(
            harvested = 1,
            sold = !!sold_var == sold_val,
            sales_miss = !!sales_miss_quo,
            sales_dk = !!sales_dk_quo
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(.data$harvested, .data$sold, .data$sales_miss, .data$sales_dk),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = starts_with("sales_"),
                .fns = ~ .x / .data$sold
            ),
            sold = .data$sold / .data$harvested
        ) %>%
        replace_nan()

    # compose table
    sales_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::cols_label(
        harvested = gt::html("Harvested<br>(N)"),
        sold = gt::html("Sold<br>(%)"),
        sales_miss = "Missing",
        sales_dk = "DK"
    ) %>%
    gt::tab_spanner(
        columns = dplyr::starts_with("sales_"),
        label = "Values (%)",
        id = "sales"
    ) %>%
    gt::tab_footnote(
        footnote = "Percentage is computed as (number of values / number parcel-plot-crop sales).",
        locations = gt::cells_column_spanners(spanners = "sales")
    ) %>%
    gt::fmt_number(
        columns = c(.data$sold, .data$sales_miss, .data$sales_dk),
        decimals = 1,
        scale_by = 100
    ) %>%        
    style_table()

}

#' Create permanent crop harvest table
#' 
#' @param parcel_plot_crop_df Data frame of parcel-plot-crop-level observations.
#' @param cases Data frame of cases to include in analysis. Data frame must contain `interview__id` and the grouping variable indicated in the `group_var` parameter
#' @param crop_id_var Atomic character vector. Name of crop ID variable as a character.
#' @param crop_vals Numeric vector. Codes of the crops that are temporary crops.
#' @param produce_var Atomic character vector. Name of variable indicating whether the crop was produced.
#' @param harvest_var Atomic character vector. Name of variable indicating whether the crop was harvested.
#' @param harvest_val Atomic numeric vector. Value of `harvest_var` that indicates the crop was harvested.
#' @param group_var Atomic character vector. Name of the grouping variable
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym .data
#' @importFrom dplyr `%>%` left_join filter mutate group_by summarise across ungroup
#' @importFrom gt gt tab_header cols_label html fmt_number tab_footnote cells_column_labels
#' 
#' @export 
perm_crop_harvest <- function(
    parcel_plot_crop_df,
    cases,
    crop_id_var,
    crop_vals,
    produce_var,
    harvest_var,
    harvest_val = 1,
    group_var
) {

    crop_id_var <- rlang::sym(crop_id_var)
    produce_var <- rlang::sym(produce_var)
    harvest_var <- rlang::sym(harvest_var)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    # compute statistics
    harvest_stats <- cases %>%
        dplyr::left_join(parcel_plot_crop_df, by = "interview__id") %>%
        dplyr::filter(!!crop_id_var %in% crop_vals) %>%
        dplyr::mutate(
            planted = 1,
            produced = (!!produce_var > 0),
            harvested = (!!harvest_var == harvest_val)
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(.data$planted, .data$produced, .data$harvested),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$produced, .data$harvested),
                .fns = ~ .x / .data$planted
            )
        ) %>%
        replace_nan()

    # compose table

    harvest_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Prevalence of tree/permanent crop harvest") %>%
    gt::cols_label(
        planted = gt::html("Planted<br>(N)"),
        produced = gt::html("Produced<br>(%)"),
        harvested = gt::html("Harvested<br>(%)")
    ) %>%
    gt::fmt_number(
        columns = c(.data$produced, .data$harvested),
        decimals = 1,
        scale_by = 100
    ) %>%
    gt::tab_footnote(
        footnote = "Percentage of all crops planted",
        locations = gt::cells_column_labels(columns = .data$harvested)
    ) %>%
    style_table()

}

#' Create permanent crop sales table
#' 
#' @param crops_sales_df Data frame. Crop sales data.
#' @param crop_vals Numeric vector. Codes that identify permanent crops.
#' @inheritParams temp_crop_sales
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym syms parse_quo global_env .data
#' @importFrom dplyr case_when `%>%` left_join filter mutate group_by summarise across ungroup starts_with
#' @importFrom glue glue glue_collapse
#' @importFrom gt gt cols_label html tab_spanner tab_footnote fmt_number
#' 
#' @export 
perm_crop_sales <- function(
    crops_sales_df,
    cases,
    crop_id_var,
    crop_vals,
    sold_var,
    sold_val = 1,
    amt_sold_vars,
    amt_sold_dk_val,
    group_var
) {

    # make group variable into symbol for later evaluation
    crop_id_var <- rlang::sym(crop_id_var)
    sold_var_txt <- sold_var
    sold_var <- rlang::sym(sold_var)
    amt_sold_vars_txt <- amt_sold_vars
    amt_sold_vars <- rlang::syms(amt_sold_vars)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)    

    sales_miss_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt}))"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt[1]}))",
            "(haven::is_tagged_na({amt_sold_vars_txt[2]}))", 
            .sep = " | "
        )
    )
    # TODO: undersand why expr above generates 2 copies of glue; why I have to subset below
    sales_miss_quo <- rlang::parse_quo(sales_miss_expr_txt[1], env = rlang::global_env())

    sales_dk_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "({amt_sold_vars_txt} == {amt_sold_dk_val})"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue_collapse(
            glue::glue(
                "({amt_sold_vars_txt[1]} == {amt_sold_dk_val})",
                "({amt_sold_vars_txt[2]} == {amt_sold_dk_val})", 
                .sep = " | "
            )
        )
    )
    sales_dk_quo <- rlang::parse_quo(sales_dk_expr_txt[1], env = rlang::global_env())

    # compute statistics
    sales_stats <- cases %>%
        dplyr::left_join(crops_sales_df, by = "interview__id") %>%
        dplyr::filter(!is.na(!!crop_id_var)) %>%
        dplyr::mutate(
            harvested = 1,
            sold = !!sold_var == sold_val,
            sales_miss = !!sales_miss_quo,
            sales_dk = !!sales_dk_quo
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(.data$harvested, .data$sold, .data$sales_miss, .data$sales_dk),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = starts_with("sales_"),
                .fns = ~ .x / .data$sold
            ),
            sold = .data$sold / .data$harvested
        ) %>%
        replace_nan()

    # compose table
    sales_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::cols_label(
        harvested = gt::html("Harvested<br>(N)"),
        sold = gt::html("Sold<br>(%)"),
        sales_miss = "Missing",
        sales_dk = "DK"
    ) %>%
    gt::tab_spanner(
        columns = dplyr::starts_with("sales_"),
        label = "Values (%)",
        id = "sales"
    ) %>%
    gt::tab_footnote(
        footnote = "Percentage is computed as (number of values / number parcel-plot-crop sales).",
        locations = gt::cells_column_spanners(spanners = "sales")
    ) %>%
    gt::fmt_number(
        columns = c(.data$sold, .data$sales_miss, .data$sales_dk),
        decimals = 1,
        scale_by = 100
    ) %>%    
    style_table()

}

#' Create livestock ownership table
#' 
#' @param hhold_df Data frame. 
#' @param animal_var Atomic character vector. Name of livestock ownership variable, as it appears in Designer.
#' @param cases Data frame. Case to include in analysis
#' @param group_var Atomic character vector. Name of the grouping variable
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym .data
#' @importFrom dplyr `%>%` left_join mutate select if_else group_by summarise across ungroup
#' @importFrom gt gt tab_header cols_label html tab_footnote fmt_number
#' 
#' @export 
livestock_ownership <- function(
    hhold_df,
    animal_var,
    cases,
    group_var
) {

    animal_var_txt <- animal_var
    animal_var <- rlang::sym(animal_var)

    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    # compute statistics
    livestock_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(
            n_hholds = 1,
            n_animals = rowSums(
                dplyr::select(., starts_with(paste0(animal_var_txt, "__"))), 
                na.rm = TRUE
            ),
            owns_animals = dplyr::if_else(.data$n_animals > 0, 1, 0, 0)
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(.data$n_hholds, .data$n_animals, .data$owns_animals),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(avg_animals = .data$n_animals/.data$owns_animals) %>%
        dplyr::select(!!group_var, .data$n_hholds, .data$owns_animals, .data$avg_animals) %>%
        replace_nan()

    # compose display table
    livestock_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Livestock ownership") %>%
    gt::cols_label(
        n_hholds = gt::html("Households<br>(N)"),
        owns_animals = gt::html("Raises livestock<br>(N)"),
        avg_animals = gt::html("Types of livestock<br>(AVG)")
    ) %>%
    gt::tab_footnote(
        footnote = "Percentage computed as (number of animals owned / number who own animals)",
        locations = gt::cells_column_labels(columns = .data$avg_animals)
    ) %>%
    gt::fmt_number(
        columns = c(.data$avg_animals),
        decimals = 1
    ) %>%    
    style_table()

}

#' Create cow displacement table
#' 
#' @param hhold_df Data frame. All household-level variables.
#' @param cases Data frame. Case to include in analysis
#' @param animal_var Atomic character vector. Name of livestock ownership variable, as it appears in Designer.
#' @param bull_val Atomic numeric vector. Value of bull answer option.
#' @param cow_val Atomic numeric vector. Value of cow answer option.
#' @param steer_heifer_val Atomic numeric vector. Value of steer/heifer answer option.
#' @param calf_val Atomic numeric vector. Value of calf answer option.
#' @param group_var Atomic character vector. Name of the grouping variable
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym global_env parse_quo .data
#' @importFrom glue glue
#' @importFrom dplyr `%>%` left_join mutate select group_by summarise ungroup across
#' @importFrom gt gt tab_header cols_label html tab_spanner fmt_number
#' 
#' @export 
cow_displacement <- function(
    hhold_df,
    cases,
    animal_var,
    bull_val = 10,
    cow_val = 12,
    steer_heifer_val = 13,
    calf_val = 14,
    group_var
) {

    animal_var_txt <- animal_var
    animal_var <- rlang::sym(animal_var)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    # compute statistics
    bull_quo <- rlang::parse_quo(
        glue::glue("{animal_var_txt}__{bull_val} == 1"), 
        env = rlang::global_env()
    )
    cow_quo <- rlang::parse_quo(
        glue::glue("{animal_var_txt}__{cow_val} == 1"), 
        env = rlang::global_env()
    )
    steer_heifer_quo <- rlang::parse_quo(
        glue::glue("{animal_var_txt}__{steer_heifer_val} == 1"), 
        env = rlang::global_env()
    )
    calf_quo <- rlang::parse_quo(
        glue::glue("{animal_var_txt}__{calf_val} == 1"), 
        env = rlang::global_env()
    )
    cattle_quo <- rlang::parse_quo(
        glue::glue(
            "({animal_var_txt}__{bull_val} == 1) |",
            "({animal_var_txt}__{cow_val} == 1) |",
            "({animal_var_txt}__{steer_heifer_val} == 1) |",
            "({animal_var_txt}__{calf_val} == 1)",
            .sep = " "
        ),
        env = rlang::global_env()
    )

    cow_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(
            n_own_cattle = !!cattle_quo,
            n_types = rowSums(
                dplyr::select(., 
                    !!rlang::sym(glue::glue("{animal_var_txt}__{bull_val}")),
                    !!rlang::sym(glue::glue("{animal_var_txt}__{cow_val}")),
                    !!rlang::sym(glue::glue("{animal_var_txt}__{steer_heifer_val}")),
                    !!rlang::sym(glue::glue("{animal_var_txt}__{calf_val}"))
                ),
                na.rm = TRUE
            ),
            bull = !!bull_quo,
            cow = !!cow_quo,
            steer_heifer = !!steer_heifer_quo,
            calf = !!calf_quo
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$n_own_cattle, .data$n_types, 
                    .data$bull, .data$cow, .data$steer_heifer, .data$calf
                ),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$bull, .data$cow, .data$steer_heifer, .data$calf),
                .fns = ~ .x / .data$n_types
            )
        ) %>%
        replace_nan()

    # compose display table
    cow_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Cow displacement") %>%
    gt::cols_label(
        n_own_cattle = gt::html("Have cattle<br>(N)"),
        n_types = gt::html("Cattle types<br>(N)"),
        bull = gt::html("Bulls"),
        cow = gt::html("Cows"),
        steer_heifer = gt::html("Steers/Heifers"),
        calf = gt::html("Calves")
    ) %>%
    gt::tab_spanner(
        columns = c(.data$bull, .data$cow, .data$steer_heifer, .data$calf),
        label = "Types (%)"
    ) %>%
    gt::fmt_number(
        columns = c(.data$bull, .data$cow, .data$steer_heifer, .data$calf),
        decimals = 1,
        scale_by = 100
    ) %>%
    style_table()

}

#' Create hen displacement table
#' 
#' @param hhold_df Data frame. All household-level variables.
#' @param animal_var Atomic character vector. Name of livestock ownership variable, as it appears in Designer.
#' @param cases Data frame. Case to include in analysis
#' @param cock_val Atomic numeric vector. Value of cock/broiler answer option.
#' @param hen_val Atomic numeric vector. Value of hen answer option.
#' @param pullet_val Atomic numeric vector. Value of pullet answer option.
#' @param group_var Atomic character vector. Name of the grouping variable
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym global_env parse_quo .data
#' @importFrom glue glue
#' @importFrom dplyr `%>%` left_join mutate select group_by summarise ungroup across
#' @importFrom gt gt tab_header cols_label html tab_spanner fmt_number
#' 
#' @export 
hen_displacement <- function(
    hhold_df,
    cases,
    animal_var,
    cock_val = 51,
    hen_val = 52,
    pullet_val = 53,
    group_var
) {

    animal_var_txt <- animal_var
    animal_var <- rlang::sym(animal_var)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    # compute statistics
    cock_quo <- rlang::parse_quo(
        glue::glue("{animal_var_txt}__{cock_val} == 1"), 
        env = rlang::global_env()
    )
    hen_quo <- rlang::parse_quo(
        glue::glue("{animal_var_txt}__{hen_val} == 1"), 
        env = rlang::global_env()
    )
    pullet_quo <- rlang::parse_quo(
        glue::glue("{animal_var_txt}__{pullet_val} == 1"), 
        env = rlang::global_env()
    )
    chicken_quo <- rlang::parse_quo(
        glue::glue(
            "({animal_var_txt}__{cock_val} == 1) |",
            "({animal_var_txt}__{hen_val} == 1) |",
            "({animal_var_txt}__{pullet_val} == 1)",
            .sep = " "
        ),
        env = rlang::global_env()
    )

    hen_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(
            n_own_chicken = !!chicken_quo,
            n_types = rowSums(
                dplyr::select(.,
                    !!rlang::sym(glue::glue("{animal_var_txt}__{cock_val}")),
                    !!rlang::sym(glue::glue("{animal_var_txt}__{hen_val}")),
                    !!rlang::sym(glue::glue("{animal_var_txt}__{pullet_val}"))
                ),
                na.rm = TRUE
            ),
            cock = !!cock_quo,
            hen = !!hen_quo,
            pullet = !!pullet_quo
        ) %>%
        group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$n_own_chicken, .data$n_types, 
                    .data$cock, .data$hen, .data$pullet
                ),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$cock, .data$hen, .data$pullet),
                .fns = ~ .x / .data$n_types
            )
        ) %>%
        replace_nan()

    # compose display table
    hen_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Hen displacement") %>%
    gt::cols_label(
        n_own_chicken = gt::html("Have chickens<br>(N)"),
        n_types = gt::html("Chicken types<br>(N)"),
        cock = gt::html("Cocks/broilers"),
        hen = gt::html("Hens"),
        pullet = gt::html("Pullets")
    ) %>%
    gt::tab_spanner(
        columns = c(.data$cock, .data$hen, .data$pullet),
        label = "Types (%)"    
    ) %>%
    gt::fmt_number(
        columns = c(.data$cock, .data$hen, .data$pullet),
        decimals = 1,
        scale_by = 100
    ) %>%
    style_table()    

}

#' Produces table for sales and production of milk or eggs
#' 
#' @param livestock_df Data frame. Livestock ownership and egg and milk production and sales.
#' @param cases Data frame. Case to include in analysis
#' @param anim_id_var Atomic character vector. Name of ID variable for livestock roster.
#' @param anim_vals Numeric vector. Values of `anim_id_var` that identify milk- or egg-producing animals, respectively.
#' @param produced_var Atomic character vector. Variable for egg/milk production.
#' @param produced_val Atomic numeric vector. Value to `produced_var` to indicate production.
#' @param sold_var Atomic character vector. Variable for egg/milk sales.
#' @param sold_val Atomic numeric vector. Value to `sold_var` to indicate sales.
#' @param amt_sold_vars Character vector. Name variable(s) that capture sales revenue. In some cases, a single variable; in others, one variable for total value, another for unit value.
#' @param amt_sold_dk_val Atomic numeric vector. Value of "do not know" (DK) option for sales revenue.
#' @param group_var Atomic character vector. Name of the grouping variable
#' @param table_title Atomic character vector. Title of the table.
#' @param anim_type_label Atomic character vector. Column label of count of milk/egg producing animals.
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym syms parse_quo global_env .data
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr `%>%` case_when left_join mutate group_by summarise across ungroup starts_with
#' @importFrom gt gt tab_header cols_label html tab_spanner tab_footnote cells_column_labels cells_column_spanners fmt_number
#' 
#' @export 
anim_prod_sales <- function(
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
) {

    anim_id_var_txt <- anim_id_var
    anim_id_var <- rlang::sym(anim_id_var)

    produced_var <- rlang::sym(produced_var)
    sold_var <- rlang::sym(sold_var)

    amt_sold_vars_txt <- amt_sold_vars
    amt_sold_vars <- rlang::syms(amt_sold_vars)

    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    sales_miss_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt}))"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt[1]}))",
            "(haven::is_tagged_na({amt_sold_vars_txt[2]}))", 
            .sep = " | "
        )
    )
    # TODO: undersand why expr above generates 2 copies of glue; why I have to subset below
    sales_miss_quo <- rlang::parse_quo(sales_miss_expr_txt[1], env = rlang::global_env())

    sales_dk_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "({amt_sold_vars_txt} == {amt_sold_dk_val})"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue_collapse(
            glue::glue(
                "({amt_sold_vars_txt[1]} == {amt_sold_dk_val})",
                "({amt_sold_vars_txt[2]} == {amt_sold_dk_val})", 
                .sep = " | "
            )
        )
    )
    sales_dk_quo <- rlang::parse_quo(sales_dk_expr_txt[1], env = rlang::global_env())   

    product_stats <- cases %>%
        dplyr::left_join(livestock_df, by = "interview__id") %>%
        dplyr::mutate(
            n_anim = !!anim_id_var %in% anim_vals,
            produced = !!produced_var == produced_val,
            sold = !!sold_var == sold_val,
            sales_miss = !!sales_miss_quo,
            sales_dk = !!sales_dk_quo
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$n_anim, .data$produced, .data$sold, 
                    .data$sales_miss, .data$sales_dk
                ),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = starts_with("sales_"),
                .fns = ~ .x / .data$sold
            ),
            sold = .data$sold / .data$produced,
            produced = .data$produced / .data$n_anim
        ) %>%
        replace_nan()

    # compose display table
    product_stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = table_title) %>%
    gt::cols_label(
        n_anim = gt::html(paste0(anim_type_label, "<br>(N)")),
        produced = gt::html("Produced<br>(%)"),
        sold = gt::html("Sold<br>(%)"),
        sales_miss = "Missing",
        sales_dk = "DK"
    ) %>%
    gt::tab_spanner(
        columns = dplyr::starts_with("sales_"),
        label = "Sales value (%)",
        id = "sales"
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_labels(columns = .data$sold),
        footnote = "Percentage computed as (number sold / number produced)"
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_spanners(spanners = "sales"),
        footnote = "Percentage computed as (number values / number sales observations)"
    ) %>%
    gt::fmt_number(
        columns = c(.data$produced, .data$sold, .data$sales_miss, .data$sales_dk),
        decimals = 1,
        scale_by = 100
    ) %>%
    style_table()

}

# NOTE: this will work for both fisheries, aquaculture, and forestry--which all have the same structure
# forestry can be accommodated because
#' Create table on product production and sales
#' 
#' Meant for fisheries, aquaculture, and forestry products.
#' 
#' @param hhold_df Data frame. Household-level data.
#' @param product_df Data frame. Product-level data.
#' @param cases Data frame. Case to include in analysis
#' @param practice_var Atomic character vector. Name of ID variable for whether practice production.
#' @param practice_val Atomic numeric vector. Value of `practice_var` to indicate practicing.
#' @param products_var Atomic character vector. Name of variable for items produced, as it appears in Designer.
#' @param sold_var Atomic character vector. Variable for egg/milk sales.
#' @param sold_val Atomic numeric vector. Value to `sold_var` to indicate sales.
#' @param amt_sold_vars Character vector. Name variable(s) that capture sales revenue. In some cases, a single variable; in others, one variable for total value, another for unit value.
#' @param amt_sold_dk_val Atomic numeric vector. Value of "do not know" (DK) option for sales revenue.
#' @param group_var Atomic character vector. Name of the grouping variable
#' @param table_title Atomic character vector. Title of the table.
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym parse_quo .data
#' @importFrom dplyr `%>%` case_when left_join mutate group_by summarise across ungroup starts_with
#' @importFrom glue glue glue_collapse
#' @importFrom gt gt tab_header cols_label html tab_spanner fmt_number tab_footnote cells_column_labels cells_column_spanners
#' 
#' @export 
other_prod_sales <- function(
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
) {

    practice_var <- rlang::sym(practice_var)
    
    sold_var <- rlang::sym(sold_var)

    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    amt_sold_vars_txt <- amt_sold_vars

    sales_miss_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt}))"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt[1]}))",
            "(haven::is_tagged_na({amt_sold_vars_txt[2]}))", 
            .sep = " | "
        )
    )
    # TODO: undersand why expr above generates 2 copies of glue; why I have to subset below
    sales_miss_quo <- rlang::parse_quo(sales_miss_expr_txt[1], env = rlang::global_env())

    sales_dk_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "({amt_sold_vars_txt} == {amt_sold_dk_val})"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue_collapse(
            glue::glue(
                "({amt_sold_vars_txt[1]} == {amt_sold_dk_val})",
                "({amt_sold_vars_txt[2]} == {amt_sold_dk_val})", 
                .sep = " | "
            )
        )
    )
    sales_dk_quo <- rlang::parse_quo(sales_dk_expr_txt[1], env = rlang::global_env())   

    # compute statistics
    hhold_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(
            n_hholds = 1,
            practice = !!practice_var == practice_val,
            n_products = rowSums(
                dplyr::select(., dplyr::starts_with(paste0(products_var, "__"))),
                na.rm = TRUE
            )
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(.data$n_hholds, .data$practice, .data$n_products),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup()

    product_stats <- cases %>%
        dplyr::left_join(product_df, by = "interview__id") %>%
        dplyr::mutate(
            sold = !!sold_var == sold_val,
            sales_miss = !!sales_miss_quo,
            sales_dk = !!sales_dk_quo
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(.data$sold, .data$sales_miss, .data$sales_dk),
                .fns = ~ any(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$sales_miss, .data$sales_dk),
                .fns = ~ .x / .data$sold
            )
        )

    stats <- hhold_stats %>%
        dplyr::left_join(product_stats, by = group_var_txt) %>%
        dplyr::mutate(
            sold = .data$sold / .data$practice,
            practice = .data$practice / .data$n_hholds
        ) %>%
        replace_nan()

    # compose display table
    stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = table_title) %>%
    gt::cols_label(
        n_hholds = gt::html("Total household<br>(N)"),
        practice = gt::html("Practice<br>(%)"),
        n_products = gt::html("Products<br>(AVG)"),
        sold = gt::html("Sell<br>(%)"),
        sales_miss = "Missing",
        sales_dk = "DK"
    ) %>%
    gt::tab_spanner(
        columns = c(.data$sales_miss, .data$sales_dk),
        label = "Sales value (%)",
        id = "sales"
    ) %>%
    gt::fmt_number(
        columns = c(.data$practice, .data$sold, .data$sales_miss, .data$sales_dk),
        decimals = 1,
        scale_by = 100
    ) %>%    
    gt::tab_footnote(
        locations = gt::cells_column_labels(columns = .data$n_products),
        footnote = "Average for households that produce."
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_labels(columns = .data$sold),
        footnote = "Percentage of households that produce."
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_spanners(spanners = "sales"),
        footnote = "Percentage of households that sell."
    ) %>%
    style_table()

}

# TODO: create temp_crops_var and perm_crops_var in fake data
# should they be harvAnyTempCrops/harvAnyPermCrops -- whether harvested any

#' Create stats on crop processing
#' 
#' @param hhold_df Household data frame
#' @param temp_crops_var Character. Indicator variable: any temporary crop harvested.
#' @param perm_crops_var Character. Indicator variable: any permanent crop harvested.
#' @param processed_var Character. Indicator variable: any crops processed.
#' @param processed_val Numeric. Value of indicator: any crops processed.
#' @param products_var Character. Name of variable as in Designer: which processed products. 
#' @param cases Data frame. Case to include in analysis
#' @param product_df Data frame. roster of processed crops.
#' @param sold_var Character. Indicator variable: whether product sold.
#' @param sold_val Numeric. Value of indicator: product sold.
#' @param amt_sold_vars Character vector. Name(s) of variables with sales values.
#' @param amt_sold_dk_val Numeric. Value of "do not know" value.
#' @param group_var Character. Name of group variable.
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym parse_quo global_env .data
#' @importFrom glue glue
#' @importFrom haven is_tagged_na
#' @importFrom dplyr `%>%` case_when left_join mutate select starts_with group_by across ungroup
#' @importFrom gt gt tab_header cols_label html fmt_number tab_spanner tab_footnote cells_column_labels
#' 
#' @export 
process_crop_prod <- function(
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
) {

    temp_crops_var <- rlang::sym(temp_crops_var)
    perm_crops_var <- rlang::sym(perm_crops_var)
    processed_var <- rlang::sym(processed_var)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    sold_var <- rlang::sym(sold_var)
    amt_sold_vars_txt <- amt_sold_vars

    sales_miss_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt}))"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue(
            "(haven::is_tagged_na({amt_sold_vars_txt[1]}))",
            "(haven::is_tagged_na({amt_sold_vars_txt[2]}))", 
            .sep = " | "
        )
    )
    sales_miss_quo <- rlang::parse_quo(sales_miss_expr_txt[1], env = rlang::global_env())
    
    sales_dk_expr_txt <- dplyr::case_when(
        length(amt_sold_vars_txt) == 1 ~ glue::glue(
            "({amt_sold_vars_txt} == {amt_sold_dk_val})"
        ),
        length(amt_sold_vars_txt) > 1 ~ glue::glue(
            "({amt_sold_vars_txt[1]} == {amt_sold_dk_val})",
            "({amt_sold_vars_txt[2]} == {amt_sold_dk_val})", 
            .sep = " | "
        )
    )
    sales_dk_quo <- rlang::parse_quo(sales_dk_expr_txt[1], env = rlang::global_env())  

    # compute statistics
    hhold_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(
            produced = (!!temp_crops_var == 1) | (!!perm_crops_var == 1),
            processed = !!processed_var == processed_val,
            n_products = rowSums(
                dplyr::select(., dplyr::starts_with(paste0(products_var, "__"))), 
                na.rm = TRUE
            )
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(.data$produced, .data$processed, .data$n_products),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup()

    processing_stats <- cases %>%
        dplyr::left_join(product_df, by = "interview__id") %>%
        dplyr::mutate(
            n_sold = !!sold_var == sold_val,
            sales_miss = !!sales_miss_quo,
            sales_dk = !!sales_dk_quo
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            any_sales = any(.data$n_sold, na.rm = TRUE),
            dplyr::across(
                .cols = c(.data$n_sold, .data$sales_miss, .data$sales_dk),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup()

# dsets <- list(hhold_stats, processing_stats)
# return(dsets)

    stats <- hhold_stats %>%
        dplyr::left_join(processing_stats, by = group_var_txt) %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$sales_miss, .data$sales_dk),
                .fns = ~ .x / .data$n_sold
            )
        ) %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$any_sales, .data$processed, .data$n_products),
                .fns = ~ .x / .data$produced
            )
        ) %>%
        replace_nan() %>%
        dplyr::select(
            !!group_var, .data$produced, .data$processed, .data$n_products, 
            .data$any_sales, .data$sales_miss, .data$sales_dk
        )

    # compose display table
    stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Processing of crop production") %>%
    gt::cols_label(
        produced = gt::html("Produce crops<br>(N)"),
        processed = gt::html("Process<br>(%)"),
        n_products = gt::html("Items produced<br>(AVG)"),
        any_sales = gt::html("Sell<br>(%)"),
        sales_miss = "Missing",
        sales_dk = "DK"
    ) %>%
    gt::fmt_number(
        columns = c(.data$processed, .data$any_sales, .data$sales_miss, .data$sales_dk),
        decimals = 1,
        scale_by = 100
    ) %>%  
    gt::tab_spanner(
        columns = c(.data$sales_miss, .data$sales_dk),
        label = "Sales values (%)"
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_labels(columns = .data$processed),
        footnote = "Percentage of those that produce"
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_labels(columns = .data$n_products),
        footnote = "Average number for those that process"
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_labels(columns = .data$n_products),
        footnote = "Percentage of those that process with any sales"
    ) %>%
    style_table()

}

#' Create crop labor table
#' 
#' @param hhold_df Data frame. Household-level variables.
#' @param cases Data frame. Case to include in analysis.
#' @param grew_crops_var Atomic character vector. Name of variable that indicates whether not grew crops.
#' @param grew_crops_val Atomic numeric vector. Value of `grew_crops_var` to indicate crops are grown.
#' @param paid_var Atomic character vector. Name of variable for categories of paid crop labor, as it appears in Designer.
#' @param free_var Atomic character vector. Name of variable for categories of free/exchange crop labor, as it appears in Designer.
#' @param members_df Data frame. Household member-level data.
#' @param member_worked_var Atomic character vector. Name of variable that indicates whether member worked to grow crops.
#' @param member_worked_val Atomic numeric vector. Value of `member_worked_var` indicating that the member worked.
#' @param group_var Atomic character vector. Name of the grouping variable
#' 
#' @importFrom rlang sym .data
#' @importFrom dplyr `%>%` left_join mutate starts_with if_else group_by summarise first across ungroup select
#' @importFrom gt gt tab_header cols_label html tab_spanner fmt_number tab_footnote cells_column_spanners
#' 
#' @export 
crop_labor <- function(
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
) {

    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    grew_crops_var <- rlang::sym(grew_crops_var)

    member_worked_var <- rlang::sym(member_worked_var)

    # compute statistics
    hhold_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(
            grew_crops = !!grew_crops_var == grew_crops_val,
            paid_labor = rowSums(dplyr::select(., dplyr::starts_with(paste0(paid_var, "__")))),
            free_labor = rowSums(dplyr::select(., dplyr::starts_with(paste0(free_var, "__")))),
            dplyr::across(
                .cols = c(.data$paid_labor, .data$free_labor),
                .fns = ~ dplyr::if_else(.x >= 1, 1, 0, 0)
            )
        ) %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(
            !!group_var := dplyr::first(!!group_var),
            dplyr::across(
                .cols = c(.data$grew_crops, .data$paid_labor, .data$free_labor),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup()

    hhold_labor_stats <- cases %>%
        dplyr::left_join(members_df, by = "interview__id") %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(
            hhold_labor = any(!!member_worked_var == member_worked_val)
        ) %>%
        dplyr::ungroup()

    stats <- hhold_stats %>%
        dplyr::left_join(hhold_labor_stats, by = "interview__id") %>%
        dplyr::mutate(
            hhold_only = (.data$hhold_labor == 1 & .data$paid_labor == 0 & .data$free_labor == 0),
            external_only = (.data$hhold_labor == 0 & (.data$paid_labor == 1 | .data$free_labor == 1)),
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = tidyselect::where(~ is.numeric(.x) | is.logical(.x)),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(
                    .data$hhold_only, .data$external_only, 
                    .data$hhold_labor, .data$paid_labor, .data$free_labor
                ),
                .fns = ~ .x / .data$grew_crops
            )
        ) %>%
        replace_nan() %>%
        dplyr::select(
            !!group_var, .data$grew_crops, .data$hhold_only, .data$external_only, 
            .data$hhold_labor, .data$paid_labor, .data$free_labor
        )

    # compose display table
    stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Crop labor inputs") %>%
    gt::cols_label(
        grew_crops = gt::html("Grew crops<br>(N)"),
        hhold_only = gt::html("Household labor only<br>(%)"),
        external_only = gt::html("External labor only<br>(%)"),
        hhold_labor = gt::html("Household"),
        paid_labor = gt::html("Paid"),
        free_labor = gt::html("Free/exchange")
    ) %>%
    gt::tab_spanner(
        columns = c(.data$hhold_labor, .data$paid_labor, .data$free_labor),
        label = "Labor types (%)",
        id = "sources"
    ) %>%
    gt::fmt_number(
        columns = c(
            .data$hhold_only, .data$external_only, 
            .data$hhold_labor, .data$paid_labor, .data$free_labor
        ),
        decimals = 1,
        scale_by = 100
    ) %>%      
    gt::tab_footnote(
        locations = gt::cells_column_spanners(spanners = "sources"),
        footnote = "Percentages may not sum to 100.. Households may have multiple sources of crop labor."
    ) %>%
    style_table()

}

# TODO globally: consider replacing `!!group_var` with `.data[["group_var"]]`; maybe similar approach for some other vars

# TODO: modify livestock tables to include a livestock ownership y/n question for ILP that does not exist in CORE-AG
# TODO globally: merge by `c("interview__id", "interview__key")` rather than just by `interview__id`

#' Create livestock labor table
#' 
#' @param hhold_df Data frame. Household-level data.
#' @param cases Data frame. Cases to include in analysis.
#' @param have_anim_var Character. Indicator variable: whether raise livestock.
#' @param have_anim_val Numeric. Indicator value: raise livestock
#' @param anim_labor_df Data frame. Roster of livestock labor.
#' @param anim_labor_id_var Character. Labor ID variable in livestock labor roster.
#' @param anim_labor_var Character. Number of workers variable.
#' @param anim_labor_none_val Numeric. Value indicatoing no workers.
#' @param hhold_labor_vals Numeric vector. Codes for household labor categories.
#' @param free_labor_val Numeric. Value of free labor.
#' @param paid_labor_val Numeric. Value of paid labor.
#' @param group_var Character. Name of group variable.
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym .data
#' @importFrom dplyr `%>%` left_join mutate group_by summarise across ungroup select
#' @importFrom gt gt tab_header cols_label html tab_spanner fmt_number tab_footnote cells_column_spanners
#' 
#' @export 
livestock_labor_tbl <- function(
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
) {

    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    have_anim_var <- rlang::sym(have_anim_var)
    anim_labor_id_var <- rlang::sym(anim_labor_id_var)
    anim_labor_var <- rlang::sym(anim_labor_var)

    # compute statistics
    hhold_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(have_anim = !!have_anim_var == have_anim_val)

    labor_stats <- cases %>%
        dplyr::left_join(anim_labor_df, by = "interview__id") %>%
        dplyr::mutate(
            hhold_labor = (!!anim_labor_id_var %in% hhold_labor_vals) & (!!anim_labor_var > anim_labor_none_val),
            free_labor = (!!anim_labor_id_var == free_labor_val) & (!!anim_labor_var > anim_labor_none_val),
            paid_labor = (!!anim_labor_id_var == paid_labor_val) & (!!anim_labor_var > anim_labor_none_val),
            n_labor = (.data$hhold_labor == 1 | .data$free_labor == 1 | .data$paid_labor == 1)
        ) %>%
        # summarize by hhold to get a summary measure of hhold labor
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(
            # !!group_var := dplyr::first(!!group_var),
            hhold_labor = any(.data$hhold_labor == 1, na.rm = TRUE),
            dplyr::across(
                .cols = c(.data$n_labor, .data$free_labor, .data$paid_labor),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup()

# TODO fairly globally: merge by c("interview__id", "interview__key", group_var_txt), 
# since those are the keys shared between from hhold and roster levels
    stats <- hhold_stats %>%
        dplyr::left_join(
            labor_stats, 
            by = "interview__id"
        ) %>%
        dplyr::mutate(
            hhold_only = (.data$hhold_labor == 1 & .data$free_labor == 0 & .data$paid_labor == 0),
            external_only = (.data$hhold_labor == 0 & (.data$free_labor == 1 | .data$paid_labor == 1))
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$have_anim, .data$n_labor, .data$hhold_only, .data$external_only, 
                    .data$hhold_labor, .data$free_labor, .data$paid_labor
                ),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$hhold_only, .data$external_only),
                .fns = ~ .x / .data$have_anim
            ),
            dplyr::across(
                .cols = c(.data$hhold_labor, .data$free_labor, .data$paid_labor),
                .fns = ~ .x / .data$n_labor
            )
        ) %>%
        dplyr::select(-.data$n_labor)

    # compose display table
    stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Livestock labor") %>%
    gt::cols_label(
        have_anim = gt::html("Own livestock<br>(N)"),
        hhold_only = gt::html("Household labor only<br>(%)"),
        external_only = gt::html("External labor only<br>(%)"),
        hhold_labor = gt::html("Household"),
        paid_labor = gt::html("Paid"),
        free_labor = gt::html("Free/exchange")
    ) %>%
    gt::tab_spanner(
        columns = c(.data$hhold_labor, .data$paid_labor, .data$free_labor),
        label = "Labor types (%)",
        id = "sources"
    ) %>%
    gt::fmt_number(
        columns = c(
            .data$hhold_only, .data$external_only, 
            .data$hhold_labor, .data$paid_labor, .data$free_labor
        ),
        decimals = 1,
        scale_by = 100
    ) %>%      
    gt::tab_footnote(
        locations = gt::cells_column_spanners(spanners = "sources"),
        footnote = "Percentages may not sum to 100.. Households may have multiple sources of crop labor."
    ) %>%
    style_table()    

}

# TODO: once labor tables finalized, consider putting repeated code in table_utils.R

# TODO: flag inconsistency between structure of livestock and fishery, aqua, and forestry labor. The former is fixed roster; the latter are multi-select triggered

# TODO: create fake data for labor

#' Create table for sector labor
#' 
#' @param hhold_df Data frame. Household-level data.
#' @param produce_var Character. Indicator variable: whether engaged in sector.
#' @param produce_val Numeric. Value: engaged in sector.
#' @param cases Data frame. Cases to include in table computations.
#' @param labor_var Character. Name of labor categories used. Name of multi-select question as in Designer.
#' @param hhold_labor_vals Numeric vector. Codes of answers corresponding to household labor.
#' @param free_labor_val Numeric. Code of free labor.
#' @param paid_labor_val Numeric. Code of paid labor.
#' @param group_var Character. Name of group variable.
#' @param table_title Character. Title of table.
#' @param sector_label Character. Label of first column that counts households engaged in sector.
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym parse_quo global_env .data
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr `%>%` left_join mutate group_by summarise across ungroup
#' @importFrom gt gt tab_header cols_label html tab_spanner fmt_number tab_footnote cells_column_spanners
#' 
#' @export 
sector_labor <- function(
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
) {

    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)
    produce_var <- rlang::sym(produce_var)

    hhold_labor_expr <- rlang::parse_quo(
        glue::glue_collapse(
            glue::glue("{labor_var}__{hhold_labor_vals} == 1"), 
            sep = " | "
        ),
        env = rlang::global_env()
    )

    free_labor_expr <- rlang::parse_quo(
        glue::glue("{labor_var}__{free_labor_val} == 1"),
        env = rlang::global_env()
    )    

    paid_labor_expr <- rlang::parse_quo(
        glue::glue("{labor_var}__{paid_labor_val} == 1"),
        env = rlang::global_env()
    )

    # compute statistics
    stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(., 
            produce = !!produce_var == produce_val,
            hhold_labor = !!hhold_labor_expr,
            free_labor = !!free_labor_expr,
            paid_labor = !!paid_labor_expr,
            hhold_only = .data$hhold_labor == 1 & .data$free_labor == 0 & .data$paid_labor == 0,
            external_only = .data$hhold_labor == 0 & (.data$free_labor == 1 | .data$paid_labor == 1)
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$produce, .data$hhold_only, .data$external_only, 
                    .data$hhold_labor, .data$free_labor, .data$paid_labor
                ),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(
                    .data$hhold_only, .data$external_only, 
                    .data$hhold_labor, .data$free_labor, .data$paid_labor
                ),
                .fns = ~ .x / .data$produce
            )
        )

    # compose display table
    produce_lbl <- paste0(sector_label, "<br>(N)")

    stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = table_title) %>%
    gt::cols_label(
        produce = gt::html(produce_lbl),
        hhold_only = gt::html("Household labor only<br>(%)"),
        external_only = gt::html("External labor only<br>(%)"),
        hhold_labor = gt::html("Household"),
        paid_labor = gt::html("Paid"),
        free_labor = gt::html("Free/exchange")
    ) %>%
    gt::tab_spanner(
        columns = c(.data$hhold_labor, .data$paid_labor, .data$free_labor),
        label = "Labor types (%)",
        id = "sources"
    ) %>%
    gt::fmt_number(
        columns = c(
            .data$hhold_only, .data$external_only, 
            .data$hhold_labor, .data$paid_labor, .data$free_labor
        ),
        decimals = 1,
        scale_by = 100
    ) %>%      
    gt::tab_footnote(
        locations = gt::cells_column_spanners(spanners = "sources"),
        footnote = "Percentages may not sum to 100. Households may have multiple sources of labor inputs."
    ) %>%
    style_table()    
        
}

#' Create table of income sources
#' 
#' @param hhold_df Data frame. Household-level data.
#' @param crop_var Character. Indicator variable: grow crops.
#' @param livestock_var Character. Indicator variable: raise livestock.
#' @param temp_crop_df Data frame. Temporary crop disposition data frame.
#' @param temp_crop_var Character. Indicator variable: sell temporary crops.
#' @param temp_crop_val Numeric. Value: sell crops.
#' @param perm_crop_df Data frame. Permanent crop disposition data frame.
#' @param perm_crop_var Character. Indicator variable: sell permanent crops.
#' @param perm_crop_val Numeric. Value: sell crops.
#' @param processed_df Data frame. Processed crops.
#' @param processed_var Character. Indicator variable: sell processed crops.
#' @param processed_val Numeric. Value: sell processed crop.
#' @param anim_df Data frame. Livestock data frame.
#' @param sold_live_anim_var Character. Indicator variable: sold live animals.
#' @param slaughter_anim_var Character. Indicator variable: sold slaughtered animal.
#' @param slaughter_anim_val Numeric. Value: sold slaughtered animal.
#' @param sold_live_poultry_var Character. Indicator variable: sold live poultry.
#' @param slaughter_poultry_var Character. Indicator variable: sold slaughtered poultry.
#' @param slaughter_poultry_val Numeric. Value: sold slaughtered poultry.
#' @param milk_var Character. Indicator variable: sold milk.
#' @param milk_val Numeric. Value: sold milk.
#' @param eggs_var Character. Indicator variable: sold eggs.
#' @param eggs_val Numeric. Value: sold eggs.
#' @param oth_anim_prod_df Data frame. Roster of other animal products.
#' @param oth_anim_var Character. Indicator variable: sold other animal products.
#' @param oth_anim_val Numeric. Value: sold other animal products.
#' @param cases Data frame. Cases to include in table analysis.
#' @param group_var Character. Name of group variable.
#' 
#' @inherit temp_crop_harvest return
#' 
#' @importFrom rlang sym .data
#' @importFrom dplyr `%>%` left_join mutate group_by mutate summarise ungroup across
#' @importFrom gt gt tab_header cols_label tab_spanner tab_footnote cells_column_spanners fmt_number
#' 
#' @export 
income_sources <- function(
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
) {


    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    crop_var <- rlang::sym(crop_var)
    livestock_var <- rlang::sym(livestock_var)
    temp_crop_var <- rlang::sym(temp_crop_var)
    perm_crop_var <- rlang::sym(perm_crop_var)
    processed_var <- rlang::sym(processed_var)

    # livestock variables
    sold_live_anim_var <- rlang::sym(sold_live_anim_var)
    slaughter_anim_var <- rlang::sym(slaughter_anim_var)
    sold_live_poultry_var <- rlang::sym(sold_live_poultry_var)
    slaughter_poultry_var <- rlang::sym(slaughter_poultry_var)
    milk_var <- rlang::sym(milk_var)
    eggs_var <- rlang::sym(eggs_var)
    oth_anim_var <- rlang::sym(oth_anim_var)

    # compute statistics
    hhold_stats <- cases %>%
        dplyr::left_join(hhold_df, by = "interview__id") %>%
        dplyr::mutate(
            crops = !!crop_var == 1,
            livestock = !!livestock_var == 1
        )

    temp_crop_stats <- cases %>%
        dplyr::left_join(temp_crop_df, by = "interview__id") %>%
        dplyr::mutate(temp_crop = !!temp_crop_var == temp_crop_val) %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(temp_crop = sum(.data$temp_crop, na.rm = TRUE)) %>%
        dplyr::ungroup()

    perm_crop_stats <- cases %>%
        dplyr::left_join(perm_crop_df, by = "interview__id") %>%
        dplyr::mutate(perm_crop = !!perm_crop_var == perm_crop_val) %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(perm_crop = sum(.data$perm_crop, na.rm = TRUE)) %>%
        dplyr::ungroup()

    processed_stats <- cases %>%
        dplyr::left_join(processed_df, by = "interview__id") %>%
        dplyr::mutate(processed = !!processed_var == processed_val) %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(processed = sum(.data$processed, na.rm = TRUE)) %>%
        dplyr::ungroup()        

    livestock_stats <- cases %>%
        dplyr::left_join(anim_df, by = "interview__id") %>%
        dplyr::mutate(
            sold_anim_alive = !!sold_live_anim_var > 0,
            sold_anim_killed = !!slaughter_anim_var == slaughter_anim_val,
            sold_poultry_alive = !!sold_live_poultry_var > 0,
            sold_poultry_killed = !!slaughter_poultry_var == slaughter_poultry_val,
            milk_sold = !!milk_var == milk_val,
            eggs_sold = !!eggs_var == eggs_val
        ) %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$sold_anim_alive, .data$sold_anim_killed, 
                    .data$sold_poultry_alive, .data$sold_poultry_killed,
                    .data$milk_sold, .data$eggs_sold
                ),
                .fns = ~ any(.x == 1, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup()

    oth_anim_prod_stats <- cases %>%
        dplyr::left_join(oth_anim_prod_df, by = "interview__id") %>%
        dplyr::mutate(anim_prod_sold = !!oth_anim_var == oth_anim_val) %>%
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarise(anim_prod_sold = any(.data$anim_prod_sold == 1, na.rm = TRUE)) %>%
        dplyr::ungroup()

    stats <- cases %>% 
        dplyr::left_join(hhold_stats, by = c("interview__id", "team")) %>%
        dplyr::left_join(temp_crop_stats, by = "interview__id") %>%
        dplyr::left_join(perm_crop_stats, by = "interview__id") %>%
        dplyr::left_join(processed_stats, by = "interview__id") %>%
        dplyr::left_join(livestock_stats, by = "interview__id") %>%
        dplyr::left_join(oth_anim_prod_stats, by = "interview__id") %>%
        dplyr::mutate(
            n_hholds = 1,
            n_any_ag = (.data$crops == 1 | .data$livestock == 1),
            anim_sold = (.data$sold_anim_alive == 1 | .data$sold_poultry_alive == 1),
            anim_killed = (.data$sold_anim_killed == 1 | .data$sold_poultry_killed == 1)
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            dplyr::across(
                .cols = c(
                    .data$n_hholds, .data$n_any_ag,
                    .data$temp_crop, .data$perm_crop, .data$processed, 
                    .data$anim_sold, .data$anim_killed, 
                    .data$milk_sold, .data$eggs_sold, .data$anim_prod_sold
                ), 
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(
                    .data$temp_crop, .data$perm_crop, .data$processed, 
                    .data$anim_sold, .data$anim_killed, 
                    .data$milk_sold, .data$eggs_sold, .data$anim_prod_sold
                ), 
                .fns = ~ .x / .data$n_any_ag
            )
        ) %>%
        replace_nan()

    # compose display table
    stats %>%
    gt::gt(rowname_col = group_var_txt) %>%
    gt::tab_header(title = "Sources of income") %>%
    gt::cols_label(
        n_hholds = gt::html("Total households<br>(N)"),
        n_any_ag = gt::html("Practice agriculture<br>(N)"),
        temp_crop = gt::html("Temporary crop"),
        perm_crop = gt::html("Tree/permanent crop"),
        processed = gt::html("Processed crop"),
        anim_sold = gt::html("Live animal"),
        anim_killed = gt::html("Slaughter"),
        milk_sold = gt::html("Milk"),
        eggs_sold = gt::html("Eggs"),
        anim_prod_sold = gt::html("Other animal products")
    ) %>%
    gt::tab_spanner(
        columns = c(.data$temp_crop, .data$perm_crop, .data$processed),
        label = "Crop sales (%)",
        id = "crops"
    ) %>%
    gt::tab_spanner(
        columns = c(
            .data$anim_sold, .data$anim_killed, 
            .data$milk_sold, .data$eggs_sold, .data$anim_prod_sold
        ),
        label = "Livestock sales (%)",
        id = "livestock"
    ) %>%
    gt::tab_footnote(
        locations = gt::cells_column_spanners(spanners = c("crops", "livestock")),
        footnote = "Percentage computed as (number with sales / number of households engaged in any agriculture)"
    ) %>%
    gt::fmt_number(
        columns = c(
            .data$temp_crop, .data$perm_crop, .data$processed, 
            .data$anim_sold, .data$anim_killed, .data$milk_sold, .data$eggs_sold, 
            .data$anim_prod_sold            
        ),
        decimals = 1,
        scale_by = 100
    ) %>%       
    style_table()

}
