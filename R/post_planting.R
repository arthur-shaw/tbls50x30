
#' Make parcels per household table
#' 
#' @param parcel_df Data frame of parcels.
#' @param cases Data frame of cases to include in analysis
#' @param use_var Character. Parcel use variable
#' @param use_vals Numeric vector. Values that correspond to an agricultural use
#' @param group_var Character. Grouping variable
#' 
#' @return {gt} table object
#' 
#' @importFrom rlang sym `!!` .data
#' @importFrom dplyr `%>%` left_join mutate if_else group_by summarise n_distinct n ungroup across select
#' 
#' @export
parcels_per_hhold <- function(
    parcel_df, # parcels df
    cases, # either df or character vector of interview__id for cases to include
    use_var = NULL,
    use_vals = NULL,
    group_var
) {

    # make group variable into symbol for later evaluation
    group_var <- rlang::sym(group_var)

    # construct a quosure that describes agricultural parcels
    if (!is.null(use_var) & !is.null(use_vals)) {
        parcel_quo <- make_multi_select_quos(
            varname = use_var,
            vals = use_vals
        )
    }

    # compute parcel statistics
    parcel_stats <- cases %>%
        dplyr::left_join(parcel_df, by = "interview__id") %>%
        {
            if (!is.null(use_var)) {
                dplyr::mutate(., 
                    ag = dplyr::if_else(
                        condition = !!parcel_quo, 
                        true = 1, 
                        false = 0, 
                        missing = 0
                    ),
                    non_ag = dplyr::if_else(.data$ag == 0, 1, 0, 0)
                )
            } else {
                .
            }

        } %>%
        dplyr::group_by(!!group_var) %>%
        {
            if (!is.null(use_var)) {
                dplyr::summarise(., 
                    interviews = dplyr::n_distinct(.data$interview__id, na.rm = TRUE),
                    parcels = dplyr::n(),
                    ag = sum(.data$ag, na.rm = TRUE),
                    non_ag = sum(.data$non_ag, na.rm = TRUE)
                )       
            } else {
                dplyr::summarise(., 
                    interviews = n_distinct(.data$interview__id, na.rm = TRUE),
                    parcels = dplyr::n()
                )
            }
        } %>%
        ungroup() %>%
        {
            if (!is.null(use_var)) {
                dplyr::mutate(., 
                    dplyr::across(
                        .cols = c(.data$ag, .data$non_ag),
                        .fns = ~ .x / .data$parcels
                    )
                )
            } else {
                .
            }
        } %>%
        # replace_nan() %>%
        {
            if (!is.null(use_var)) {
                dplyr::select(., !!group_var, .data$interviews, .data$parcels, .data$ag, .data$non_ag)
            } else {
                dplyr::select(., !!group_var, .data$interviews, .data$parcels)
            }
        }

    # create table
    parcel_stats %>%
    gt::gt(rowname_col = rlang::as_name(group_var)) %>%
    gt::tab_header(title = "Parcels per household, by team") %>%
    gt::cols_label(
        interviews = gt::html("Interviews<br>(N)"),
        parcels = gt::html("Total parcels<br>(N)")
    ) %>%
    {
        if (!is.null(use_var)) {
            gt::cols_label(.,
                ag = gt::html("Agricultural"),
                non_ag = gt::html("Non-agricultural")
            ) %>%
            gt::tab_spanner(
                label = "Parcels (%)",
                columns = c(.data$ag, .data$non_ag)
            ) %>%
            gt::fmt_number(
                columns = c(.data$ag, .data$non_ag),
                decimals = 1,
                scale_by = 100
            )
        } else {
            .
        }
    } %>%
    style_table()

}

#' Make parcel GPS measurement table
#' 
#' @param parcel_df Data frame of parcels.
#' @param cases Data frame of cases to include in analysis
#' @param gps_var Character. Name of column that includes GPS-measured area.
#' @param not_measured_val Numeric. Special value that indicates area was not measured.
#' @param why_no_gps_var Character. Name of column that captures reason why no GPS measurement was done.
#' @param group_var Character. Name of grouping variable column (e.g., team, region, etc.).
#' 
#' @inherit parcels_per_hhold return
#' 
#' @importFrom rlang sym `!!` .data list2 as_name
#' @importFrom dplyr `%>%` left_join mutate if_else group_by summarise n across ungroup starts_with matches
#' @importFrom haven is_tagged_na
#' @importFrom gt gt html tab_header cols_label tab_spanner fmt_number
#' 
#' @export 
parcel_gps <- function(
    parcel_df,
    cases,
    gps_var,
    not_measured_val = -9999,
    why_no_gps_var = NULL,
    group_var
) {

    # TODO: add mechanism for filtering to ag parcels or do outside this function

    # set variables to symbols for later evaluation
    gps_var <- rlang::sym(gps_var)
    if (!is.null(why_no_gps_var)) {
        why_no_gps_var_txt <- why_no_gps_var
        why_no_gps_var <- rlang::sym(why_no_gps_var)
    }
    group_var <- rlang::sym(group_var)

    # compute GPS measurement stats
    parcel_gps_stats <- cases %>%
        left_join(parcel_df, by = "interview__id") %>%
        dplyr::mutate(
            not_measured = dplyr::if_else(
                condition = (
                    # case 1: interviewer records not-recorded value
                    (!!gps_var == .data$not_measured_val) | 
                    # case 2: SuSo numeric missing value
                    # TODO: check whether this case is needed
                    (!!gps_var == -999999999) | 
                    # case 3: SuSo's extended missing value for Stata
                    haven::is_tagged_na(!!gps_var)
                ),
                true = 1,
                false = 0,
                missing = 0
            )
        ) %>%
        { 
            if (is.null(why_no_gps_var)) {
                .
            } else {
                create_dummies(df = ., var = !!why_no_gps_var)
            }
        } %>%
        group_by(!!group_var) %>%
        {
            if (is.null(why_no_gps_var)) {
                dplyr::summarise(.,
                    parcels = dplyr::n(),
                    not_measured = sum(.data$not_measured, na.rm = TRUE)
                )
            } else {
                dplyr::summarise(.,
                    parcels = dplyr::n(),
                    not_measured = sum(.data$not_measured, na.rm = TRUE),
                    dplyr::across(
                        .cols = dplyr::starts_with(paste0(why_no_gps_var_txt, "_")),
                        .fns = ~ sum(.x, na.rm = TRUE)
                    )                    
                )
            }
        } %>%
        dplyr::ungroup() %>%
        {
            if (is.null(why_no_gps_var)) {
                .
            } else {
                dplyr::mutate(.,
                    dplyr::across(
                        .cols = starts_with(paste0(why_no_gps_var_txt, "_")),
                        .fns = ~ .x / .data$not_measured
                    )
                )            
            }
        } %>%
        replace_nan()

    # compose labels of table columns
    col_labels <- rlang::list2(
        parcels = gt::html("Parcels<br>(N)"),
        not_measured = gt::html("Not measured<br>(N)")
    )
    # extract column labels
    if (!is.null(why_no_gps_var)) {

        lbls <- extract_var_labels(df = parcel_df, var = !!why_no_gps_var)
        vals <- extract_var_values(df = parcel_df , var = !!why_no_gps_var)
        col_labels2 <- create_col_labels(
            labels = lbls,
            values = vals,
            df = parcel_gps_stats,
            pattern = paste0(why_no_gps_var_txt, "_")
        )

        col_labels <- c(col_labels, col_labels2)

    }

    # compose table
    parcel_gps_stats %>%
    gt::gt(rowname_col = rlang::as_name(group_var)) %>%
    gt::tab_header(title = "Parcel GPS area measurement, by team") %>%
    gt::cols_label(.list = col_labels) %>%
    {
        if (!is.null(why_no_gps_var)) {
            gt::tab_spanner(.,
                label = "Why not measured (%)",
                columns = dplyr::starts_with(paste0(why_no_gps_var_txt, "_"))
            )
        } else {
            .
        }
    }  %>%
    {
        if (!is.null(why_no_gps_var)) {
            gt::fmt_number(.,
                columns = dplyr::matches(paste0(why_no_gps_var_txt, "_")),
                decimals = 1,
                scale_by = 100
            )
        } else {
            .
        }
    } %>%
    style_table()

}

#' Make plots per parcel table
#' 
#' @param parcel_plot_df Data frame of parcel-plots.
#' @param cases Data frame of cases to include in analysis.
#' @param parcel_id_var Character. Name of parcel ID column.
#' @param group_var Character. Name of grouping variable column (e.g., team, region, etc.).
#' 
#' @inherit parcel_gps return
#' 
#' @importFrom rlang sym `!!` as_name .data
#' @importFrom dplyr `%>%` left_join group_by summarise n_distinct n ungroup mutate
#' @importFrom gt gt tab_header cols_label html fmt_number
#' 
#' @export 
plots_per_parcel <- function(
    parcel_plot_df,
    cases,
    parcel_id_var,
    group_var
) {

    # make group variable into symbol for later evaluation
    parcel_id_var <- rlang::sym(parcel_id_var)
    group_var <- rlang::sym(group_var)

    # compute parcel-plot statistics
    parcel_plot_stats <- cases %>%
        dplyr::left_join(parcel_plot_df, by = "interview__id") %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            n_parcels = dplyr::n_distinct(.data$interview__id, !!parcel_id_var, na.rm = TRUE),
            n_plots = dplyr::n()
        ) %>%
        ungroup() %>%
        mutate(mean_plots = .data$n_plots / .data$n_parcels) %>%
        replace_nan()

    # compose table
    parcel_plot_stats %>%
    gt::gt(rowname_col = rlang::as_name(group_var)) %>%
    gt::tab_header(title = "Number of plots per parcel, by team") %>%
    gt::cols_label(
        n_parcels = gt::html("Parcels<br>(N)"),
        n_plots = gt::html("Plots<br>(N)"),
        mean_plots = gt::html("Plots per parcel<br>(AVG)")
    ) %>%
    gt::fmt_number(
        columns = c(.data$mean_plots),
        decimals = 1
    ) %>%
    style_table()

}

#' Make plot use table
#' 
#' @param plot_df Data frame of plots.
#' @param cases Data frame of cases to include in analysis.
#' @param plot_use_var Character. Name of plot use column.
#' @param group_var Name of grouping variable column (e.g., team, region, etc.).
#' 
#' @inherit plots_per_parcel return
#' 
#' @importFrom rlang sym `!!` list2 as_name .data
#' @importFrom dplyr `%>%` left_join group_by summarise n across starts_with ungroup mutate matches
#' @importFrom gt gt html tab_header cols_label tab_spanner fmt_number
#' 
#' @export 
plot_use <- function(
    plot_df,
    cases,
    plot_use_var,
    group_var
) {

    # make group variable into symbol for later evaluation
    plot_use_var_txt <- plot_use_var
    plot_use_var <- rlang::sym(plot_use_var)
    group_var_txt <- group_var
    group_var <- rlang::sym(group_var)

    plot_use_stats <- cases %>%
        dplyr::left_join(plot_df, by = "interview__id") %>%
        create_dummies(var = !!plot_use_var) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            plots = dplyr::n(),
            dplyr::across(
                .cols = starts_with(paste0(plot_use_var_txt, "_")),
                .fns = ~ sum(.x, na.rm = TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = starts_with(paste0(plot_use_var_txt, "_")),
                .fns = ~ .x / .data$plots
            )
        ) %>%
        replace_nan()        

    # compose table

    # compile column labels
    # ... for fixed-name vars
    col_lbls1 <- rlang::list2(
        plots = gt::html("Plots<br>(N)")
    )

    # ... from value labels
    lbls <- extract_var_labels(df = plot_df, var = !!plot_use_var)
    vals <- extract_var_values(df = plot_df , var = !!plot_use_var)
    col_labels2 <- create_col_labels(
        labels = lbls,
        values = vals,
        df = plot_use_stats,
        pattern = paste0(plot_use_var_txt, "_")
    )

    # combine labels
    col_labels <- c(col_lbls1, col_labels2)

    # create display table
    plot_use_stats %>%
    gt::gt(rowname_col = rlang::as_name(group_var)) %>%
    gt::tab_header(title = "Plot use, by team") %>%
    gt::cols_label(.list = col_labels) %>%
    gt::tab_spanner(
        columns = matches(paste0(plot_use_var_txt, "_")),
        label = "Use (%)"
    ) %>%
    gt::fmt_number(
        columns = matches(paste0(plot_use_var_txt, "_")),
        decimals = 1,
        scale_by = 100
    ) %>%
    style_table()

}

#' Make plot GPS measurement table
#' 
#' @param plot_df Data frame of plots.
#' @param cases Data frame of cases to include in analysis.
#' @param gps_var Character. Name of column that includes GPS-measured area.
#' @param not_measured_val Numeric. Special value that indicates area was not measured.
#' @param why_no_gps_var Character. Name of column that captures reason why no GPS measurement was done.
#' @param group_var Character. Name of grouping variable column (e.g., team, region, etc.).
#' 
#' @inherit parcel_gps return
#' 
#' @importFrom rlang sym `!!` list2 as_name .data
#' @importFrom dplyr `%>%` left_join mutate if_else group_by summarise n across starts_with ungroup matches
#' @importFrom haven is_tagged_na
#' @importFrom gt html gt tab_header cols_label tab_spanner fmt_number
#' 
#' @export 
plot_gps <- function(
    plot_df,
    cases,
    gps_var,
    not_measured_val = -9999,
    why_no_gps_var,
    group_var
) {

    # set variables to symbols for later evaluation
    gps_var <- rlang::sym(gps_var)
    why_no_gps_var_txt <- why_no_gps_var
    why_no_gps_var <- rlang::sym(why_no_gps_var)
    group_var <- rlang::sym(group_var)

    # compute GPS measurement stats
    plot_gps_stats <- cases %>%
        left_join(plot_df, by = "interview__id") %>%
        dplyr::mutate(
            not_measured = dplyr::if_else(
                condition = (
                    # case 1: interviewer records not-recorded value
                    (!!gps_var == not_measured_val) | 
                    # case 2: SuSo numeric missing value
                    # TODO: check whether this case is needed
                    (!!gps_var == -999999999) | 
                    # case 3: SuSo's extended missing value for Stata
                    haven::is_tagged_na(!!gps_var) |
                    # case 4: regular NA
                    is.na(!!gps_var)
                ),
                true = 1,
                false = 0,
                missing = 0
            )
        ) %>%
        create_dummies(df = ., var = !!why_no_gps_var) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            plots = dplyr::n(),
            not_measured = sum(.data$not_measured, na.rm = TRUE),
            dplyr::across(
                .cols = dplyr::starts_with(paste0(why_no_gps_var_txt, "_")),
                .fns = ~ sum(.x, na.rm = TRUE)
            )                    
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = starts_with(paste0(why_no_gps_var_txt, "_")),
                .fns = ~ .x / .data$not_measured
            )
        ) %>%
        replace_nan()

    # compose labels of table columns
    col_labels1 <- rlang::list2(
        plots = gt::html("Plots<br>(N)"),
        not_measured = gt::html("Not measured<br>(N)")
    )
    # extract column labels
    lbls <- extract_var_labels(df = plot_df, var = !!why_no_gps_var)
    vals <- extract_var_values(df = plot_df , var = !!why_no_gps_var)
    col_labels2 <- create_col_labels(
        labels = lbls,
        values = vals,
        df = plot_gps_stats,
        pattern = paste0(why_no_gps_var_txt, "_")
    )
    # combine column labels
    col_labels <- c(col_labels1, col_labels2)

    # compose table
    plot_gps_stats %>%
    gt::gt(rowname_col = rlang::as_name(group_var)) %>%
    gt::tab_header(title = "Plot GPS area measurement, by team") %>%
    gt::cols_label(.list = col_labels) %>%
    gt::tab_spanner(
        columns = dplyr::matches(paste0(why_no_gps_var_txt, "_")),
        label = "Why not measured (%)"
    ) %>%
    gt::fmt_number(
        columns = dplyr::matches(paste0(why_no_gps_var_txt, "_")),
        decimals = 1,
        scale_by = 100
    ) %>%
    style_table()

}

#' Make crops per plot table
#' 
#' @param crop_df Data frame of parcel-plot-crops
#' @param cases Data frame of cases to include in analysis.
#' @param parcel_id_var Character. Name of parcel ID column.
#' @param plot_id_var Character. Name of plot ID column.
#' @param group_var Character. Name of grouping variable column (e.g., team, region, etc.).
#' 
#' @inherit parcel_gps return
#' 
#' @importFrom rlang sym `!!` as_name .data
#' @importFrom dplyr `%>%` left_join group_by summarise n_distinct n ungroup mutate
#' @importFrom gt gt tab_header cols_label html fmt_number
#' 
#' @export 
crops_per_plot <- function(
    crop_df,
    cases,
    parcel_id_var,
    plot_id_var,
    group_var
) {

    # make group variable into symbol for later evaluation
    parcel_id_var <- rlang::sym(parcel_id_var)
    plot_id_var <- rlang::sym(plot_id_var)
    group_var <- rlang::sym(group_var)

    # compute crop stats
    crop_count_stats <- cases %>%
        dplyr::left_join(crop_df, by = "interview__id") %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            n_plots = dplyr::n_distinct(.data$interview__id, !!parcel_id_var, !!plot_id_var),
            n_crops = dplyr::n()
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(mean_crops = .data$n_crops / .data$n_plots) %>%
        replace_nan()

    # compose table
    crop_count_stats %>%
    gt::gt(rowname_col = rlang::as_name(group_var)) %>%
    gt::tab_header(title = "Number of crops per plot, by team") %>%
    gt::cols_label(
        n_plots = gt::html("Parcels<br>(N)"),
        n_crops = gt::html("Crops<br>(N)"),
        mean_crops = gt::html("Crops per plot<br>(AVG)")
    ) %>%
    gt::fmt_number(
        columns = c(.data$mean_crops),
        decimals = 1
    ) %>%
    style_table()  

}

#' Make crop types tables
#' 
#' @param crop_df Data frame of parcel-plot-crops
#' @param cases Data frame of cases to include in analysis.
#' @param crop_type_var Charcter. Name of crop type column.
#' @param temp_crop_val Numeric. Value of temporary crop option.
#' @param perm_crop_val Numeric. Value of permanent crop option.
#' @param group_var Character. Name of grouping variable column (e.g., team, region, etc.).
#' 
#' @inherit plot_gps return
#' 
#' @importFrom rlang sym `!!` as_name .data
#' @importFrom dplyr `%>%` left_join group_by mutate if_else group_by summarise n across ungroup starts_with
#' @importFrom gt gt tab_header cols_label html tab_spanner fmt_number
#' 
#' @export 
crop_types <- function(
    crop_df,
    cases,
    crop_type_var,
    temp_crop_val = 1,
    perm_crop_val = 2,
    group_var
) {

    # make group variable into symbol for later evaluation
    crop_type_var <- rlang::sym(crop_type_var)
    group_var <- rlang::sym(group_var)

    # compute crop stats
    crop_type_stats <- cases %>%
        dplyr::left_join(crop_df, by = "interview__id") %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::mutate(
            crop_temp = dplyr::if_else(!!crop_type_var == temp_crop_val, 1, 0, 0),
            crop_perm = dplyr::if_else(!!crop_type_var == perm_crop_val, 1, 0, 0)
        ) %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise(
            n_crops = dplyr::n(),
            dplyr::across(
                .cols = c(.data$crop_temp, .data$crop_perm),
                .fns = ~ sum(.x, na.rm =  TRUE)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            dplyr::across(
                .cols = c(.data$crop_temp, .data$crop_perm),
                .fns = ~ .x / .data$n_crops
            )
        ) %>%
        replace_nan()

    # compose table
    crop_type_stats %>%
    gt::gt(rowname_col = rlang::as_name(group_var)) %>%
    gt::tab_header(title = "Crop types, by team") %>%
    gt::cols_label(
        n_crops = gt::html("Crops<br>(N)"),
        crop_temp = "Temporary",
        crop_perm = "Permanent"
    ) %>%
    gt::tab_spanner(
        label = "Type (%)",
        columns = dplyr::starts_with("crop_")
    ) %>%
    gt::fmt_number(
        columns = dplyr::starts_with("crop_"),
        decimals = 1,
        scale_by = 100
    ) %>%
    style_table()

}

