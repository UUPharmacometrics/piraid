#' Get the initial estimate for a certain item and parameter
#' 
#' The initial estimate will be:
#' 
#' 1. The user defined value from initial_estimates_item_parameters
#' 2. Taken from the published model in the scale
#' 3. A fall back value of:
#'       Binary: DIS=1, DIF=0.1, GUE=0.01
#'       Ordered categorical: DIS=1, DIFn=-3 + 6n/#cats (not lower than 0.1)
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param parameter The name of a parameter
#' @return The initial estimate of the parameter for the item
#' @md
initial_estimate <- function(model, item, parameter) {
    par_row <- dplyr::filter(model$item_parameters, item==!!item$number, parameter==!!parameter)
    if (nrow(par_row) != 0 && !is.na(par_row$init)) {
        return(par_row$init)
    }
    parameter_index <- item_parameter_index(model, item$number, parameter)
    published <- published_init(model$scale, item$number, parameter_index)
    if (!is.null(published)) {
        return(published)
    }
    if (parameter == "DIS") {
        1
    } else if (parameter == "GUE") {
        0.01
    } else if (parameter == "DIF") {
        0.1
    } else {
        index <- as.numeric(stringr::str_extract(parameter, '\\d+'))
        if(index==1){
            init <- -2
        }else{
            init <- 4/(length(item$levels)-2)
        }
        return(init)
    }
}

#' Get vector of parameter names of an item
#' 
#' @param model An irt_model object
#' @param item_number An item number
#' @return A character vector of parameter names
#' @export
item_parameter_names <- function(model, item_number) {
    stopifnot(is.irt_model(model))
    scale <- model$scale
    item <- get_item(scale, item_number)
    if (item$type == item_type$ordered_categorical) {
        dif_names <- paste0("DIF", 1:(length(item$levels) - 1))
        c("DIS", dif_names)
    } else {    # Currently binary
        c("DIS", "DIF", "GUE")
    }
}

#' Get the index of an item parameter relative to the first parameter of that item
#' 
#' DIS would have index 1, DIF1 would have 2 etc
#' 
#' @param model An irt_model object
#' @param item_number Number of the item
#' @param parameter The name of the parameter
#' @return The index of the parameter  starting from 1 or NA if not found
item_parameter_index <- function(model, item_number, parameter) {
    names <- item_parameter_names(model, item_number)
    match(parameter, names)
}

#' Check if an item parameter is fixed
#' 
#' True should be returned if:
#' 
#' * init = 0
#' * The level corresponding to param was consolidated (in the upper end of the levels)
#' * fix was set using the fix_item_parameters function
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param parameter The item parameter name
#' @return A string containing "FIX" or an empty string
#' @md
is_item_parameter_fixed <- function(model, item, parameter) {
    init <- initial_estimate(model, item, parameter)

    if (init == 0) {
        return(TRUE)
    } else if (consolidated(model, item, item_parameter_index(model, item$number, parameter) - 1)) {
        return(TRUE)
    }
    par_row <- dplyr::filter(model$item_parameters, item==!!item$number, parameter==!!parameter)
    if (nrow(par_row) != 0 && !is.na(par_row$fix) && par_row$fix) {
        return(TRUE)
    }
    FALSE
}

item_categories_probability_labels <- function(model, item){
    lbls <- sprintf("P(Y=%i)", item$levels)
    names(lbls) <- item$levels
    return(lbls)
}

#' Give a theta init string from limits
#'
#' init=0 gives 0
#'  
#' @param init The initial estimate
#' @param lower The lower bound (not mandatory)
#' @param upper The upper bound (not mandatory)
#' @return A theta init string for NONMEM
#' @keywords internal
theta_init <- function(init, lower, upper) {
    if (init == 0) {
        return("0")
    }
    
    if (missing(upper) && missing(lower)) {
        init
    } else if (missing(upper)) {
        paste0("(", lower, ",", init, ")")
    } else {
        paste0("(", lower, ",", init, ",", upper, ")")
    }
}


#' Create initial estimate $THETA for an item parameter
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param parameter The name of the parameter
#' @param theta_num The number of the THETA
theta_string_item_parameter <- function(model, item, parameter, theta_num) {
    init <- theta_string_init_part(model, item, parameter)
    fix <- theta_string_fix_part(model, item, parameter)
    label <- theta_string_label_part(model, item, parameter, theta_num)
    paste0(init, fix, " ", label)
}

#' Create initial estimates $THETAs for item
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param start_theta The number of the first theta for this item
#' @return A vector of initial estimates definitions for NONMEM
#' @keywords internal
item_inits <- function(model, item, start_theta) {
    parameters <- item_parameter_names(model, item$number)
    theta_strings <- character()
    for (parameter in parameters) {
        par_row <- dplyr::filter(model$item_parameters, item==!!item$number, parameter==!!parameter)
        if (nrow(par_row) != 0 && !is.na(par_row$ignore) && par_row$ignore) {  # Item ignored: put placeholder
            item_ignored <- TRUE
        } else {
            item_ignored <- FALSE
        }
        if (!item_ignored) {
            theta_strings <- c(theta_strings, theta_string_item_parameter(model, item, parameter, start_theta))
        } else {
            theta_strings <- c(theta_strings, "0 FIX  ; THETA PLACEHOLDER")
        }
        start_theta <- start_theta + 1
    }
    theta_strings
}

#' Generate the init and limits part of the $THETA record for an item
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param parameter The item parameter name
#' @return A string containing the initial estimate and limits
theta_string_init_part <- function(model, item, parameter) {
    init <- initial_estimate(model, item, parameter)
    if (is_item_parameter_fixed(model, item, parameter)) {
        init
    } else if (parameter == "DIS") {
        theta_init(init, lower=0)
    } else if (item$type == item_type$binary && parameter == "DIF") {
        theta_init(init, lower=-50, upper=50)
    } else if (startsWith(parameter, "DIF")) {
        index <- stringr::str_extract(parameter, "\\d+")
        if (index == 1) {
            theta_init(init)
        } else {
            theta_init(init, lower=0, upper=50)
        }
    } else if (parameter == "GUE") {
        theta_init(init, lower=0, upper=1)
    } else {
        stop("Unknown parameter")
    }
}

#' Generate the fix part of the $THETA record for an item
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param parameter The item parameter name
#' @return A string containing "FIX" or an empty string
#' @md
theta_string_fix_part <- function(model, item, parameter) {
    fix <- is_item_parameter_fixed(model, item, parameter)
    if (fix) {
        " FIX"
    } else {
        ""
    }
}

#' Generate the label comment part of the $THETA record for an item
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param parameter The item parameter name
#' @param theta_number The number of the THETA
#' @return A string containing the label for this THETA
theta_string_label_part <- function(model, item, parameter, theta_number) {
    paste0("; I", item$number, parameter, " ", theta_number)
}

# The functions below operate in different ways on the item_parameters data.frame
# in a model object. The data.frame has the columns item, parameter, fix and init,
# where item is the number of the item, parameter is the name of the item parameter,
# fix is a boolean telling wheter the parameter should be fixed or not and init is
# the initial estimate that will be used for the parameter.

#' Insert into parameter table
#'
#' A helper function to insert new data into the parameter. The columns are:
#' item, parameter, fix, init and ignore.
#' 
#' @param model An irt_model object
#' @param new_df Data to be inserted
#' @param column Name of the column to be inserted (fix, init or ignore)
#' @return A new irt_model object
insert_into_parameter_table <- function(model, new_df, column) {
    df <- model$item_parameters
    if (nrow(df) != 0) {
        remaining <- dplyr::semi_join(new_df, df, by=c("item", "parameter"))       # Rows with keys existing in both return rows from new
        new <- dplyr::anti_join(new_df, df, by=c("item", "parameter"))    # Rows with keys in new but not in old
        if (nrow(remaining) != 0) {
            for (i in 1:nrow(remaining)) {  # Replace the fix or init value of each row already existing in the item_parameters
                df[df$item == remaining[i,]$item & df$parameter == remaining[i,]$parameter, ][[column]] <- remaining[i, ][[column]]
            }
        }
        model$item_parameters <- rbind(df, new)
    } else {
        model$item_parameters <- new_df 
    }
    model
}

update_parameter_table <- function(model, new_df){
    model$item_parameters <- update_or_insert(model$item_parameters, new_df, c("item", "parameter"))
    model
}


#' Fix the same parameters for some items
#'
#' @param model An irt_model object
#' @param items A vector of item numbers
#' @param parameter_names A vector of names of parameters
#' @return A new irt_model object
#' @export
fix_item_parameters <- function(model, items, parameter_names) {
    for (item in items) {   # Check that all parameter_names are valid
        names <- item_parameter_names(model, item)
        if (!all(parameter_names %in% names)) {
            names_string <- paste(names,  collapse=',')
            stop(paste0("A specified parameter does not exist for item ", item, ". Valid parameter names for this item are: ", names_string))
        }
    }
    new_df <- expand.grid(item=items, parameter=parameter_names, fix=TRUE, init=as.numeric(NA), ignore=as.logical(NA), stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
    insert_into_parameter_table(model, new_df, "fix")
}

#' Fix all item parameters 
#' 
#' The function fixes all item parameters in the model
#'
#' @param model An irt_model object
#'
#' @return An updated irt_model object
#' @export
fix_all_item_parameters <- function(model){
    df_new <- purrr::map_dfr(all_items(model), 
                   ~tibble::tibble(item = .x, 
                                   parameter = item_parameter_names(model, .x),
                                   fix = T)) 
    model <- update_parameter_table(model, df_new)
    model
}

#' Ignore all parameters for some items
#'
#' Ignoring an item means to set its parameters to 0 FIX
#' and to let data records with this item be ignored.
#' 
#' @param model An irt_model object
#' @param items A vector of item numbers
#' @return A new irt_model objet
#' @export
ignore_items <- function(model, items) {
    new_df <- NULL
    for (i in items) {
        parameters <- item_parameter_names(model, i)
        new_df <- dplyr::bind_rows(
            new_df,
            data.frame(
                item = i,
                parameter = parameters,
                ignore = TRUE,
                stringsAsFactors = FALSE,
                KEEP.OUT.ATTRS = FALSE
            )
        )
    }
    model <- update_parameter_table(model, new_df)
    model
}

#' Get a vector of all ignored items
#' 
#' @param model An irt_model object
#' @return Vetor of ignored items
get_ignored_items <- function(model) {
    rows <- dplyr::filter(model$item_parameters, .data$ignore==TRUE)
    unique(rows[['item']])
}

#' Set initial estimates
#' 
#' These functions allow to set the initial estimates for parameters in the model. Values
#' can either be set for particular set of items and parameters or by providing a data.frame with 
#' the inital values for each parameter. 
#' 
#' @param model An irt_model object
#' @param items A vector of item numbers
#' @param parameters A vector of parameter names that will be used for each of the items
#' @param inits A vector of initial estimates. Must be equal in size to parameters or have size one.
#' @return A new irt_model object
#' @export
set_initial_estimates <- function(model, items, parameters, inits){
    stopifnot(is.irt_model(model))
    stopifnot(length(inits) == 1 || length(parameters) == length(inits))
    df <- expand.grid(parameter=parameters, item=items, stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
    df$init <- inits    # Broadcast hence order of parameters and item columns
    update_parameter_table(model, df)
}

#' @rdname set_initial_estimates
#' @param df A data.frame with columns 'item', 'parameter', 'init'/'value'
#' @export
set_initial_estimates_table <- function(model, df){
    stopifnot(is.irt_model(model))
    if(!all(c("item", "parameter") %in% colnames(df))) rlang::abort("The columns 'item' and 'parameter' are required")
    if("value" %in% colnames(df)) {
        df <- dplyr::rename(df, init=.data$value)
    }else if(!"init" %in% colnames(df)) rlang::abort("The columns 'init' or 'value' are required")
    
    update_parameter_table(model, df)
}

#' Update model parameters
#' 
#' This function updates the parameter values of the model. It accepts data in long and wide
#' format and ,therefore, can be used to update from a NONMEM table or the output of 
#' \code{\link{estimate_item_parameters}}.  
#' 
#' @details 
#' The long format is recognized by the presence of the column 'parameter'. In long format, only the columns
#' item, parameter and value are used.  
#' 
#' If the 'parameter' column is not found, the wide format is assumed for the parameter data. In wide format, the columns
#' item, dis, gue, dif, and difN (where N indicates a number) are used. All other columns are ignored. 
#' 
#' Capitalization of the column or parameter names does not matter.    
#'
#' @param model An irt_model object
#' @param data A data.frame 
#'
#' @return An irt_model object
#' @export 
update_parameters <- function(model, data){
    stopifnot(is.irt_model(model))
    stopifnot(is.data.frame(data))
    data <- dplyr::rename_all(data, tolower)
    if(!'item' %in% colnames(data)){
        rlang::abort("The data column 'item' is required.")
    }
    if("parameter"  %in% colnames(data) ){
        item_prms <- dplyr::select(data, c("item", "parameter","value")) %>% 
            dplyr::mutate_at("parameter", tolower)
    }else{
        item_prms_wide <- dplyr::filter(data, !duplicated(.data$item)) %>% 
            dplyr::select("item", dplyr::matches("^dis|gue$"), dplyr::matches("^dif\\d*$")) 
        
        if(ncol(item_prms_wide)==1) return(model)
        
        item_prms <- item_prms_wide %>%
            tidyr::pivot_longer(cols = -.data$item, 
                                names_to = "parameter", 
                                values_to = "value",
                                values_drop_na = TRUE) %>% 
            dplyr::rename(item = .data$item)
        
    }
    required_prms <- purrr::map_dfr(all_items(model), 
                                    ~tibble::tibble(item = .x, 
                                                    parameter = item_parameter_names(model, .x))) %>% 
        dplyr::mutate_at("parameter", tolower)
    item_prms <- dplyr::left_join(required_prms, 
                                  item_prms, 
                                  by = c("item", "parameter")) %>% 
        dplyr::filter(!is.na(value)) %>% 
        dplyr::mutate(parameter = toupper(.data$parameter),
                      init = value)
    return(update_parameter_table(model, item_prms))
}


#' Generate a data frame to give an overview of initial estimates for a model
#' 
#' @param model An irt_model object
#' @return A data frame with columns item, parameter, fix and init for all items and parameters
#' @export
list_initial_estimates <- function(model) {
    stopifnot(is.irt_model(model))
    # First generate table over keys (item, parameters)
    table <- data.frame(item=numeric(0), parameter=character(0))
    for (item in model$scale$items) {
        parameters <- item_parameter_names(model, item$number)
        item_table <- expand.grid(item=item$number, parameter=parameters, stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
        table <- rbind(table, item_table)
    }

    # Add fix and init columns
    table <- table %>%
        dplyr::group_by(.data$item, .data$parameter) %>%
        dplyr::mutate(fix=is_item_parameter_fixed(model, get_item(model$scale, .data$item), .data$parameter), init=initial_estimate(model, get_item(model$scale, .data$item), .data$parameter)) %>%
        dplyr::ungroup()
    as.data.frame(table)
}


#' Get all unique non-ignored parameter names for a model
#' 
#' @param model An irt_model object
#' @return A vector of parameter names
all_parameter_names <- function(model) {
    scale <- model$scale
    ignored_items <- get_ignored_items(model)
    parameter_names <- c()
    for (item in scale$items) {
        if (!(item$number %in% ignored_items)) {
            parameter_names <- unique(c(parameter_names, item_parameter_names(model, item$number)))
        }
    }
    sort(parameter_names)
}


#' Test if a model has all required inital estimates 
#'
#' @param model The model
#'
#' @return TRUE if an initial value is given for all item parameters in the model, otherwise FALSE 
#' @export 
has_all_initial_estimates <- function(model){
    stopifnot(is.irt_model(model))
    for (item in model$scale$items) {
        item_prms <- dplyr::filter(model$item_parameters, .data$item==!!item$number)
        if(nrow(item_prms)==0) return(FALSE)
        if(item$type == item_type$binary){
            required_prms <- c("DIS", "DIF", "GUE")
        }else if(item$type == item_type$ordered_categorical){
            required_prms <- c("DIS", paste0("DIF", item$levels[-1]))
        }
        if(!all(required_prms %in% item_prms$parameter)) return(FALSE)
    }
    return(TRUE)
}
