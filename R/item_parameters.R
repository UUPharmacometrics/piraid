#' Get the initial estimate for a certain item and parameter
#' 
#' The initial estimate will be:
#' 
#' 1. The user defined value from set_item_parameters
#' 2. 50 if the parameter was consolidated
#' 3. Taken from the published model in the scale
#' 4. A fall back value of:
#'       Binary: DIS=1, DIF=0.1, GUE=0.01
#'       Ordered categorical: DIS=1, DIFn=-3 + 6n/#cats
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
    if (length(model$consolidation) >= item$number) {   # Does item exist in consolidation list
        consolidated <- model$consolidation[[item$number]]
        if (!is.null(consolidated) && item$number %in% consolidated) {
            return(50)
        }
    }
    published <- published_init(model$scale, item$number, parameter)
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
        index <- stringr::str_extract(parameter, '\\d+')
        -3 + (6 * index) / length(item$levels)
    }
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
    parameters <- item_parameter_names(model$scale, item$number)
    theta_strings <- character()
    for (parameter in parameters) {
        theta_strings <- c(theta_strings, theta_string_item_parameter(model, item, parameter, start_theta))
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
    if (parameter == "DIS") {
        theta_init(init, lower=0)
    } else if (item$type == "binary" && parameter == "DIF") {
        theta_init(init, lower=-50, upper=50)
    } else if (startsWith(parameter, "DIF")) {
        index <- stringr::str_extract(parameter, "\\d+")
        if (index == 1) {
            theta_init(init)
        } else {
            theta_init(init, lower=0, upper=50)
        }
    } else if (parameter == "GUE") {
        init
    } else {
        stop("Unknown parameter")
    }
}

#' Generate the fix part of the $THETA record for an item
#' 
#' FIX should be returned if:
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
theta_string_fix_part <- function(model, item, parameter) {
    init <- initial_estimate(model, item, parameter)
    if (init == 0) {
        return(" FIX")
    } else if (consolidated(model, item, item_parameter_index(model$scale, item$number, parameter))) {
        return(" FIX")
    }
    par_row <- dplyr::filter(model$item_parameters, item==!!item$number, parameter==!!parameter)
    if (nrow(par_row) != 0 && !is.na(par_row$fix) && par_row$fix) {
        return(" FIX")
    }
    ""
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
#' item, parameter, fix and init
#' 
#' @param model An irt_model object
#' @param new_df Data to be inserted
#' @param column Name of the column to be inserted (fix or init)
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

#' Fix the same parameters for some items
#'
#' @param model An irt_model object
#' @param items A vector of item numbers
#' @param parameter_names A vector of names of parameters
#' @return A new irt_model object
#' @export
fix_item_parameters <- function(model, items, parameter_names) {
    for (item in items) {   # Check that all parameter_names are valid
        names <- item_parameter_names(model$scale, item)
        if (!all(parameter_names %in% names)) {
            names_string <- paste(names,  collapse=',')
            stop(paste0("A specified parameter does not exist for item ", item, ". Valid parameter names for this item are: ", names_string))
        }
    }
    new_df <- expand.grid(item=items, parameter=parameter_names, fix=TRUE, init=as.numeric(NA), stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
    insert_into_parameter_table(model, new_df, "fix")
}

#' Set initial estimates for some parameters
#' 
#' Each item will for the selected parameters get a new initial estimate.
#' If there is only one initial estimate provided all parameters will get this value.
#' If more than one initial estimate is provided they will be distributed to the parameters
#' in order. Note that the number of parameters and the number of initial estimates provided
#' must then be equal.
#' 
#' @param model An irt_model object
#' @param items A vector of item numbers
#' @param parameters A vector of parameter names
#' @param inits A vector of initial estimates. Must be equal in size to parameters or have size one.
#' @return A new irt_model object
#' @export
initial_estimates_item_parameters <- function(model, items, parameters, inits) {
    stopifnot(length(inits) == 1 || length(parameters) == length(inits))
    
    new_df <- expand.grid(parameter=parameters, item=items, fix=as.logical(NA), init=as.numeric(NA), stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
    new_df$init <- inits    # Broadcast hence order of parameters and item columns
    new_df <- dplyr::select(new_df, "item", "parameter", "fix", "init")
    insert_into_parameter_table(model, new_df, "init")
}