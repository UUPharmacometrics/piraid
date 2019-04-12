#' Add a custom latent variable model
#'
#' The model will be used for the items with numbers listed in the items argument.
#' If a model was already added for a listed item the function will give an error.
#' Use reset_lv_models to restart adding models for the latent variable.
#' The model code should be in regular NM-TRAN format with the following restrictions:
#' * The final line must define PSI. (i.e. PSI=some code)
#' * ETAs and THETAs can be used and numbering of these must start from 1
#' 
#' @param model An irt_model object
#' @param items A vector of item numbers
#' @param lv_model_code NM-TRAN abbreviated code as a character vector of lines
#' @return A new irt_model object
#' @export
#' @md
add_custom_lv_model <- function(model, items, lv_model_code) {
    for (lv_model in model$lv_models) {
        if (any(items %in% lv_model$items)) {
            stop("A model for this item has already been specified. Please use reset_lv_models to start over.")
        }
    }
    model$lv_models <- c(model$lv_models, list(list(items=items, model_code=lv_model_code)))
    model
}

#' Add a pre-defined latent variable model
#' 
#' The specified latent variable model will be applied to all items listed. It can
#' be one of "constant" and "linear".
#' "constant" means PSI = THETA(1) + ETA(1)
#' "linear" means PSI = BASE + SLP * TIME where BASE = THETA(1) + ETA(1) and SLP = THETA(2) + ETA(2)
#' Each call to this function for a set of items will us a different set of ETAs and THETAs
#' The default is to use the same "constant" model for all items.
#' 
#' @param model An irt_model object
#' @param items A vector of item numbers
#' @param lv_model_type The name of the type
#' @return A new irt_model object
#' @export
add_lv_model <- function(model, items, lv_model_type) {
    if (lv_model_type == "constant") {
        code = c("PSI=THETA(1)+ETA(1)")
    } else if (lv_model_type == "linear") {
        code = c("BASE=THETA(1)+ETA(1)", "SLP=THETA(2)+ETA(2)", "PSI=BASE+SLP*TIME")
    } else {
        stop("Unknown lv_model_type. Available are \"constant\" and \"linear\"")   
    }
    add_custom_lv_model(model, items, code)
}

#' Reset all added latent variable models
#' 
#' @param model An irt_model object
#' @return A new irt_model object
#' @export
reset_lv_models <- function(model) {
    model$lv_models <- list()
}

#' Get number of THETAs and ETAs used in a vector of code lines
#' 
#' @param lines A character vector of code lines
#' @return A list with numetas and numthetas
get_number_of_thetas_and_etas <- function(lines) {
    numetas <- 0
    numthetas <- 0
    for (line in lines) {
        m <- stringr::str_match_all(line, "\\bETA\\((\\d+)\\)")[[1]]
        if (nrow(m) > 0) {
            found <- as.integer(m[,2])
            numetas <- max(numetas, found)
        }
        m <- stringr::str_match_all(line, "\\bTHETA\\((\\d+)\\)")[[1]]
        if (nrow(m) > 0) {
            found <- as.integer(m[,2])
            numthetas <- max(numthetas, found)
        }
    }
    list(numetas=numetas, numthetas=numthetas)
}

#' Get the index of the latent variable model to use for a certain item
#' 
#' The returned index is for the internal model$lv_models list, but
#' also for the number of the PSI model in the NONMEM model. A returned
#' value if 0 means to use the default.
#' 
#' @param model An irt_model object
#' @param item An item number
#' @return The index
get_item_lv_index <- function(model, item) {
    index <- 1
    for (lv_model in model$lv_models) {
        if (item %in% lv_model$items) {
            return(index)
        }
        index <- index + 1
    }
    0
}

#' Generate abbreviated code for the latent variable model
#' 
#' @param model An irt_model object
#' @param next_theta The number of the next theta to be used
#' @return A list of: a code generator object, next theta and next eta
latent_variable_code <- function(model, next_theta) {
    cg <- code_generator()
    next_eta <- 1
    index <- 1
    for (lv_model in model$lv_models) {
        if_header <- paste0("IF (PSI_MODEL.EQ.", index, ") THEN")
        if (index != 1) {
            if_header <- paste0("ELSE ", if_header)
        }
        cg <- add_line(cg, if_header)
        a <- renumber_parameters(lv_model$model_code, next_theta, next_eta)
        cg <- increase_indent(cg)
        cg <- add_lines(cg, a$code)
        cg <- decrease_indent(cg)
        next_theta <- a$next_theta
        next_eta <- a$next_eta
        index <- index + 1
    }
    if (!all_items_have_lv_model(model)) {  # Do we need the default lv_model?
        if (index != 1) {
            cg <- add_line(cg, "ELSE")
            cg <- increase_indent(cg)
        }
        cg <- add_line(cg, paste0("PSI=THETA(", next_theta, ")+ETA(", next_eta, ")"))
        if (index != 1) {
            cg <- decrease_indent(cg)
        }
        next_theta <- next_theta + 1
        next_eta <- next_eta + 1
    }
    if (index != 1) {
        cg <- add_line(cg, "END IF")
    }
    list(cg=cg, next_theta=next_theta, next_eta=next_eta)
}

#' Renumber parameters used in an array of NM-TRAN abbreviated code lines
#' 
#' THETA(1) in the code will become next_theta, THETA(2) will become next_theta+1 etc
#' @param code An character vector of code lines
#' @param next_theta The number of the next THETA to be used
#' @param next_eta The number of the next ETA to be used
#' @return A list of an array of new code lines, the next_theta and the next_eta
renumber_parameters <- function(code, next_theta, next_eta) {
    a <- get_number_of_thetas_and_etas(code)
    numetas <- a$numetas
    numthetas <- a$numthetas
    new_code <- c()
    for (line in code) {
        for (old_theta_num in seq(1, numthetas)) {
            new_theta_num <- next_theta + old_theta_num - 1
            line <- stringr::str_replace_all(line, paste0("\\bTHETA\\(", old_theta_num, "\\)"), paste0("THETA(", new_theta_num, ")"))
        }
        for (old_eta_num in seq(1:numetas)) {
            new_eta_num <- next_eta + old_eta_num - 1
            line <- stringr::str_replace_all(line, paste0("\\bETA\\(", old_eta_num, "\\)"), paste0("ETA(", new_eta_num, ")"))
        }
        new_code <- c(new_code, line)
    }
    list(code=new_code, next_theta=next_theta + numthetas, next_eta=next_eta + numetas)
}

#' Check if all items have an attached lv model
#' 
#' @param model An irt_model object
#' @return A boolean
all_items_have_lv_model <- function(model) {
    for (item in model$scale$items) {
        item_found <- FALSE
        for (lv_model in model$lv_models) {
            if (item$number %in% lv_model$items) {
                item_found <- TRUE
                break
            }
        }
        if (!item_found) {
            return(FALSE)
        }
    }
    TRUE
}