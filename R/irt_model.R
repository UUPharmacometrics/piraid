#' Create an IRT model object
#' 
#' \code{irt_model} returns a newly created model object
#'
#' @param scale The scale
#' @return A model object
#' @export
irt_model <- function(scale) {
    stopifnot(is.irt_scale(scale))
    item_parameters <- data.frame(item=numeric(0), parameter=character(0), fix=logical(0), init=numeric(0), ignore=logical(0), stringsAsFactors=FALSE)
    model <- base_model(subclass="irt_model")
    model$scale = scale
    model$consolidation = list()
    model$lv_models = list()
    model$item_parameters = item_parameters
    if(!is.null(scale$source_file)) {
        if(!file.exists(scale$source_file)) {
            warning("The file that was used to generate the scale does not exist and couldn't be added to the model.")
        }else{
            model <-  set_dataset(model, path = scale$source_file)
        }
    }
    model
}

#' Check wheter x is an irt_model object
#' 
#' @param x An object to test
#' @return True if x is an irt_model object
#' @export
is.irt_model <- function(x) {
    inherits(x, "irt_model")
}

#' Print a brief overview of a model
#' 
#' @param x An irt_model object
#' @param ... No additional arguments are supported
#' @export
print.irt_model <- function(x, ...) {
    model <- x
    scale <- model$scale
    number_of_lv_models <- length(model$lv_models)
    if (number_of_lv_models == 0) {
        number_of_lv_models <- 1
    }
    number_of_item_parameters <- sum(sapply(all_items(scale), function(x, scale) {length(item_parameter_names(model, x))}, scale=scale))
    cat("A model object from ", utils::packageName(), "\n\n", sep="")
    cat("Number of item parameters: ", number_of_item_parameters, "\n", sep="")
    cat("Number of latent variables: ",  number_of_lv_models, "\n", sep="")
    cat("\nScale information:\n", sep="")
    print_scale_info(scale, header=FALSE)
}

#' Change the scale of an IRT model object
#'
#' \code{set_scale} returns a newly created model object
#'
#' @param model An irt_model orbject
#' @param scale The scale object
#' @return The updated model object
#' @export
set_scale <- function(model, scale) {
    stopifnot(is.irt_model(model))
    stopifnot(is.irt_scale(scale))
    model$scale <- scale
    model
}

#' Print NONMEM code
#' 
#' Output the NONMEM code of a model object to the console
#' 
#' @param model A model object
#' @export
print_model_code <- function(model) {
    stopifnot(is.irt_model(model))
    cat(str_irt_model(model))
}

#' Save NONMEM code
#' 
#' Saves the NONMEM code of a model object to a file
#' 
#' @param model A model object
#' @param path Path to the created model file
#' @export
save_model_code <- function(model, path) {
    stopifnot(is.irt_model(model))
    str <- str_irt_model(model)
    fp <- file(path)
    writeLines(str, fp)
    close(fp)
}

#' Check if all mandatory components has been added to a model
#'
#' Currently the scale and the dataset are mandatory.
#' Will give error if model is not complete
#'
#' @param model irt_model object
#' @keywords internal
model_complete <- function(model) {
    if (!("dataset" %in% names(model))) {
        stop("A dataset needs to be added to the model. Please use set_dataset before printing the model")
    }
}

#' Will remove the consolidated levels from the scale of a model
#' 
#' @param model irt_model object
#' @return An irt_model object
#' @keywords internal
apply_consolidation <- function(model) {
    scale <- model$scale

    for (n in seq_along(model$consolidation)) {
        consolidated <- model$consolidation[[n]]
        if (is.null(consolidated)) {
            next
        }
        item <- get_item(scale, n)
        item$levels <- setdiff(item$levels, model$consolidation[[n]])
        scale <- add_item(scale, item, replace=TRUE)
    }
    model$scale <- scale
    model
}

#' Create NONMEM model as a string
#' 
#' @param model A model object
#' @return A string with the NONMEM code
str_irt_model <- function(model) {
    model_complete(model)
    model <- apply_consolidation(model) # Will remove the consolidated levels from the scale
    next_theta <- 1
    cg <- code_generator()
    cg <- add_line(cg, "$SIZES LIM6=4000 LTH=-1000 DIMNEW=-10000")
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, irt_data_and_input_code(model))
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$PRED")
    cg <- add_code(cg, type_constants(model))
    cg <- add_empty_line(cg)
    cg <- banner_comment(cg, "assignment of item parameters")
    first <- TRUE
    for (item in model$scale$items) {
        res <- irt_item_assignment_code(model, item, next_theta, first)
        if (first) {
            first <- FALSE
        }
        next_theta <- res$next_theta
        cg <- add_code(cg, res$code)
    }
    cg <- irt_item_assignment_fallthrough(cg, model$scale)
    cg <- add_empty_line(cg)
    cg <- banner_comment(cg, "The latent variable model")
    cg <- add_empty_line(cg)
    a <- latent_variable_code(model, next_theta)
    numthetas <- a$next_theta - next_theta
    numetas <- a$next_eta - 1
    cg <- add_code(cg, a$cg)
    cg <- add_empty_line(cg)
    cg <- add_code(cg, data_models_code(model))
    cg <- add_code(cg, response_probability_prediction_code())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, undef_item_code())
    cg <- add_empty_line(cg)
    cg <- add_code(cg, simulation_code(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, estimation_task(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, omegas(model, numetas))
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$THETA")
    cg <- add_code(cg, initial_item_thetas(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, initial_thetas(model, numthetas))
    if (model$simulation) {
        cg <- add_code(cg, simulation_task(model))
    }
    get_code(cg)
}

has_lv_model <- function(model){
    return(length(model$lv_models)>0)
}

#' Create the $DATA and $INPUT to NONMEM model code
#' 
#' @param model A irt_model object
#' @param rewind If the dataset should be rewound or not
#' @return A code generator object
irt_data_and_input_code <- function(model, rewind=FALSE) {
    cg <- data_and_input_code(model)
    ignored_items <- get_ignored_items(model)
    if (length(ignored_items) > 0) {
        ignores <- paste0("IGNORE(ITEM.EQN.", ignored_items, ")")
        ignore_lines <- join_with_max_length(ignores)
        cg <- increase_indent(cg)
        cg <- add_lines(cg, ignore_lines)
        cg <- decrease_indent(cg)
    }
    cg
}

#' Generate the NONMEM code for definitions of type constants
#' 
#' @param model An irt_model object
#' @return A code generator object
type_constants <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "constants to select model type")
    levels <- ordcat_level_arrays(model$scale)
    cg <- add_line(cg, "PSI_MODEL=0")
    cg <- add_line(cg, "UNDEF=0")
    for (i in seq_along(levels)) {
        cg <- add_line(cg, paste0("OC", i, "=", i, '    ; ordered categorical ', levels_as_string(levels[[i]])))
    }
    i <- length(levels) + 1
    for (item in model$scale$items) {
        if (item$type == item_type$binary) {
            cg <- add_line(cg, paste0("BIN=", i, '    ; binary ', levels_as_string(item$levels)))
            break
        }
    }
    cg
}

#' Generate NONMEM code for all different item model types
#' 
#' @param model An irt_model object
#' @return A code generator object
data_models_code <- function(model) {
    cg <- code_generator()
    bin_items <- items_by_type(model$scale, item_type$binary)
    if (length(bin_items) > 0) {
        cg <- add_code(cg, binary_data_model_code())    # Only one type of binary allowed (0, 1)
    }
    ordcat_levels <- ordcat_level_arrays(model$scale)
    if (length(ordcat_levels) > 0) {
        for (l in ordcat_levels) {
            cg <- add_code(cg, ordered_categorical_data_model_code(model$scale, l))
            cg <- add_empty_line(cg)
        }
    }
    cg
}

#' Get code symbol for type constant given scale and item
#' 
#' @param scale An irt_scale object
#' @param item An irt_item object
#' @return The code symbol for item (i.e. BIN, OC1 etc)
model_type_constant <- function(scale, item) {
    if (item$type == item_type$binary) {
        return("BIN")
    }
    ordcat_levels <- ordcat_level_arrays(scale)
    for (i in 1:length(ordcat_levels)) {
        if (length(item$levels) == length(ordcat_levels[[i]]) && all(item$levels == ordcat_levels[[i]])) {
            return(paste0("OC", i))
        }
    }
}

#' Generate NONMEM code for assignment of item model parameters
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param next_theta The number of the next THETA to be created
#' @param first Is this the first item?
#' @return A code generator object
irt_item_assignment_code <- function(model, item, next_theta, first) {
    scale <- model$scale
    cg <- code_generator()
    if (first) {
        ifstring <- "IF"
    } else {
        ifstring <- "ELSE IF"
    }
    cg <- add_line(cg, paste0(ifstring, "(ITEM.EQ.", item$number, ") THEN"))
    cg <- increase_indent(cg)
    lv_index <- get_item_lv_index(model, item$number)
    if (lv_index != 0) {
        cg <- add_line(cg, paste0("PSI_MODEL=", lv_index))
    }
    cg <- add_line(cg, paste0("MODEL=", model_type_constant(scale, item)))
    if (item$type == item_type$ordered_categorical) {
        cg <- add_line(cg, paste0("DIS=THETA(", next_theta, ")"))
        next_theta <- next_theta + 1
        for (i in 1:(length(item$levels) - 1)) {
            cg <- add_line(cg, paste0("DIF", i, "=THETA(", next_theta, ")"))
            next_theta <- next_theta + 1
        }
    } else { # item_type$binary
        cg <- add_line(cg, paste0("DIS=THETA(", next_theta, ")"))
        cg <- add_line(cg, paste0("DIF=THETA(", next_theta + 1, ")"))
        cg <- add_line(cg, paste0("GUE=THETA(", next_theta + 2, ")"))
        next_theta <- next_theta + 3
    }
    cg <- decrease_indent(cg)
    list(code=cg, next_theta=next_theta)
}

#' Create fallthrough NONMEM code for unknown items
#' 
#' If the dataset contains items that the model wasn't created
#' for this code takes care of bailing out. It also contains
#' some dummy assignments that are only there to silence some
#' NM-TRAN warnings.
#' 
#' @param cg A code generator object to build upon
#' @param scale An irt_scale object
#' @return A code generator object
irt_item_assignment_fallthrough <- function(cg, scale) {
    cg <- add_line(cg, "ELSE")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "MODEL=UNDEF")
    cg <- add_line(cg, "PPRED=0")
    cg <- add_line(cg, "SDPRED=0")
    cg <- add_line(cg, "P=0")
    cg <- add_line(cg, "P0=0")
    levels <- ordcat_level_arrays(scale)
    max <- 1
    for (level_array in levels) {
        if (length(level_array) - 1 > max) {
            max <- length(level_array) - 1
        }
        if (max(level_array) > max) {
            max <- max(level_array)
        }
    }
    for (i in 1:max) {
        cg <- add_line(cg, paste0("P", i, "=0"))
    }
    for (i in 1:max) {
        cg <- add_line(cg, paste0("PGE", i, "=0"))
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "END IF")
    cg
}

#' Generate NONMEM code for binary data items
#' 
#' @return A code generator
binary_data_model_code <- function() {
    cg <- code_generator()
    cg <- banner_comment(cg, "binary data model")
    cg <- add_line(cg, "IF(MODEL.EQ.BIN) THEN")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "P1=GUE+(1-GUE)*EXP(DIS*(PSI-DIF))/(1+EXP(DIS*(PSI-DIF)))")
    cg <- add_line(cg, "P0=1-P1")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "PPRED = P1")
    cg <- add_line(cg, "SDPRED = SQRT(P0*P1)")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg <- add_line(cg, "IF(MODEL.EQ.BIN.AND.DV.EQ.0) P=P0")
    cg <- add_line(cg, "IF(MODEL.EQ.BIN.AND.DV.EQ.1) P=P1")
    cg <- add_empty_line(cg)
    cg
}

#' Generate NONMEM code for an ordered categorical item model
#'
#' @param scale A scale object
#' @param levels A vector of the response levels
#' @return A code generator object
ordered_categorical_data_model_code <- function(scale, levels) {
    dummy_item <- irt_item(0, "", levels, item_type$ordered_categorical)
    cg <- code_generator()
    cg <- banner_comment(cg, paste0("ordered categorical data model with ", length(levels), " levels: ", levels_as_string(levels)))
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, "DIFG1=DIF1")
    if (length(levels) > 2) {
        for (i in 1:(length(levels) - 2)) {
            cg <- add_line(cg, paste0("DIFG", i + 1, "=DIFG", i, "+DIF", i + 1))
        }
    }
    cg <- add_empty_line(cg)
    for (i in 1:(length(levels) - 1)) {
        cg <- add_line(cg, paste0("PGE", i, "=EXP(DIS*(PSI-DIFG", i, "))/(1+EXP(DIS*(PSI-DIFG", i, ")))"))
    } 
    cg <- add_empty_line(cg)
    cg <- add_line(cg, paste0("P", levels[1], "=1-PGE1"))
    if (length(levels) > 2) {
        for (i in 1:(length(levels) - 2)) {
            cg <- add_line(cg, paste0("P", levels[i + 1], "=PGE", i, "-PGE", i + 1))
        }
    }
    cg <- add_line(cg, paste0("P", levels[length(levels)], "=PGE", i + 1))
    cg <- add_empty_line(cg)
    cg <- add_line(cg, ppred_code(levels))
    cg <- add_line(cg, sdpred_code(levels))
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ".AND.DV.LE.", levels[1], ") P=P", levels[1]))
    remaining <- levels[-length(levels)]
    remaining <- remaining[-1]
    for (e in remaining) {
        cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ".AND.DV.EQ.", e, ") P=P", e))
    }
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ".AND.DV.GE.", levels[length(levels)], ") P=P", levels[length(levels)]))
    cg
}

#' Generate NONMEM code for response probability and residual
#' 
#' @return A code generator object
response_probability_prediction_code <- function() {
    cg <- code_generator()
    cg <- banner_comment(cg, "Response probability prediction and residual")
    cg <- add_line(cg, "IF(P.LT.1E-16) P=1E-16  ; To protect for P->0")
    cg <- add_line(cg, "IF(P.GT.(1-1E-16)) P=1-1E-16  ; To protect for P->1")
    cg <- add_line(cg, "Y=-2*LOG(P)")
    cg <- add_line(cg, pwres_code())
    cg
}

#' Generate NONMEM code to clear variables for undef item
#'
#' @return A code generator object
undef_item_code <- function() {
    cg <- code_generator()
    cg <- add_line(cg, "IF (MODEL.EQ.UNDEF) THEN")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "PWRES=0")
    cg <- add_line(cg, "P=0")
    cg <- add_line(cg, "Y=0")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}

#' Generate NONMEM code for model simulation
#' 
#' @param model An irt_model object
#' @return A code generator object
simulation_code <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "simulation code")
    cg <- add_line(cg, "IF(ICALL.EQ.4) THEN")
    cg <- increase_indent(cg)
    bin_items <- items_by_type(model$scale, item_type$binary)
    if (length(bin_items) > 0) {
        cg <- add_line(cg, "IF(MODEL.EQ.BIN) THEN")
        cg <- increase_indent(cg)
        cg <- add_line(cg, "CALL RANDOM (2,R)")
        cg <- add_line(cg, "SDV=0")
        cg <- add_line(cg, "IF(P1.GT.R) SDV=1")
        cg <- decrease_indent(cg)
        cg <- add_line(cg, "ENDIF")
    }
    levels <- ordcat_level_arrays(model$scale)
    for (l in levels) {
        cg <- add_code(cg, ordered_categorical_simulation_code(model$scale, l))
        cg <- add_empty_line(cg)
    }
    cg <- add_line(cg, "DV=SDV")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}

#' Generate NONMEM code for estimation and table output
#' 
#' @param model An irt_model object
#' @return A code generator object
estimation_task <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "estimation task")
    if (model$simulation) {
        msfo <- " MSFO=msf4"
    } else {
        msfo <- ""
    }
    cg <- add_line(cg, paste0("$ESTIMATION METHOD=COND LAPLACE -2LL MAXEVAL=999999 PRINT=1", msfo, " ", model$estimaton_options))

    item_parameters <- all_parameter_names(model)
    item_parameters <- c(item_parameters, paste0("DIFG", 1:sum(stringr::str_starts(item_parameters, "DIF\\d"))))

    irt_table_options <- c("$TABLE", "ID", "TIME", "DV", mdv_string(model), "ITEM", "PSI", "PPRED", "PWRES", item_parameters, "VARCALC=1", paste0("FILE=irt_tab", model$run_number), "NOAPPEND", "ONEHEADER", "NOPRINT")
    cg <- add_line(cg, paste(irt_table_options, collapse=" "))
    cg
}

#' Generate NONMEM code for a model simulation task
#' 
#' @param model An irt_model object
#' @return A code generator object
simulation_task <- function(model) {
    cg <- code_generator()
    cg <- add_empty_line(cg)
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$PROBLEM Simulation")
    cg <- add_empty_line(cg)
    cg <- add_code(cg, irt_data_and_input_code(model, rewind=TRUE))
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$MSFI msf4")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, paste0("$SIMULATION (875435432) (3872543 UNIFORM) NOPREDICTION ONLYSIMULATION SUBPROBLEMS=", model$subproblems, " TRUE=FINAL ", model$simulation_options))
    columns <- paste(model$data_columns, collapse=' ')
    cg <- add_line(cg, paste0("$TABLE ", columns, " FILE=simulation_tab", model$run_number, " FORMAT=,1PE11.4 NOAPPEND ONEHEADER NOPRINT"))
    cg
}

#' Generate NONMEM code for the random effect
#' 
#' @param model An irt_model object
#' @param numomegas The number of omegas needed
#' @return A code generator object
omegas <- function(model, numomegas) {
    cg <- code_generator()
    cg <- banner_comment(cg, "random effects")
    for (i in seq(1:numomegas)) {
        if(has_lv_model(model)){
            cg <- add_line(cg, "$OMEGA 0.1")
        }else{
            cg <- add_line(cg, "$OMEGA 1 FIX")
        }
    }
    cg
}

#' Generate NONMEM code for simulation of and ordered categorical item
#' 
#' @param scale A scale object
#' @param levels A vector of levels for the item
#' @return A code generator object
ordered_categorical_simulation_code <- function(scale, levels) {
    dummy_item <- irt_item(0, "", levels, item_type$ordered_categorical)
    cg <- code_generator()
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, "CALL RANDOM(2, R)")
    cg <- add_line(cg, paste0("SDV=", levels[1]))
    for (i in 1:(length(levels) - 1)) {
        cg <- add_line(cg, paste0("IF(R.LT.PGE", i, ") SDV=", levels[i + 1]))
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}

#' Generate NONMEM code for the thetas of the latent variable model
#' 
#' @param model An irt_model object
#' @param numthetas The number of latent variable model thetas
#' @return A code generator object
initial_thetas <- function(model, numthetas) {
    cg <- code_generator()
    cg <- banner_comment(cg, "latent variable model parameters")
    for (i in seq(1, numthetas)) {
        if(has_lv_model(model)){
            cg <- add_line(cg, "0.1")
        }else{
            cg <- add_line(cg, "0 FIX")
        }
    }
    cg
}

#' Generate NONMEM code for initial estimates of thetas
#' 
#' Will use the initial estimates of the scale
#' @param model An irt_model object
#' @return A code generator object
initial_item_thetas <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "item parameters")
    i <- 1
    for (item in model$scale$items) {
        theta_strings <- item_inits(model, item, i)
        for (line in theta_strings) {
            cg <- add_line(cg, line)   
        }
        i <- i + length(theta_strings)
    }
    cg
}


#' Consolidation of levels in a model
#' 
#' The consolidated levels will be treated as being equal to the nearest lower level.
#' 
#' @param model An irt_model object
#' @param item_numbers A vector of item numbers to consolidate
#' @param levels A vector of levels to consolidate, i.e. merging into the nearest lower level.
#' @return A new irt_model object
#' @export
consolidate_levels <- function(model, item_numbers, levels) {
    stopifnot(is.irt_model(model))
    stopifnot(length(levels) >= 1)
    for (item_number in item_numbers) {
        item <- get_item(model$scale, item_number)
        run <- rle(item$levels %in% levels)$values      # Check that consolidated levels are at the upper edge of the available levels and consecutive
        if (length(run) == 2) {
            high <- all(run == c(FALSE, TRUE))
            if (high) {
                levels_to_consolidate <- sort(levels)
                model$consolidation[[item_number]] <- levels_to_consolidate 
            } else {
                stop("Can only consolidate levels at the high end of the level range")
            }
        } else {
            stop("Can only consolidate levels at the high end of the level range")
        }
    }
    model
}

#' Automatically consolidate all items given a certain minimum count
#'
#' If a the occurences of a level for an item is not above count it will
#' be consolidated into the nearest level. Consolidation can only be done
#' from the higher levels and down using this function. Levels that are
#' present in the scale but missing in the dataset also gets consolidated.
#'
#' @param model An irt_model object
#' @param count The maximum count for consolidation
#' @return A new irt_model object
#' @export 
consolidate_levels_below <- function(model, count) {
    stopifnot(is.irt_model(model))
    df <- item_level_count(model)
    df <- dplyr::mutate(df, thresh=.data$n <= !!count)
    df
    
    for (item in model$scale$items) {
        item_data <- dplyr::filter(df, .data$ITEM == item$number)
        levels_in_data <- item_data[['DV']]
        levels_in_scale <- item$levels
        levels_below_thresh <- dplyr::filter(item_data, .data$thresh)[['DV']]
        levels_missing_in_data <- setdiff(levels_in_scale, levels_in_data)
        candidates_for_consolidation <- sort(c(levels_below_thresh, levels_missing_in_data))
        levels_to_consolidate <- c()
        for (level in rev(sort(levels_in_scale))) {
            if (level %in% candidates_for_consolidation) {
                levels_to_consolidate <- c(level, levels_to_consolidate)   
            } else {
                break
            }
        }
        if (length(levels_to_consolidate > 0) && length(levels_to_consolidate) < length(levels_in_scale)) {
            model <- consolidate_levels(model, item_numbers=item$number, levels=levels_to_consolidate)
        }
    }
    model
}

#' Check if a level for an item was consolidated
#' 
#' @param model An irt_model object
#' @param item An irt_item object
#' @param level A level
#' @return TRUE if level was consolidated for item and FALSE otherwise
consolidated <- function(model, item, level) {
    if (length(model$consolidation) >= item$number) {   # Does item exist in consolidation list?
        consolidated <- model$consolidation[[item$number]]
        if (!is.null(consolidated) && level %in% consolidated) {
            return(TRUE)
        }
    }
    FALSE
}

#' Get an MDV string for $TABLE if dataset has MDV column
#' 
#' @param model irt_model object
#' @return An empty string or " MDV "
#' @keywords internal
mdv_string <- function(model) {
    if ("MDV" %in% model$data_columns) {
        "MDV"
    } else {
        NULL
    }
}

#' Get the count of each item and level
#' 
#' @param model_or_data An irt_model object or a data.frame
#' @return data.frame with ITEM, DV and count columns
#' @export
item_level_count <- function(model_or_data) {
    if (is.irt_model(model_or_data)) {
        df <- read_dataset(model_or_data$dataset) %>% prepare_dataset()
    } else {
        df <- model_or_data
    }
    
    dplyr::count(df, .data$ITEM, .data$DV) %>% as.data.frame()
}