#' Create an IRT model object
#' 
#' \code{irt_model} returns a newly created model object
#'
#' @param scale The scale
#' @param base_scale A base scale on which the scale is based
#' @return A model object
#' @export
irt_model <- function(scale, base_scale) {
    if (missing(base_scale)) {
        base_scale <- scale
    }
    model <- structure(list(scale=scale, base_scale=base_scale, simulation=FALSE, consolidation=list(), run_number=1,
        lv_models=list()), class="irt_model")
}

#' Change the scale and/or base scale of an IRT model object
#'
#' \code{set_scale} returns a newly created model object
#'
#' @param model An irt_model orbject
#' @param scale The scale object
#' @param base_scale A base scale on which the scale is based
#' @return The updated model object
#' @export
set_scale <- function(model, scale, base_scale) {
    model$scale <- scale
    if (missing(base_scale)) {
        base_scale <- scale
    }
    model$base_scale <- base_scale
    model
}

#' Print NONMEM code
#' 
#' Output the NONMEM code of a model object to the console
#' 
#' @param model A model object
#' @export
print_model <- function(model) {
    cat(str_irt_model(model))
}

#' Save NONMEM code
#' 
#' Saves the NONMEM code of a model object to a file
#' 
#' @param model A model object
#' @param path Path to the created model file
#' @export
save_model <- function(model, path) {
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
        stop("A dataset needs to be added to the model. Please use add_dataset before printing the model")
    }
}


#' Create NONMEM model as a string
#' 
#' @param model A model object
#' @return A string with the NONMEM code
str_irt_model <- function(model) {
    model_complete(model)
    next_theta <- 1
    cg <- code_generator()
    cg <- add_line(cg, "$SIZES LIM6=4000 LTH=-1000")
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, data_and_input_code(model))
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

#' Add dataset
#' 
#' Add a dataset to a model object
#' 
#' @param model A model object
#' @param path Path to a dataset file
#' @return A new model object
#' @export
add_dataset <- function(model, path) {
    model$dataset <- path
    df <- utils::read.csv(model$dataset, nrows=0)
    model$data_columns <- colnames(df)
    model
}

#' Add simulation
#' 
#' Add a number of simulations to the model
#' 
#' @param model A model object
#' @param nsim The number of simulations to add
#' @return A new model object
#' @export
add_simulation <- function(model, nsim=1) {
    model$simulation <- TRUE
    model$subproblems <- nsim
    model
}

#' Set the run number
#' 
#' The run number will be added to the names of the table files
#' 
#' @param model An irt_model object
#' @param run_number An integer
#' @export
set_run_number <- function(model, run_number) {
    model$run_number <- run_number
}

#' Create the $DATA and $INPUT to NONMEM model code
#' 
#' @param model A irt_model object
#' @param rewind If the dataset should be rewound or not
#' @return A code generator object
data_and_input_code <- function(model, rewind=FALSE) {
    cg <- code_generator()
    if (rewind) {
        rewind_code = " REWIND"
    } else {
        rewind_code = ""
    }
    if (!is.null(model$dataset)) {
        cg <- add_line(cg, paste0("$INPUT ", paste(model$data_columns, collapse=' ')))
        cg <- add_line(cg, paste0("$DATA ", normalizePath(model$dataset), rewind_code, " IGNORE=@"))
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
    cg <- add_line(cg, "MODEL=0")
    cg <- add_line(cg, "PSI_MODEL=0")
    for (i in 1:length(levels)) {
        cg <- add_line(cg, paste0("OC", i, "=", i, '    ; ordered categorical ', levels_as_string(levels[[i]])))
    }
    i = i + 1
    for (item in model$scale$items) {
        if (item$type == "binary") {
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
    bin_items <- binary_items(model$scale)
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
    if (item$type == "binary") {
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
    if (item$type == "ordcat") {
        cg <- add_line(cg, paste0("DIS=THETA(", next_theta, ")"))
        next_theta <- next_theta + 1
        for (i in 1:(length(item$levels) - 1)) {
            cg <- add_line(cg, paste0("DIF", i, "=THETA(", next_theta, ")"))
            next_theta <- next_theta + 1
        }
    } else { # type == "binary"
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
    cg <- add_line(cg, "; Exit if dataset contains an ITEM that the model cannot handle")
    cg <- add_line(cg, "EXIT 2")
    cg <- add_line(cg, "; Unreachable code below. There to silence NM-TRAN warning.")
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
    cg <- add_line(cg, "PPRED = P1*1")
    cg <- add_line(cg, "SDPRED = SQRT(P0*(0-PPRED) + P1*(1-PPRED))")
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
    dummy_item <- irt_item(0, "", levels, "ordcat")
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
    cg <- add_line(cg, paste0("PPRED=", item_probability_sum(levels)))
    cg <- add_line(cg, paste0("SDPRED=", item_standard_deviation(levels)))
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

#' Generate a NONMEM code string with a sum of probabilities
#' 
#' @param levels A vector of levels
#' @return A string with NONMEM code for 
item_probability_sum <- function(levels) {
    levels <- levels[levels!=0]
    term_func <- function(level) {
        paste0("P", level, "*", level)
    }
    terms <- sapply(levels, term_func)
    paste(terms, collapse=" + ")
}

#' Generate a NONMEM code string with an stdev calculation
#' 
#' @param levels A vector of levels
#' @return A NONMEM code string with sum P1*(1-PPRED)**2 etc
item_standard_deviation <- function(levels) {
    term_func <- function(level) {
        paste0("P", level, "*(", level, "-PPRED)**2")
    }
    terms <- sapply(levels, term_func)
    paste0("SQRT(", paste(terms, collapse=" + "), ")")
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
    cg <- add_line(cg, "PWRES=(DV-PPRED)/SDPRED")
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
    bin_items <- binary_items(model$scale)
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
    cg <- add_line(cg, paste0("$ESTIMATION METHOD=COND LAPLACE -2LL MAXEVAL=999999 PRINT=1", msfo))
    cg <- add_line(cg, "$COVARIANCE")
    max <- 0
    binary <- ""
    for (item in model$scale$items) {
        if (length(item$levels) > max) {
            max <- length(item$levels)
        }
        if (item$type == "binary") {
            binary <- " GUE "   
        }
    }
    dif_numbers <- seq(1, max - 1)
    cg <- add_line(cg, paste0("$TABLE ID TIME DV", mdv_string(model), "ITEM PSI PPRED PWRES FILE=psi_tab", model$run_number, " NOAPPEND ONEHEADER NOPRINT"))
    columns <- c("DIS", paste0("DIF", dif_numbers), paste0("DIFG", dif_numbers))
    columns_str <- paste(columns, collapse=" ")
    cg <- add_line(cg, paste0("$TABLE ID TIME DV", mdv_string(model), "ITEM ", columns_str, binary))
    cg <- add_line(cg, paste0("       FILE=item_parameters_tab", model$run_number, " NOAPPEND ONEHEADER NOPRINT"))
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
    cg <- add_code(cg, data_and_input_code(model, rewind=TRUE))
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "$MSFI msf4")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, paste0("$SIMULATION (875435432) (3872543 UNIFORM) NOPREDICTION ONLYSIMULATION SUBPROBLEMS=", model$subproblems, " TRUE=FINAL"))
    columns <- paste(model$data_columns, collapse=' ')
    cg <- add_line(cg, paste0("$TABLE ", columns, " FILE=simulation_tab", model$run_number, " FORMAT=,1PE11.4 NOAPPEND ONEHEADER NOPRINT"))
    cg <- add_line(cg, paste0("$TABLE ID ITEM DV PSI", mdv_string(model), "FILE=mirror_plot_tab", model$run_number, " NOAPPEND ONEHEADER NOPRINT"))
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
        cg <- add_line(cg, "$OMEGA 0.1")
    }
    cg
}

#' Generate NONMEM code for simulation of and ordered categorical item
#' 
#' @param scale A scale object
#' @param levels A vector of levels for the item
#' @return A code generator object
ordered_categorical_simulation_code <- function(scale, levels) {
    dummy_item <- irt_item(0, "", levels, "ordcat")
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
        cg <- add_line(cg, "0.1")
    }
    cg
}

#' Generate NONMEM code for initial estimates of thetas
#' 
#' Will use the initial estimates of the scale
#' @param model An irt_model object
#' @return A code generator object
initial_item_thetas <- function(model) {
    #stopifnot('dataset' %in% names(model))
    #df <- read.csv(model$dataset, stringAsFactors=FALSE)
    #initial_thetas_from_dataset(df, model$scale)
    cg <- code_generator()
    cg <- banner_comment(cg, "item parameters")
    i <- 1
    for (base_item in model$base_scale$items) {
        item <- get_item(model$scale, base_item$number) 
        if (is.null(item)) {
            cg <- theta_placeholder(cg, base_item)
        } else {
            if (!is.null(item$inits)) {
                labels <- item_labels(item)
                inits <- item_inits(item)
                if (length(model$consolidation) >= item$number) {   # Does item exist in consolidation list
                    consolidated <- model$consolidation[[item$number]]
                    if (!is.null(consolidated)) {
                        inits <- item_inits(item, consolidated=consolidated)
                    }
                }
                for (line in paste0(inits, "  ; ", labels)) {
                    cg <- add_line(cg, paste0(line, " ", i))    # Or have add_line support arrays
                    i <- i + 1
                }
            } else {
                # Fallback to simple inits for now: FIXME move fallback to item method "item_inits"
                cg <- add_line(cg, paste0("(0,1) ; I", item$number, "DIS"))
                for (i in seq(1, length(item$levels) - 1)) {
                    cg <- add_line(cg, paste0("(0.1) ; I", item$number, "DIF", i))
                }
            }
        }
    }
    cg
}

#' Generate NONMEM code for unused THETAS
#' 
#' Theta placeholders will be used for items that are in the base_scale
#' but not in the scale of a model. This is to keep theta numbering the same
#' for subscales and for full scales
#' 
#' @param cg A code generator object to add lines to
#' @param item An irt item object to add placeholders for
#' @return A new code generator object
theta_placeholder <- function(cg, item) {
    for (dummy in item$levels) {
        cg <- add_line(cg, "0 FIX; THETA PLACEHOLDER")
    }
    cg 
}

# This function is not used and probably broken!
# The purpose was the calculate decent inital estimates given
# a dataset using the mirt package
initial_thetas_from_data <- function(model, df) {
    wide <- df %>%
        prepare_dataset() %>%
        wide_item_data(baseline=TRUE)

    mirt_model <- mirt::mirt(data=wide, model=1, itemtype="graded")
    coeffs <- mirt::coef(mirt_model, IRTpars=TRUE)
    #dataset_scale <- scale_from_dataset(df)
    inits <- list()

    for (item in model$scale$items) {
        if (!(item$number %in% colnames(wide))) {
            inits <- c(inits, list(rep("0 FIX", length(item$levels))))
            next
        }
        scale_levels <- item$levels
        dataset_levels_with_na <- unique(wide[[item$number]])
        dataset_levels <- sort(dataset_levels_with_na[!is.na(dataset_levels_with_na)])
        item_coeffs <- as.numeric(coeffs[[item$number]])
        current_inits <- item_coeffs[1]
        scale_in_dataset <- scale_levels %in% dataset_levels
        rl_encoded <- rle(scale_in_dataset)$values
        if (identical(rl_encoded, c(T))) {
            inits <- c(inits, list(current_inits))
        } else if (identical(rl_encoded, c(T, F))) {
            inits <- c(inits, list(c(current_inits, rep(50, length(scale_levels) - length(dataset_levels)))))
        } else {
            
        }
    }
    inits
}

# Supports filename or data.frame as data
#' Check dataset for missing items or levels
#' 
#' The check will check for the following:
#' 1. Items  present in the dataset but not in the scale
#' 2. Items present in the scale but not in the dataset
#' 3. Levels present in the dataset but not in the scale
#' 4. Levels present in the scale but not in the dataset
#' The results of the check will be printed to the console
#' 
#' @param model_or_data Either a data.frame or a model object from which to take the dataset
#' @param scale If model_or_data was a data.frame a scale must be supplied otherwise the scale from the model object will be taken
#' @export
data_check <- function(model_or_data, scale=NULL) {
    if (class(model_or_data) == "irt_model") {
        dataset <- model_or_data$dataset
        scale <- model_or_data$scale
    } else {
        dataset <- model_or_data
    }

    df <- read_dataset(dataset) %>%
        prepare_dataset()

    all_dataset_items <- unique(df$ITEM)
    all_scale_items <- all_items(scale)
    dataset_in_scale <- all_dataset_items %in% all_scale_items
    if (!all(dataset_in_scale)) {
        missing_items <- all_dataset_items[!dataset_in_scale]
        cat("Items present in dataset but not in scale:", paste(missing_items, collapse=", "), "\n")
    }
    scale_in_dataset <- all_scale_items %in% all_dataset_items
    if (!all(scale_in_dataset)) {
        missing_items <- all_scale_items[!scale_in_dataset]
        cat("Items present in scale but not in dataset:", paste(missing_items, collapse=", "), "\n")
    }

    wide <- wide_item_data(df)
    missing_items <- c()
    for (item in scale$items) {
        n <- as.character(item$number)
        if (n %in% colnames(wide)) {
            dataset_levels <- sort(unique(wide[[n]]))
            scale_levels <- item$levels
            dataset_in_scale <- dataset_levels %in% scale_levels
            if (!all(dataset_in_scale)) {
                missing_levels <- dataset_levels[!dataset_in_scale]
                cat("Levels present in dataset but not in scale for item", n, ":", paste(missing_levels, collapse=", "), "\n")
            }
            scale_in_dataset <- scale_levels %in% dataset_levels
            if (!all(scale_in_dataset)) {
                missing_levels <- scale_levels[!scale_in_dataset]
                cat("Levels present in scale but not in dataset for item", n, ":", paste(missing_levels, collapse=", "), "\n")
            }
        }
    }
}

#' Consolidation of levels in a model without changing the scale
#' 
#' When consolidating levels in the higher end of the levels it is possible to 
#' do it without altering the scale or the model. It is done by setting the inital
#' estimate of the DIF thetas to a high fixed number. This will make the probability
#' of the level to become zero. The benefit of doing this is that the same model can
#' be used for different datasets and different consolidations can be done simply by
#' changing the initial estimates.
#' 
#' @param model An irt_model object
#' @param item_numbers A vector of item numbers to do the same consolidation for
#' @param levels A vector of levels to consolidate, i.e. merging into the lowest level of this vector.
#' @return A new irt_model object
#' @export
consolidate_levels_in_model <- function(model, item_numbers, levels) {
    stopifnot(length(levels) >= 2)
    for (item_number in item_numbers) {
        item <- get_item(model$scale, item_number)
        run <- rle(item$levels %in% levels)$values      # Check that consolidated levels are at an edge of the available levels and consecutive
        if (length(run) == 2) {
            high <- all(run == c(FALSE, TRUE))
            if (high) {
                levels_to_consolidate <- sort(levels)[-1]
                model$consolidation[[item_number]] <- levels_to_consolidate 
            } else {
                stop("Can only consolidate using initial estimates in the upper end of the level range")   
            }
        } else {
            stop("Could only consolidate levels at the low or high end of the level range")
        }
    }
    model
}


#' Get a MDV string for $TABLE if dataset has MDV column
#' 
#' @param model irt_model object
#' @return An empty string or " MDV "
#' @keywords internal
mdv_string <- function(model) {
    if ("MDV" %in% model$data_columns) {
        " MDV "
    } else {
        ""
    }
}