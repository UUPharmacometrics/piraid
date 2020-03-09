# Abstract base class for all types of Models

#' @keywords internal
base_model <- function(subclass = NULL) {
    structure(
        list(
            scale = NULL,
            simulation = FALSE,
            run_number = 1,
            ignore_statements = c(),
            use_data_path = TRUE,
            simulation_options = "",
            estimation_options = ""
        ),
        class = c(subclass, "base_model")
    )
}

#' Check wheter x is a base_model object
#' 
#' @param x An object to test
#' @return True if x is an base_model object
is.base_model <- function(x) {
    inherits(x, "base_model")
}

#' Generate code for model
#' 
#' @param model A model object
#' @export
model_code <- function(model) UseMethod("model_code")

#' Print NONMEM code
#' 
#' Output the NONMEM code of a model object to the console
#' 
#' @param model A model object
#' @export
print_model_code <- function(model) {
    stopifnot(is.base_model(model))
    cat(model_code(model))
}

#' Save NONMEM code
#' 
#' Saves the NONMEM code of a model object to a file
#' 
#' @param model A model object
#' @param path Path to the created model file
#' @export
save_model_code <- function(model, path) {
    stopifnot(is.base_model(model))
    str <- model_code(model)
    fp <- file(path)
    writeLines(str, fp)
    close(fp)
}

#' Set dataset
#' 
#' This function sets the dataset of a model (i.e., $DATA) as well as the data columns in $INPUT.
#' 
#' The function will open the dataset to extract the column names for $INPUT. The dataset is expected to be in CSV format without special characters
#' in the header. 
#' 
#' @param model A model object
#' @param path Path to a dataset file
#' @param use_path Should the path be put in $DATA or not? If FALSE only the filename will go into $DATA
#' @param data_columns Optional character vector of data columns for $INPUT to use instead of the dataset header
#' @return A new model object
#' @export
set_dataset <- function(model, path, use_path=TRUE, data_columns=NULL) UseMethod("set_dataset")

#' @export
set_dataset.default <- function(model, path, use_path=TRUE, data_columns=NULL) {
    stopifnot(is.base_model(model))
    model$dataset <- path
    model$use_data_path <- use_path
    if(is.null(data_columns)){
        df <- utils::read.csv(model$dataset, nrows=0)
        model$data_columns <- colnames(df)
    }else{
        model$data_columns <- data_columns
    }
    model
}

#' Add simulation
#' 
#' Add a number of simulations to the model
#' 
#' @param model A model object
#' @param nsim The number of simulations to add
#' @param options Extra options to add to $SIM as one string
#' @return A new model object
#' @export
add_simulation <- function(model, nsim=1, options="") {
    stopifnot(is.base_model(model))
    model$simulation <- TRUE
    model$subproblems <- nsim
    model$simulation_options <- options
    model
}
#' Add extra options to $ESTIMATION
#' 
#' @param model A model object
#' @param options Extra options to add to $EST as one string
#' @return A new model object
#' @export
add_estimation_options <- function(model, options) {
    stopifnot(is.base_model(model))
    model$estimation_options <- options
    model
}

#' Set the run number
#' 
#' The run number will be added to the names of the table files
#' 
#' @param model A model object
#' @param run_number An integer
#' @export
set_run_number <- function(model, run_number) {
    stopifnot(is.base_model(model))
    model$run_number <- run_number
    model
}

#' Generate a NONMEM code string with a sum of probabilities
#' 
#' @param levels A vector of levels
#' @return A string with NONMEM code for the sum 
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

ppred_code <- function(levels) {
    paste0("PPRED=", item_probability_sum(levels))
}

sdpred_code <- function(levels) {
    paste0("SDPRED=", item_standard_deviation(levels))
}

pwres_code <- function() {
    "PWRES = (DV - PPRED) / SDPRED"
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
        if (model$use_data_path) {
            data_path <- normalizePath(model$dataset)
        } else {
            data_path <- basename(model$dataset)    # Only use filename
        }
        cg <- add_line(cg, paste0("$INPUT ", paste(model$data_columns, collapse=' ')))
        cg <- add_line(cg, paste0("$DATA ", data_path, rewind_code, " IGNORE=@"))
        if(!rlang::is_empty(model$ignore_statements)){
            cg <- add_line(cg, paste(model$ignore_statements))    
        }
    }
    cg
}