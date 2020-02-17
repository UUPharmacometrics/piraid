# Abstract base class for all types of Models

#' @keywords internal
base_model <- function(subclass=NULL) {
    structure(list(simulation=FALSE, run_number=1, use_data_path=TRUE, simulation_options="", estimation_options=""), class=c(subclass, "base_model"))
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
set_dataset <- function(model, path, use_path=TRUE, data_columns=NULL) {
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