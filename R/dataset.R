# Filter out non-observations from a dataset
filter_observations <- function(df) {
    if ('MDV' %in% colnames(df)) {
        df <- dplyr::filter(df, .data$MDV == 0)
    }
    if ('EVID' %in% colnames(df)) {
        df <- dplyr::filter(df, .data$EVID == 0)
    }
    df
}

prepare_dataset <- function(df) {
    df %>%
        filter_observations() %>%
        dplyr::mutate("DV"=as.numeric(replace(.data$DV, .data$DV == '.', '0')))
}

# Convert data to wide form with one column per item
wide_item_data <- function(df, baseline=FALSE) {
    grouped <- dplyr::group_by(df, .data$ID)
    if (baseline) {
        filtered <- dplyr::slice(grouped, 1)
    } else {
        filtered <- grouped
    }
    filtered %>%
        dplyr::group_by(.data$ID, .data$TIME) %>%
    dplyr::select("ID", "ITEM", "DV", "TIME") %>%
    dplyr::distinct(.data$ITEM, .keep_all=TRUE) %>%
    tidyr::spread(.data$ITEM, .data$DV) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"ID", -"TIME")
}

convert_to_wide_data <- function(df){
    df %>% 
        dplyr::as_tibble() %>% 
        tidyr::spread(.data$ITEM, .data$DV, sep = "_")
}

# Can handle bypassing a data.frame
# Reads in a dataset, checks for mandatory columns and renames columns to standard names
# TODO: maybe this should be split into read_data & rename_columns
read_dataset <- function(filename, id='ID', time='TIME', item='ITEM', dv='DV') {
    if (is.character(filename)) {
        df <- utils::read.csv(filename, stringsAsFactors=FALSE)
    } else if (is.data.frame(filename)) {
        df <- filename
    } else {
        stop("Input to read_dataset is neither a filename nor a data.frame")
    }
    
    cols <- colnames(df)
    replace(cols, cols==id, 'ID')
    replace(cols, cols==time, 'TIME')
    replace(cols, cols==item, 'ITEM')
    replace(cols, cols==dv, 'DV')
    mandatory_columns <- c('ID', 'TIME', 'ITEM', 'DV')
    if (!all(mandatory_columns %in% cols)) {
        stop(paste0("Mandatory columns ", paste(cols[!(mandatory_columns %in% cols)], collapse=", "), " not present in dataset"))
    }
    colnames(df) <- cols
    df
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
check_data <- function(model_or_data, scale=NULL) UseMethod("check_data")

#' @rdname check_data
#' @export
check_data.data.frame <- function(model_or_data, scale=NULL) {
    mismatch_found <- FALSE
    df <- read_dataset(model_or_data) %>%
        prepare_dataset()
    
    all_dataset_items <- unique(df$ITEM)
    all_scale_items <- all_items(scale)
    dataset_in_scale <- all_dataset_items %in% all_scale_items
    if (!all(dataset_in_scale)) {
        missing_items <- all_dataset_items[!dataset_in_scale]
        mismatch_found <- TRUE
        cat("Items present in dataset but not in scale:", paste(missing_items, collapse=", "), "\n")
    }
    scale_in_dataset <- all_scale_items %in% all_dataset_items
    if (!all(scale_in_dataset)) {
        missing_items <- all_scale_items[!scale_in_dataset]
        mismatch_found <- TRUE
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
                mismatch_found <- TRUE
                cat("Levels present in dataset but not in scale for item", n, ":", paste(missing_levels, collapse=", "), "\n")
            }
            scale_in_dataset <- scale_levels %in% dataset_levels
            if (!all(scale_in_dataset)) {
                missing_levels <- scale_levels[!scale_in_dataset]
                mismatch_found <- TRUE
                cat("Levels present in scale but not in dataset for item", n, ":", paste(missing_levels, collapse=", "), "\n")
            }
        }
    }
    if(!mismatch_found) {
        cat("The dataset is in agreement with the scale definition", "\n")
        invisible(TRUE)
    }else{
        invisible(FALSE)
    }
}

#' @rdname check_data
#' @export
check_data.character <- function(model_or_data, scale = NULL) {
    df <- read_dataset(model_or_data)
    check_data(df, scale)
}

#' @rdname check_data
#' @export
check_data.irt_model <- function(model_or_data, scale=NULL) {
    filename <- model_or_data$dataset
    scale <- model_or_data$scale
    check_data(filename, scale)
}