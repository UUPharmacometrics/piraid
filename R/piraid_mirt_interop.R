#' Generate mirt type vector for irt_model object
#'
#' @param model irt_model object 
#' @param wide_data data in wide format
#'
#' @return a vector with item model types for mirt
prepare_mirt_type_vector <- function(model, wide_data){
    types <- c()
    for (item_name in colnames(wide_data)) {
        item <- sub("ITEM_", "", item_name) %>% as.numeric()
        if (get_item(model$scale, item)$type == item_type$ordered_categorical) {
            types <- c(types, "graded")
        } else {
            types <- c(types, "3PL")    # For binary always use 3PL
        }
    }
    types
}


evaluate_mirt_model <- function(model){
    df <- read_dataset(model$dataset) %>% 
        prepare_dataset()
    wide_data <- convert_to_wide_data(df)
    
    item_data_wide <- dplyr::select(wide_data, dplyr::starts_with("ITEM"))
    
    # get item models
    types <- prepare_mirt_type_vector(model, item_data_wide)
    # get required prms
    mirt_prms <- mirt::mirt(data=item_data_wide, model=1, itemtype=types, pars = "values")
    # convert piraid prms to mirt
    item_prms <-  piraid_estimates_to_mirt_format(model$item_parameters) 
    # update prms 
    mirt_prms <- dplyr::left_join(mirt_prms, item_prms, by = c("item","name"), suffix = c("", "_new")) %>% 
        dplyr::mutate(value = ifelse(is.na(.data$value_new), .data$value, .data$value_new)) %>% 
        dplyr::select(-.data$value_new)
    # evaluate model
    mirt_model <- mirt::mirt(data=item_data_wide, model=1, itemtype = types, pars = mirt_prms, TOL = NA)
    return(mirt_model)
}

piraid_estimates_to_mirt_format <- function(prm_df){
    prm_df %>% 
        dplyr::select(.data$item, .data$parameter, .data$init) %>% 
        tidyr::nest(-.data$item) %>% 
        dplyr::mutate(
            item = paste0("ITEM_", .data$item),
            # convert to named vector with MIRT prm names
            data = purrr::map(.data$data, ~set_names(.$init, stringr::str_replace_all(.$parameter, nmirt_to_mirt_name_map))) %>%  
                # translate DIF prms for graded model to accumulative format
                purrr::map_if(~mirt_model_from_prms(.x)=="graded", function(x){
                    dif_prm_index <- stringr::str_detect(names(x), "b\\d+")
                    x[dif_prm_index] <- cumsum(x[dif_prm_index])
                    x
                }) %>% 
                # add upper probability paramater for 3PL model
                purrr::map_if(~mirt_model_from_prms(.x)=="3PL", ~c(.x, u = 1)) %>% 
                purrr::map(~mirt::traditional2mirt(.x, mirt_model_from_prms(.x), ncat_from_prms(.x))) %>% 
                purrr::map(~tibble::enframe(.x))
        ) %>% 
        tidyr::unnest()
}


#' Convert data from wide to long format
#' 
#' This functionc converts a data.frame in wide format to the long format.
#'
#' @param data A data.frame or matrix where each column corresponds to an item and each row
#' to a subject
#'
#' @return A data.frame with one row per subject and item 
#' @export
convert_to_long_nmdata <- function(data){
    item_map <- seq_len(ncol(data)) %>% 
        purrr::set_names(colnames(data))
    tibble::as_data_frame(data) %>% 
        tibble::rownames_to_column("ID") %>%
        dplyr::mutate(
            ID = as.integer(factor(.data$ID))
        ) %>% 
        tidyr::gather("ITEM", "DV", -.data$ID) %>% 
        dplyr::arrange(.data$ID) %>%
        dplyr::mutate(
            ITEM = item_map[.data$ITEM],
            TIME = 0,
            MDV = is.na(.data$DV) %>% as.integer()
        )
}