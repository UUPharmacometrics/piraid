#' Estimate item parameter
#' 
#' This function estimates the item parameters in a model using the \code{mirt} package. The dataset and the item models 
#' are taken from the provided model object.  
#' 
#' The available strategies of how to use the longitudinal data include 'baseline' (the default) and 'visits-as-subjects'.
#' 
#' With the 'baseline' strategy only the baseline data is used for the estimation and all other data is ignored.  
#' 
#' The 'visits-as-subjects' strategy uses all available data and treats each visit as a seperate subject during estimation.
#' 
#' @param model The model
#' @param data_use_strategy The method of how to use the available longitudinal data
#' 
#' @return A dataframe with the estimated item parameters 
#'
#' @export
estimate_item_parameters <- function(model, data_use_strategy = "baseline"){
    data_use_strategy <- rlang::arg_match(data_use_strategy)
    
    df <- read_dataset(model$dataset) %>% prepare_dataset()
    wide_data <- convert_to_wide_data(df)
    
    if(data_use_strategy == 'baseline') wide_data <- dplyr::group_by(wide_data, .data$ID) %>% dplyr::slice(1) %>% dplyr::ungroup()
    
    
    wide_data <- dplyr::select(wide_data, starts_with("ITEM"))
    rlang::inform(paste("Using data with", ncol(wide_data), "items and", nrow(wide_data), "subjects"))
    types <- c()
    for (item_name in colnames(wide_data)) {
        item <- sub("ITEM_", "", item_name) %>% as.numeric()
        if (get_item(model$scale, item)$type == item_type$ordered_categorical) {
            types <- c(types, "graded")
        } else {
            types <- c(types, "3PL")    # For binary always use 3PL
        }
    }
    rlang::inform("Starting item paramter estimation using 'mirt'")
    mirt_model <- mirt::mirt(data=wide_data, model=1, itemtype=types)
    rlang::inform("Estimation done")
    coef_list <- mirt::coef(mirt_model, IRTpars=TRUE, simplify =T)
    prm_names <- colnames(coef_list$items) 
    ncat <- mirt::extract.mirt(mirt_model, "K")
    item_prms <- coef_list$items
    
    # translate DIF parameters to NM format DIF(x) = DIF(x) - DIF(x-1)
    for(item_index in seq_along(ncat)){
        if(ncat[item_index]<=2) next 
        for(dif_index in (ncat[item_index]-1):2){
            dif_prm <- paste0("b", dif_index)
            pdif_prm <- paste0("b", dif_index-1)
            item_prms[item_index, dif_prm] <- item_prms[item_index, dif_prm] - item_prms[item_index, pdif_prm]
        }
    }
    
    item_prms %>% 
        dplyr::as_tibble(rownames = "item") %>% 
        dplyr::mutate(item = sub("ITEM_", "", item) %>% as.integer()) %>% 
        dplyr::rename(
            DIS = a,
            GUE = g
        ) %>% 
        dplyr::rename_at(vars(starts_with("b")), ~sub("^b", "DIF", .)) %>% 
        dplyr::select(-u) %>% 
        tidyr::gather("parameter", "value", -item) %>%
        dplyr::filter(!is.na(value)) %>% 
        dplyr::arrange(item)
}

#' @rdname estimate_item_parameters
#' @export
data_use_strategies <- c("baseline", "visits-as-subjects")
