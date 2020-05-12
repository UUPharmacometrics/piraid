#' Estimate item parameter
#' 
#' This function estimates the item parameters in a model using the \code{mirt} package. The dataset and the item models 
#' are taken from the provided model object.  
#' 
#' The available strategies of how to use the longitudinal data include 'baseline' (the default), 'all-pooled' and 'all-baseref'.
#' 
#' With the 'baseline' strategy only the baseline data is used for the estimation and all other data is ignored.  
#' 
#' The 'all-pooled' strategy uses all available data and treats each visit as a separate subject during estimation.
#' 
#' The 'all-baseref' strategy also uses all available data but defines the population at baseline as a reference (mean 0, variance 1). 
#' Observations from later timepoints are treated as separate subjects.
#' 
#' @param model The model
#' @param data_use_strategy The method of how to use the available longitudinal data
#' 
#' @return A data frame with the estimated item parameters 
#'
#' @export
estimate_item_parameters <- function(model, data_use_strategy = "baseline"){
    stopifnot(is.irt_model(model))
    data_use_strategy <- rlang::arg_match(data_use_strategy, data_use_strategies)
    
    df <- read_dataset(model$dataset) %>% prepare_dataset()
    wide_data <- convert_to_wide_data(df)
    
    if(data_use_strategy == 'baseline') wide_data <- dplyr::group_by(wide_data, .data$ID) %>% dplyr::slice(1) %>% dplyr::ungroup()
    if(data_use_strategy == 'all-baseref') isbaseline <- factor(duplicated(wide_data$ID), levels = c(T,F)) 
    
    wide_data <- dplyr::select(wide_data, dplyr::starts_with("ITEM"))
    
    #consolidate levels in data as specified by model
    for (i in seq_along(model$consolidation)) {
        if(!is.null(model$consolidation[[i]])){
            new_value <- min(model$consolidation[[i]]) - 1 
            wide_data[[i]] <- ifelse(wide_data[[i]] %in% model$consolidation[[i]], new_value, wide_data[[i]])
        }
    }

    required_levels <- model$scale$items %>% 
        purrr::map("levels") %>% 
        purrr::imap(~setdiff(.x, purrr::pluck(model$consolidation, .y, .default = c()))) %>% 
        purrr::map(as.integer)

    # check if all levels are available in the data
    all_equal <- wide_data %>% 
        dplyr::summarise_all(~list(unique(.))) %>% 
        purrr::transpose() %>% 
        purrr::flatten() %>% 
        purrr::map(stats::na.exclude) %>% 
        purrr::map(sort) %>% 
        purrr::map(as.integer) %>% 
        purrr::map2(required_levels, ~identical(.x,.y)) %>% 
        purrr::reduce(.init = TRUE, `&`)
    if(!all_equal) stop("The data provided does not contain all levels of the scale. Can not estimate item parameters.", call. = F)
    rlang::inform(paste("Using data with", ncol(wide_data), "items and", nrow(wide_data), "subjects"))
    types <- prepare_mirt_type_vector(model, wide_data)
    rlang::inform("Starting item paramter estimation using 'mirt'")
    if(data_use_strategy == 'all-baseref'){
        mirt_group_model <- mirt::multipleGroup(data=wide_data, model = 1, itemtype=types, group = isbaseline, 
                                          invariance=c('slopes', 'intercepts', 'free_var','free_means'), verbose = interactive())
        mirt_model <- mirt::extract.group(mirt_group_model, 1)
    }else{
        mirt_model <- mirt::mirt(data=wide_data, model=1, itemtype=types, verbose = interactive())
    }
    rlang::inform("Estimation done")

    coef_list <- mirt::coef(mirt_model, IRTpars=TRUE)
    mirt_estimates_to_nmirt_format(coef_list)
}

#' @rdname estimate_item_parameters
#' @format NULL
#' @docType NULL
#' @keywords NULL
#' @eval paste0("@usage data_use_strategies \n#", deparse(data_use_strategies))
#' @export
data_use_strategies <- c("baseline", "all-pooled", 'all-baseref')


#' Estimate Latent Variable Values
#' 
#' The function uses the data associated with the model to estimate a latent variable value for each time point.
#' 
#' @param model The model
#'
#' 
#' @return A tibble with columns PSI (the estimated latent variable value), PSI_SE (the associated standard error), and all columns 
#' from the data (except ITEM and DV)
#'
#' @export
estimate_lv_values <- function(model){
    stopifnot(is.irt_model(model))
    df <- read_dataset(model$dataset) %>% prepare_dataset()
    wide_data <- convert_to_wide_data(df)
    
    item_data_wide <- dplyr::select(wide_data, dplyr::starts_with("ITEM"))
    non_item_data_wide <- dplyr::select(wide_data, -dplyr::starts_with("ITEM"))

    mirt_model <- as_mirt_model(model, use_data = TRUE)
    rlang::inform("Estimating latent variable values for all subjects using 'mirt'")
    lv_values <- mirt::fscores(mirt_model, method = "MAP", full.scores.SE = TRUE)
    rlang::inform("Latent variable estimation done")
    
    lv_values %>% 
        dplyr::as_tibble() %>% 
        dplyr::rename(PSI = .data$F1, PSI_SE = .data$SE_F1) %>% 
        dplyr::bind_cols(non_item_data_wide, .)

}



mirt_to_nmirt_name_map <- c(a = "DIS", b = "DIF", g = "GUE")
nmirt_to_mirt_name_map <- purrr::set_names(names(mirt_to_nmirt_name_map), mirt_to_nmirt_name_map)

mirt_estimates_to_nmirt_format <- function(estimates_list){
    estimates_list %>% 
        purrr::map(~ .[1, ]) %>% 
        purrr::map_if(~rlang::has_name(., "b1"), ~c(.[1:2], diff(.[-1]))) %>% # translate DIF parameters to NM format DIF(x) = DIF(x) - DIF(x-1)
        purrr::map_dfr(~tibble::tibble( parameter=names(.x), value = .x), .id = "item") %>% 
        dplyr::filter(!(.data$parameter=="u"|.data$item=="GroupPars")) %>% 
        dplyr::mutate(parameter = stringr::str_replace_all(.data$parameter, mirt_to_nmirt_name_map),
                      item = stringr::str_extract(.data$item, "\\d+") %>% as.integer())
} 

nmirt_estimates_to_mirt_format <- function(df){
    df %>% 
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

# get mirt model name from prm names
mirt_model_from_prms <- function(prms){
    if(all(rlang::has_name(prms, c("a","b","g")))) return("3PL")
    if(all(rlang::has_name(prms, c("a","b1")))) return("graded")
    rlang::abort("Could not infer model from parameter names.")
}

# get the number of categories from the prm names
ncat_from_prms <- function(prms){
    if(all(rlang::has_name(prms, c("a","b","g")))){
        return(2)
    }else{
        npar <- names(prms) %>% 
            stringr::str_extract("\\d+") %>% 
            as.integer() %>% 
            max(na.rm=T)  
        return(npar + 1)
    } 
}
