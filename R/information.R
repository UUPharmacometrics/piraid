
#' Calculate item information curves
#' 
#' These functions calculate the item information curves for the model specified. The results are returned either 
#' as a data.frame (\code{calculate_item_information_curves}) or visualized in a plot (\code{plot_item_information_curves}). 
#'
#' @param model An irt_model object
#' @param psi_range A vector of lenght 2 specifying the lower and upper boundary for the latent variable
#'
#' @return A data.frame or a plot 
#' @export
calculate_item_information_curves <- function(model, psi_range = c(-4,4)){
    mirt_fit <- as_mirt_model(model)
    item_names <- item_name_list(model$scale)
    theta <- seq(psi_range[1], psi_range[2], length.out = 100)
    df <- seq_along(item_names) %>% 
        purrr::map(~mirt::extract.item(mirt_fit, .x)) %>% 
        purrr::set_names(nm = item_names) %>% 
        purrr::map_dfr(~mirt::iteminfo(.x, theta) %>% tibble::tibble(information = ., psi = theta), .id = "item") %>% 
        dplyr::mutate(item = factor(.data$item, levels = item_names)) 
    return(df)
    
}

#' @export
#' @rdname calculate_item_information_curves
plot_item_information_curves <- function(model, psi_range = c(-4,4)){
    p <- calculate_item_information_curves(model, psi_range =  psi_range) %>% 
        ggplot2::ggplot(ggplot2::aes_string("psi", "information"))+
        ggplot2::geom_line()+
        ggplot2::xlab("PSI")+
        ggplot2::ylab("Information")+
        ggplot2::facet_wrap(~item)
    return(p)
}


#' Calculate the item information in a population
#' 
#' These functions calculate the population item information for the model specified. The results are returned either 
#' as a data.frame (\code{calculate_population_item_information}) or visualized in a plot (\code{plot_population_item_information}). 
#'
#' @param model An irt_model object
#' @param mean mean of the population
#' @param var variance of the population
#'
#' @return A data.frame or a plot 
#' @export
calculate_population_item_information <- function(model, mean = 0, var = 1){
    mirt_fit <- evaluate_mirt_model(model)
    item_names <- item_name_list(model$scale)
    df <- seq_along(item_names) %>% 
        purrr::map(~mirt::extract.item(mirt_fit, .x)) %>%
        purrr::map(~function(psi) dnorm(psi, mean, sd =sqrt(var))*mirt::iteminfo(.x, psi)) %>% 
        purrr::map(~integrate(.x, -Inf, Inf)) %>% 
        purrr::set_names(nm = item_names) %>%
        purrr::map_dbl("value") %>% 
        tibble::enframe("item", "information") %>% 
        dplyr::mutate(item = factor(.data$item, levels = item_names)) 
    return(df)
}

#' @rdname calculate_population_item_information
#' @export
plot_population_item_information <- function(model, mean = 0, var = 1){
    p <- calculate_population_item_information(model, mean, var) %>% 
        ggplot2::ggplot(ggplot2::aes_string("item", "information"))+
        ggplot2::geom_col()+
        ggplot2::xlab("")+
        ggplot2::ylab("Information")
    return(p)   
}