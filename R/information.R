
#' Plot item information curves
#' 
#' The function plots the item information curves for the model specified
#'
#' @param model An irt_model object
#' @param psi_range A vector of lenght 2 specifying the lower and upper boundary for the latent variable
#'
#' @return A plot object
#' @export
plot_item_information_curves <- function(model, psi_range = c(-4,4)){
    mirt_fit <- evaluate_mirt_model(model)
    item_names <- item_name_list(model$scale)
    theta <- seq(psi_range[1], psi_range[2], length.out = 100)
    p <- seq_along(item_names) %>% 
        purrr::map(~mirt::extract.item(mirt_fit, .x)) %>% 
        purrr::set_names(nm = item_names) %>% 
        purrr::map_dfr(~mirt::iteminfo(.x, theta) %>% tibble::tibble(information = ., psi = theta), .id = "item") %>% 
        dplyr::mutate(item = factor(item, levels = item_names)) %>% 
        ggplot2::ggplot(ggplot2::aes(psi, information))+
        ggplot2::geom_line()+
        ggplot2::xlab("PSI")+
        ggplot2::ylab("Information")+
        ggplot2::facet_wrap(vars(item))
    return(p)
}
