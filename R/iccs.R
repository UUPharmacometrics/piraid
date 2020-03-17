
#' Calculate and plot item characteristic curves
#' 
#' These functions calculate and plot the item characteristic curves for the model specified. The results are returned either 
#' as a data.frame (\code{calculate_iccs}) or visualized in a plot (\code{plot_iccs}). 
#'
#' @param model An irt_model object
#' @param psi_range A vector of lenght 2 specifying the lower and upper boundary for the latent variable
#'
#' @return A data.frame or a plot 
#' @export
calculate_iccs <- function(model, psi_range = c(-4, 4)) {
    mirt_model <- as_mirt_model(model)
    item_names <- get_mirt_names(model)
    level_labels <- purrr::map(model$scale$items, "levels") %>% 
        purrr::flatten_int() %>% 
        unique() %>% 
        sort() %>% 
        sprintf("P(Y=%i)", .)
    theta <- seq(psi_range[1], psi_range[2], length.out = 100)
    df <- purrr::set_names(item_names, item_names) %>% 
        purrr::map(~ mirt::extract.item(mirt_model, .x)) %>%
        purrr::map_dfr(
            ~ mirt::probtrace(.x, theta) %>% 
                {magrittr::set_colnames(., paste0("Prob", seq_len(ncol(.))))} %>% 
                tibble::as_tibble() %>%
                dplyr::mutate(
                    psi = theta
                ),
            .id = "item"
        ) %>% 
        tidyr::pivot_longer(tidyselect::starts_with("Prob"), 
                            names_to = "category",
                            values_to = "probability",
                            values_drop_na = TRUE) %>% 
        dplyr::mutate(
            item = factor(.data$item, 
                          levels = item_names, 
                          labels = item_name_list(model$scale)),
            category = factor(.data$category, 
                              levels = sort(unique(.data$category)), 
                              labels = level_labels)
        )
    return(df)
} 

#' @export
#' @rdname calculate_iccs
plot_iccs <- function(model, ...){
    df <- calculate_iccs(model, ...)
    ggplot2::ggplot(df, aes(psi, probability, color = category))+
        ggplot2::geom_line()+
        ggplot2::xlab("PSI")+
        ggplot2::ylab("Probability")+
        ggplot2::scale_color_discrete("")+
        ggplot2::facet_wrap(vars(item))+
        ggplot2::theme(legend.position = "top")
}