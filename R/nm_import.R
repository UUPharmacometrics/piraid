#' Read item paramter values from NM table
#'
#' @param model An irt_model object
#' @param nmtab_file path to the table file
#'
#' @return A data frame with item parameters 
#' @export
read_item_parameters_from_nmtab <- function(model, nmtab_file){
    if (!requireNamespace("xpose", quietly = TRUE)) {
        stop("Package \"xpose\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    output_table <- xpose::read_nm_tables(nmtab_file, quiet = T)
    if(!"ITEM" %in% colnames(output_table)) stop("The column ITEM needs to be available in the table.")
    
    item_prms <- dplyr::filter(output_table, !duplicated(.data$ITEM)) %>% 
        dplyr::select("ITEM", dplyr::matches("DIS|DIF|GUE")) %>% 
        tidyr::gather("parameter", "value", -.data$ITEM)
    
    required_prms <- purrr::map_dfr(all_items(model), ~tibble::tibble(ITEM = .x, parameter = item_parameter_names(model, .x)))
    
    dplyr::left_join(required_prms, item_prms, by = c("ITEM", "parameter")) %>% 
        dplyr::rename(item = .data$ITEM)
}