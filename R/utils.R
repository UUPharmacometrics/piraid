#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Update existing rows or insert new ones
#'
#' Update the rows in old to the ones in new if they exist, otherwise insert them as new rows. Rows are matched by the columns provided in id_cols.
#'
#' @param old the data.frame to update
#' @param new the data.frame with the updated values
#' @param id_cols columns used to join 
#' @param only_existing_cols whether to allow new columns to be created
#'
#' @return data.frame with updated values
#'
update_or_insert <- function(old, new, id_cols, only_existing_cols = TRUE){
    if(only_existing_cols) new <- dplyr::select(new, !!!intersect(colnames(old),colnames(new)))
    old <- old %>% 
        dplyr::mutate(.index = seq_len(dplyr::n())) # add index column to keep track of ordering
    # determine all entries that will not change and update index column
    unchanged <- dplyr::anti_join(old, new, by=id_cols) %>%
        dplyr::arrange(.index) %>%
        dplyr::mutate(.index = seq_len(dplyr::n()))
    # generate index for updated or added entries
    changed <- dplyr::mutate(new, .index = seq_len(dplyr::n())+nrow(unchanged))
    # determined added and updated entries
    added <- dplyr::anti_join(changed, old, by=id_cols)
    updated <- dplyr::semi_join(changed, old, by=id_cols)
    # determine columns in old that were not provided in new
    missing_cols <- dplyr::semi_join(old, changed, by=id_cols) %>% 
        dplyr::select(!!!id_cols, !!!setdiff(colnames(old), colnames(new)), -.index)
    updated <- dplyr::full_join(updated, missing_cols, by = id_cols)
    # warn if entries will be updated
    if(nrow(updated)!=0) rlang::inform(paste(nrow(updated) ,"existing row(s) updated"))
    # combine and sort by index
    dplyr::bind_rows(unchanged, added, updated) %>%
        dplyr::arrange(.index) %>% 
        dplyr::select(-.index)
}