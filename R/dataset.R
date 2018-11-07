# Filter out non-observations from a dataset
filter_observations <- function(df) {
    if ('MDV' %in% colnames(df)) {
        df <- dplyr::filter(df, MDV == 0)
    }
    if ('EVID' %in% colnames(df)) {
        df <- dplyr::filter(df, EVID == 0)
    }
    df
}

prepare_dataset <- function(df) {
    df %>%
        filter_observations() %>%
        dplyr::mutate(DV=as.numeric(replace(DV, DV=='.', '0')))
}

# Convert data to wide form with one column per
wide_item_data <- function(df, baseline=FALSE) {
    grouped <- dplyr::group_by(df, ID)
    if (baseline) {
        filtered <- dplyr::filter(grouped, TIME == min(TIME))
    } else {
        filtered <- grouped
    }
    filtered %>%
        dplyr::group_by(ID, TIME) %>%
    dplyr::select(ID, ITEM, DV, TIME) %>%
    dplyr::distinct(ITEM, .keep_all=TRUE) %>%
    tidyr::spread(ITEM, DV) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID, -TIME)
}

# Can handle bybassing a data.frame
read_dataset <- function(filename) {
    if (is.character(filename)) {
        read.csv(filename, stringsAsFactors=FALSE)
    } else if (is.data.frame(filename)) {
        filename
    } else {
        stop("Input to read_dataset is neither a filename nor a data.frame")
    }
}