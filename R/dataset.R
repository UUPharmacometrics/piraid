# Filter out non-observations from a dataset
filter_observations <- function(df) {
    if ('MDV' %in% colnames(df)) {
        df <- dplyr::filter(df, UQ(sym("MDV")) == 0)
    }
    if ('EVID' %in% colnames(df)) {
        df <- dplyr::filter(df, UQ(sym("EVID")) == 0)
    }
    df
}

prepare_dataset <- function(df) {
    df %>%
        filter_observations() %>%
        dplyr::mutate("DV"=as.numeric(replace(UQ(sym("DV")), UQ(sym("DV"))=='.', '0')))
}

# Convert data to wide form with one column per item
wide_item_data <- function(df, baseline=FALSE) {
    grouped <- dplyr::group_by(df, UQ(sym("ID")))
    if (baseline) {
        filtered <- dplyr::filter(grouped, UQ(sym("TIME")) == min(UQ(sym("TIME"))))
    } else {
        filtered <- grouped
    }
    filtered %>%
        dplyr::group_by(UQ(sym("ID")), UQ(sym("TIME"))) %>%
    dplyr::select("ID", "ITEM", "DV", "TIME") %>%
    dplyr::distinct(UQ(sym("ITEM")), .keep_all=TRUE) %>%
    tidyr::spread(UQ(sym("ITEM")), UQ(sym("DV"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"ID", -"TIME")
}

# Can handle bypassing a data.frame
# Reads in a dataset, checks for mandatory columns and renames columns to standard names
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
