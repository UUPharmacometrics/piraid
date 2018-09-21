irt_scale <- function() {
    structure(list(items=list()), class="irt_scale")
}

predefined_scale <- function(scale_name) {
    scale_name <- tolower(scale_name)
    path <- system.file("extdata", paste0(scale_name, ".yaml"), package="nmIRT")
    if (path == "") {
        stop("Error: No such predefined scale. Available scale is UPDRS")
    }
    load_scale(path)
}

load_scale <- function(filename) {
    scale <- irt_scale()
    db <- yaml::read_yaml(filename)
    for (item in db$items) {
        new_irt_item <- irt_item(number=item$number, levels=item$levels, type=item$type)
        scale <- add_item(scale, new_irt_item)
    }
    scale
}

# Determine the scale from a dataset using the ITEM, DV and MDV columns
scale_from_dataset <- function(filename, item='ITEM', dv='DV') {
    df <- read.csv(filename)
    if ('MDV' %in% colnames(df)) {
        df <- dplyr::filter(df, MDV == 0)
    }
    df <- dplyr::select(df, !!item, !!dv)
    distinct <- dplyr::group_by_(df, item) %>% distinct_(dv) %>% arrange_(item, dv)
}

add_item <- function(scale, item) {
    scale$items <- c(scale$items, list(item))
    scale
}

# Get a sorted array of ordered categorical levels
ordcat_levels <- function(scale) {
    levels <- c()
    for (item in scale$items) {
        if (item$type == "ordcat") {
            levels <- c(levels, item$levels) 
        }
    }
    sort(unique(levels))
}


irt_item <- function(number, levels, type) {
    structure(list(number=number, levels=levels, type=type), class="irt_item")
}
