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
        new_irt_item <- irt_item(number=item$number, levels=item_levels(item$levels), type=item$type)
        scale <- add_item(scale, new_irt_item)
    }
    scale
}

# Determine the scale from a dataset using the ITEM, DV and MDV columns
# df can be a data.frame or a filename
scale_from_dataset <- function(df, item='ITEM', dv='DV') {
    if (is.character(df)) {
        df <- read.csv(df)
    }
    scale <- irt_scale()
    if ('MDV' %in% colnames(df)) {
        df <- dplyr::filter(df, MDV == 0)
    }
    df <- dplyr::select(df, !!item, !!dv)
    distinct <- dplyr::group_by_(df, item) %>% dplyr::distinct_(dv) %>% dplyr::summarise(DV=list(!!rlang::sym(dv)))
    for (i in  1:nrow(distinct)) {
        item_no <- distinct[i, 'ITEM']
        levels <- distinct[i, 'DV'][[1]][[1]]
        new_item <- irt_item(as.numeric(item_no), levels, "ordcat")     # Assuming ordered categorical here
        scale <- add_item(scale, new_item)
    }
    scale
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
            levels <- c(levels, length(item$levels)) 
        }
    }
    sort(unique(levels))
}

# Get a list of unique ordered categorical level arrays
ordcat_level_arrays <- function(scale) {
    levels <- list()
    i = 1
    for (item in scale$items) {
        if (item$type == "ordcat") {
            levels[[i]] <- item$levels
        }
        i = i + 1
    }
    unique(levels)
}


irt_item <- function(number, levels, type) {
    structure(list(number=number, levels=levels, type=type), class="irt_item")
}



# x can either be a string or an array of levels
item_levels <- function(x) {
    if (is.numeric(x)) {
        levels <- x
    } else {
        m <- stringr::str_match(x,'\\[(\\d+),(\\d+)\\]')
        if (!is.na(m[1,2])) {
            levels <- seq(m[1,2], m[1,3])
        } else {
            # FIXME: Error check here
            m <- stringr::str_extract_all(x, '\\d+', simplify=TRUE)
            if (length(m) > 0) {
                levels <- as.numeric(m)
            }
        }
    }
    levels
}

levels_as_string <- function(levels) {
    x <- levels
    y <- seq(x[1], x[1] + length(x) - 1)
    if (isTRUE(all.equal(x, y))) {  # We have consecutive list and can use [] notation
        s <- paste0('[', x[1], ',', x[length(x)], ']')
    } else {    # Need to use () notation
        s <- paste0('(', paste(as.character(x), collapse=","), ')')
    }
}
