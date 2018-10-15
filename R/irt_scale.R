irt_scale <- function() {
    structure(list(items=list()), class="irt_scale")
}

#' Retrieve a built in scale
#' 
#' \code{predefined_scale} returns a scale object from a build in scale
#'
#' @param scale_name The name of the scale
#' @return A scale object or an error if the scale does not exist
predefined_scale <- function(scale_name) {
    scale_name <- tolower(scale_name)
    path <- system.file("extdata", paste0(scale_name, ".yaml"), package="nmIRT")
    if (path == "") {
        stop("Error: No such predefined scale. Available scale is MDS-UPDRS")
    }
    load_scale(path)
}

#' Load a scale from file
#'
#' \code{load_scale} loads a scale file from disk and returns a scale object
#'
#' @param filename
#' @return A scale object
load_scale <- function(filename) {
    scale <- irt_scale()
    db <- yaml::read_yaml(filename)
    for (item in db$items) {
        new_irt_item <- irt_item(number=item$number, levels=item_levels(item$levels), type=item$type, categories=strsplit(item$categories, ",")[[1]])
        scale <- add_item(scale, new_irt_item)
    }
    scale
}

#' Save a scale to file
#'
#' \code{save_scale} saves a scale object to a file
#'
#' @param scale
#' @param filename
save_scale <- function(scale, filename) {
    for (i in 1:(length(scale$items))) {
        scale$items[[i]]$levels <- levels_as_string(scale$items[[i]]$levels)
        if (!is.null(scale$items[[i]]$categories)) {
            scale$items[[i]]$categories <- paste(scale$items[[i]]$categories, collapse=",")
        }
    }
    yaml::write_yaml(scale, filename)
}

#' Select categories
#'
#' \code{select_categories} get a new scale with items from selected categories only
#'
#' @param scale
#' @param categories
select_categories <- function(scale, categories) {
    new_scale <- irt_scale()
    for (item in scale$items) {
        if (any(categories %in% item$categories)) {
            new_scale <- add_item(new_scale, item)
        }
    }
    new_scale
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
    if ('EVID' %in% colnames(df)) {
        df <- dplyr::filter(df, EVID == 0)
    }
    df <- dplyr::select(df, !!item, !!dv)
    distinct <- dplyr::group_by_(df, item) %>% dplyr::distinct_(dv) %>% dplyr::summarise(DV=list(!!rlang::sym(dv)))
    for (i in  1:nrow(distinct)) {
        item_no <- distinct[i, 'ITEM']
        levels <- sort(distinct[i, 'DV'][[1]][[1]])
        new_item <- irt_item(as.numeric(item_no), levels, "ordcat")     # Assuming ordered categorical here
        scale <- add_item(scale, new_item)
    }
    scale
}

#' Get an item from a scale
#' 
#' \code{get_item} returns the item with the specified number (id)
#'
#' @param scale The scale
#' @param number The item number to retrieve
#' @return The item with corresponding number or NULL if not found
#' @keywords internal
get_item <- function(scale, number) {
    for (item in scale$items) {
        if (item$number == number) {
            return(item)
        }
    }
    NULL
}

#' Add an item to a scale
#'
#' \code{add_item} returns a new scale with item added
#' Will not add items with less than 2 levels and warn instead.
#' Will not add items with the same number as item already in scale
#'
#' @param scale An irt_scale object
#' @param item An irt_item to add to scale
#' @return A new scale with the item added
#' @keywords internal
#' @examples
#' scale <- predefined_scale("MDS-UPDRS")
#' item <- irt_item(99, c(1,2,3), "ordcat")
#' scale <- add_item(scale, item)
add_item <- function(scale, item) {
    if (!is.null(get_item(scale, item$number))) {
        warning(paste0("Item ", item$number, " is already present in the scale and will not be added."))
    } else if (length(item$levels) < 2) {
        warning(paste0("Item ", item$number, " has only 1 level and will not be added to the scale."))
    } else {
        scale$items <- c(scale$items, list(item))
    }
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


irt_item <- function(number, levels, type, categories=NULL) {
    structure(list(number=number, levels=levels, type=type, categories=categories), class="irt_item")
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
