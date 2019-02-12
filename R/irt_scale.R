#' Constructor for an IRT scale object
#'
#' \code{irt_scale} returns a new empty scale object
#' @keywords internal
irt_scale <- function() {
    structure(list(items=list()), class="irt_scale")
}

#' Retrieve a list of all built in scales
#' 
#' @return A character vector with the names
#' @export
list_predefined_scales <- function() {
    dir <- system.file("extdata", package="nmIRT")
    files <- tools::file_path_sans_ext(list.files(dir))
    return(files)
}

#' Retrieve a built in scale
#' 
#' \code{predefined_scale} returns a scale object from a built in scale.
#' Use \code{\link{list_predefined_scales}} to get a list of all available scales.
#'
#' @param scale_name The name of the scale
#' @return A scale object or an error if the scale does not exist
#' @export
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
#' @param path Path to the file to read
#' @return A scale object
#' @export
load_scale <- function(path) {
    scale <- irt_scale()
    db <- yaml::read_yaml(path)
    for (item in db$items) {
        if (is.null(item$categories)) {
            categories <- c()
        } else {
            categories <- strsplit(item$categories, ",")[[1]]
        }
        name <- item$name
        if (is.null(name)) {
            name <- ""
        }
        new_irt_item <- irt_item(number=item$number, name=name, levels=item_levels(item$levels),
            type=item$type, categories=categories, inits=item$inits$values)
        scale <- add_item(scale, new_irt_item)
    }
    scale
}

#' Save a scale to file
#'
#' \code{save_scale} saves a scale object to a file
#'
#' @param scale An irt_scale object
#' @param path Path to the file to save the scale
#' @export
save_scale <- function(scale, path) {
    for (i in 1:(length(scale$items))) {
        scale$items[[i]]$levels <- levels_as_string(scale$items[[i]]$levels)
        if (!is.null(scale$items[[i]]$categories)) {
            scale$items[[i]]$categories <- paste(scale$items[[i]]$categories, collapse=",")
        }
    }
    yaml::write_yaml(scale, path)
}

#' Select categories
#'
#' \code{select_categories} Create a new scale with items from selected categories only
#'
#' @param scale An irt_scale object
#' @param categories A vector of category names to include in the new scale
#' @return A new irt_scale object
#' @export
select_categories <- function(scale, categories) {
    new_scale <- irt_scale()
    for (item in scale$items) {
        if (any(categories %in% item$categories)) {
            new_scale <- add_item(new_scale, item)
        }
    }
    new_scale
}

#' Infer a scale from a dataset
#' 
#' \code{scale_from_dataset} determines the scale from a dataset using the ITEM, DV and MDV columns.
#' All items will be assumed to be of type ordered categorical
#' 
#' @param df Can either be a data.frame or a path to a csv file
#' @param item Name of the item columns. Default is \code{ITEM}
#' @param dv Name of the dv column. Default is \code{DV}
#' @return An irt_scale object
#' @export
scale_from_dataset <- function(df, item='ITEM', dv='DV') {
    if (is.character(df)) {
        df <- utils::read.csv(df)
    }
    scale <- irt_scale()
    df <- filter_observations(df)
    df <- dplyr::select(df, !!item, !!dv)
    distinct <- dplyr::group_by_(df, item) %>% dplyr::distinct_(dv) %>% dplyr::summarise(DV=list(!!rlang::sym(dv)))
    for (i in  1:nrow(distinct)) {
        item_no <- distinct[i, item]
        levels <- sort(distinct[i, 'DV'][[1]][[1]])
        new_item <- irt_item(as.numeric(item_no), "", levels, "ordcat")     # Assuming ordered categorical here
        scale <- add_item(scale, new_item)
    }
    scale
}

#' Print an overview of a scale
#' 
#' Generate a table with the columns Item, Levels, Type and Categories to give an
#' overview of what a scale object contains.
#' 
#' @param scale An irt_scale object
#' @return A data.frame with scale information
#' @export
scale_overview <- function(scale) {
    n <- length(scale$items)
    df <- data.frame(Item=rep(as.numeric(NA), n), Levels=rep("", n), Type=rep("", n), Categories=rep("", n), stringsAsFactors=FALSE)
    i <- 1
    for (item in scale$items) {
        if (length(item$categories) > 0) {
            categories <- paste(item$categories, collapse=NULL)
        } else {
            categories <- ""
        }
        df[i, ] <- list(item$number, levels_as_string(item$levels), item$type, categories)
        i <- i + 1
    }
    df
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
add_item <- function(scale, item, overwrite=FALSE) {
    if (!is.null(get_item(scale, item$number))) {
        warning(paste0("Item ", item$number, " is already present in the scale and will not be added."))
    } else if (length(item$levels) < 2) {
        warning(paste0("Item ", item$number, " has only 1 level and will not be added to the scale."))
    } else if (length(item$levels) != 2 && item$type == "binary") {
        warning(paste0("Item ", item$number, " is of type binary, but does not have exactly 2 levels. Will not be added to scale."))
    } else {
        scale$items <- c(scale$items, list(item))
    }
    scale
}

#' Remove one or more items from a scale
#' 
#' @param scale An irt_scale object
#' @param numbers A vector of the item numbers to remove
#' @return A new irt_scale object with the items removed
#' @export
remove_items <- function(scale, numbers) {
    for (n in numbers) {
        for (i in seq(1, length(scale$items))) {
            if (scale$items[[i]]$number == n) {
                scale$items[[i]] <- NULL
                break
            }
        }
    }
    scale
}

#' Gets the vector index of an item
#' 
#' @param scale An irt_scale object
#' @param number The item number of an item
#' @return The vector index of the scale$item vector
#' @keywords internal
get_item_index <- function(scale, number) {
    for (i in seq(1, length(scale$items))) {
        if (scale$items[[i]]$number == number) {
            return(i)
        }
    }
    NULL
}

#' Consolidate item levels
#'
#' Consolidate, i.e. merge levels together, of a certain item in a scale object. Consolidated items will be removed
#' from the scale.
#' 
#' @param scale An irt_scale object
#' @param item_number The number of an item
#' @param levels The levels to be merged together
#' @return A new irt_scale object
#' @export
consolidate_levels <- function(scale, item_number, levels) {
    stopifnot(length(levels) >= 2)
    item <- get_item(scale, item_number)
    run <- rle(item$levels %in% levels)$values      # Check that consolidated levels are at an edge of the available levels and consecutive
    if (length(run) == 2) {
        low <- all(run == c(TRUE, FALSE))
        high <- all(run == c(FALSE, TRUE))
    }
    if (length(run) == 2 && (low || high)) {
        if (low) {
            levels_to_remove <- sort(levels)[-length(levels)]
        } else {
            levels_to_remove <- sort(levels)[-1]
        }
        index <- get_item_index(scale, item_number)
        scale$items[[index]]$levels <- setdiff(item$levels, levels_to_remove)
    } else {
        stop("Could only consolidate levels at the low or high end of the level range")
    }
    scale
}

#' Get a sorted array of sizes of ordered categorical items
#' 
#' Will give a list of all unique number of items for all ordcat items
#' 
#' @param scale A scale object
#' @keywords internal
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
            i = i + 1
        }
    }
    unique(levels)
}

#' Constructor for the irt_item class
#' 
#' @param number Item number
#' @param name A descriptive name of the item
#' @param levels A vector of levels for this item
#' @param type The item type (binary or ordcat)
#' @param categories An optional character vector of names of categories
#' @param inits Option vector of initial values for the item parameters
#' @keywords interal
irt_item <- function(number, name, levels, type, categories=NULL, inits=NULL) {
    structure(list(number=number, name=name, levels=levels, type=type, categories=categories, inits=inits), class="irt_item")
}

#' Get vector of levels from character description or vector of levels
#' 
#' If a vector of levels is input it will simply be output as it is
#' If a string is input it will be parsed to get a list of all levels it describes
#' Allowed formats are [a,b] for an interval and (1,2,3,4) to explicitly write all possible levels
#' @examples 
#' levels <- nmIRT:::item_levels("[1,3]")   # levels will be 1, 2 and 3.
#' levels <- nmIRT:::item_levels("(0,1,2,3,4)")
#' @param x Can either be a string or an array of levels
#' @return A sorted array of levels
#' @keywords internal
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
    sort(levels)
}

#' Take a vector of levels and produce a description string usable for presentation or output to scale file
#' 
#' @param levels A vector of levels
#' @return A string describing the levels
#' @keywords internal
levels_as_string <- function(levels) {
    x <- levels
    y <- seq(x[1], x[1] + length(x) - 1)
    if (isTRUE(all.equal(x, y))) {  # We have consecutive list and can use [] notation
        s <- paste0('[', x[1], ',', x[length(x)], ']')
    } else {    # Need to use () notation
        s <- paste0('(', paste(as.character(x), collapse=","), ')')
    }
}

#' Get a vector of all item numbers from scale
#' 
#' @param scale An irt_scale object
#' @return A vector of all item numbers
#' @export
all_items <- function(scale) {
    a <- c()
    for (item in scale$items) {
        a <- c(a, item$number)
    }
    a
}

#' Get a list of all items of binary type from a scale
#' 
#' @param scale An irt_scale object
#' @return A vector of item numbers for all binary items
#' @export
binary_items <- function(scale) {
    a <- c()
    for (item in scale$items) {
        if (item$type == "binary") {
            a <- c(a, item$number)
        }
    }
    a
}

#' Get vector of labels for parameters of item
#' 
#' @param item An irt_item object
#' @return A label vector
#' @keywords interal
item_labels <- function(item) {
    if (item$type == "ordcat") {
        dis_label <- paste0("I", item$number, "DIS")
        dif_labels <- paste0("I", item$number, "DIF", 1:(length(item$levels) - 1))
        labels <- c(dis_label, dif_labels)
    } else {    # Currently binary
        dis_label <- paste0("I", item$number, "DIS")
        dif_label <- paste0("I", item$number, "DIF")
        gue_label <- paste0("I", item$number, "GUE")
        labels <- c(dis_label, dif_label, gue_label)
    }
    labels
}

#' Give a theta init string from limits
#'
#' init=0 gives 0 FIX
#'  
#' @param init The initial estimate
#' @param lower The lower bound (not mandatory)
#' @param upper The upper bound (not mandatory)
#' @return A theta init string for NONMEM
#' @keywords internal
theta_init <- function(init, lower, upper) {
    if (init == 0) {
        return("0 FIX")
    }

    if (missing(upper) && missing(lower)) {
        return(paste0(init))
    } else if (missing(upper)) {
        return(paste0("(", lower, ", ", init, ")"))
    } else {
        return(paste0("(", lower, ", ", init, ", ", upper, ")"))
    }
}

#' Create initial estimates string for item
#' 
#' @param item An item object
#' @return A vector of initial estimates definitions for NONMEM
#' @keywords internal
item_inits <- function(item, consolidated=NULL) {
    if (item$type == "ordcat") {
        dis_init <- theta_init(item$inits[1], 0)
        dif1_init <- item$inits[2]
        if (length(item$inits) > 2) {
            dif_rest_inits <- paste0("(0,", item$inits[-1:-2], ",50)")
            if (!is.null(consolidated)) {
                n <- length(consolidated)
                dif_rest_inits[(length(dif_rest_inits) - (n - 1)):length(dif_rest_inits)] <- "50 FIX"
            }
        } else {
            dif_rest_inits <- c()
        }
        inits <- c(dis_init, dif1_init, dif_rest_inits)
    } else {    # Binary
        dis_init <- theta_init(item$inits[1], 0)
        dif_init <- theta_init(item$inits[2], -50, 50)
        gue_init <- theta_init(item$inits[3], 0)
        inits <- c(dis_init, dif_init, gue_init)
    }
    inits
}

#' Create a vector of item labels with item numbers as names of entries
#' 
#' @param scale An irt_scale object
#' @return A vector with item numbers as row names
#' @keywords internal
item_name_list <- function(scale) {
    numbers <- c()
    item_names <- c()
    for (item in scale$items) {
        item_names <- c(item_names, item$name)
        numbers <- c(numbers, item$number)
    }
    names(item_names) <- numbers
    item_names  
}