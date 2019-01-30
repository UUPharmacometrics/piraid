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
    df <- filter_observations(df)
    df <- dplyr::select(df, !!item, !!dv)
    distinct <- dplyr::group_by_(df, item) %>% dplyr::distinct_(dv) %>% dplyr::summarise(DV=list(!!rlang::sym(dv)))
    for (i in  1:nrow(distinct)) {
        item_no <- distinct[i, 'ITEM']
        levels <- sort(distinct[i, 'DV'][[1]][[1]])
        new_item <- irt_item(as.numeric(item_no), NULL, levels, "ordcat")     # Assuming ordered categorical here
        scale <- add_item(scale, new_item)
    }
    scale
}

# Get an overview of a scale
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
#' @examples
#' scale <- predefined_scale("MDS-UPDRS")
#' item <- irt_item(99, c(1,2,3), "ordcat")
#' scale <- add_item(scale, item)
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

# Remove one or more items from a scale
remove_items <- function(scale, number) {
    for (n in number) {
        for (i in seq(1, length(scale$items))) {
            if (scale$items[[i]]$number == n) {
                scale$items[[i]] <- NULL
                break
            }
        }
    }
    scale
}

get_item_index <- function(scale, number) {
    for (i in seq(1, length(scale$items))) {
        if (scale$items[[i]]$number == number) {
            return(i)
        }
    }
    NULL
}

#' 
#' Consolidated levels will also be consolidated after simulations
#'
consolidate_levels <- function(scale, item_number, levels) {
    stopifnot(length(levels) >= 2)
    item <- get_item(scale, item_number)
    run <- rle(item$levels %in% levels)$values      # Check that consolidated levels are at an edge of the available levels and consecutive
    low <- all(run == c(TRUE, FALSE))
    high <- all(run == c(FALSE, TRUE))
    if (length(run) == 2 && (low || high)) {
        if (low) {
            levels_to_remove <- sort(levels)[-length(levels)]
        } else {
            levels_to_remove <- sort(levels)[-1]
        }
        index <- get_item_index(scale, item_number)
        scale$items[[index]]$levels <- setdiff(item$levels, levels_to_remove)
    } else {
        error("Could only consolidate levels at the low or high end of the level range")
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
            i = i + 1
        }
    }
    unique(levels)
}


irt_item <- function(number, name, levels, type, categories=NULL, inits=NULL) {
    structure(list(number=number, name=name, levels=levels, type=type, categories=categories, inits=inits), class="irt_item")
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

# Get array of all item numbers from scale
all_items <- function(scale) {
    a <- c()
    for (item in scale$items) {
        a <- c(a, item$number)
    }
    a
}

# Get list of all unique items of binary type
unique_binary_items <- function(scale) {
    items <- list()
    for (item in scale$items) {
        if (item$type == "binary") {
            item$number = 1
            items <- c(items, list(item))
        }
    }
    unique(items)
}

# Get array of labels for parameters of item
item_labels <- function(item) {
    if (item$type == "ordcat") {
        dis_label <- paste0("I", item$number, "DIS")
        dif_labels <- paste0("I", item$number, "DIF", 1:(length(item$levels - 1)))
        labels <- c(dis_label, dif_labels)
    } else {    # Currently binary
        dis_label <- paste0("I", item$number, "DIS")
        dif_label <- paste0("I", item$number, "DIF")
        gue_label <- paste0("I", item$number, "GUE")
        labels <- c(dis_label, dif_label, gue_label)
    }
    labels
}

# Give a theta init string from limits
# init=0 gives 0 FIX
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

# Create initial estimates string for item
item_inits <- function(item) {
    if (item$type == "ordcat") {
        dis_init <- theta_init(item$inits[1], 0)
        dif1_init <- item$inits[2]
        if (length(item$inits) > 2) {
            dif_rest_inits <- paste0("(0,", item$inits[-1:-2], ",50)")
        } else {
            dif_rest_inits <- c()
        }
        inits <- c(dis_init, dif1_init, dif_rest_inits)
    } else {    # Binary
        dis_init <- theta_init(item$inits[1], 0)
        dif_init <- theta_init(item$inits[2])
        gue_init <- theta_init(item$inits[3], 0)
        inits <- c(dis_init, dif_init, gue_init)
    }
    inits
}

# Create an array of item labels with item numbers as names of entries
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