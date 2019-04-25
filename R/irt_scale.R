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
#' \code{scale_from_df} and \code{scale_from_csv} determine the scale from a dataset (provided as data.frame or csv file) 
#' using the ITEM, DV and MDV columns.
#' 
#' @param df A data.frame 
#' @param file The path to a csv file
#' @param item Name of the item columns. Default is \code{ITEM}
#' @param dv Name of the dv column. Default is \code{DV}
#' @param name Name of an optional name column. Each item can be given a name (mainly for plotting) through this column. Only the first row of a new item will be used for the name.
#' @param type Name of an optional type column. Each item can be given a type (either ordcat or binary) through this column. If the type option isn't used the type is 
#' inferred from the number of distinct DV values
#' @return An irt_scale object
#' @export
scale_from_df <- function(df, item='ITEM', dv='DV', name=NULL, type=NULL) {
    scale <- irt_scale()
    item_sym <- rlang::sym(item)
    dv_sym <- rlang::sym(dv)
    input_df <- df
    df <- filter_observations(df)
    df <- dplyr::select(df, !!item_sym, !!dv)
    distinct <- dplyr::group_by(df, !!item_sym) %>% 
        dplyr::distinct(!!dv_sym) %>% 
        dplyr::summarise(DV=list(!!dv_sym))
    for (i in  1:nrow(distinct)) {
        item_no <- as.numeric(distinct[i, item])
        levels <- sort(distinct[i, 'DV'][[1]][[1]])
        item_type <- "ordcat"
        
        first_row <- input_df %>% 
            dplyr::filter(!!item_sym == !!item_no) %>% 
            dplyr::slice(1)
        if (!is.null(name)){
            item_name <- first_row[[name]]
        } else {
            item_name <- ""
        }
        if(!is.null(type)){
                item_type <- first_row[[type]]
        }else{
            if(length(levels)>2){
                item_type <- "ordcat"
            }else{
                item_type <- "binary"
            }
        }
        new_item <- irt_item(as.numeric(item_no), item_name, levels, item_type)
        scale <- add_item(scale, new_item)
    }
    scale
}

#' @export
#' @rdname scale_from_df 
scale_from_csv <- function(file, item='ITEM', dv='DV', name=NULL, type=NULL){
    df <- read_dataset(file, item=item, dv=dv)
    df <- prepare_dataset(df)
    scale <- scale_from_df(df, item, dv, name, type)
    scale$source_file <- file
    scale
}

#' Print an overview of a scale
#' 
#' Generate a table with the columns Item, Levels, Type, Categories and Name to give an
#' overview of what a scale object contains.
#' 
#' @param scale An irt_scale object
#' @return A data.frame with scale information
#' @export
scale_overview <- function(scale) {
    n <- length(scale$items)
    df <- data.frame(Item=rep(as.numeric(NA), n), Levels=rep("", n), Type=rep("", n), Categories=rep("", n), Name=rep("", n), stringsAsFactors=FALSE)
    i <- 1
    for (item in scale$items) {
        if (length(item$categories) > 0) {
            categories <- paste(item$categories, collapse=NULL)
        } else {
            categories <- ""
        }
        name <- gsub("\\n", " ", item$name)
        df[i, ] <- list(item$number, levels_as_string(item$levels), item$type, categories, name)
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
#' Consolidate, i.e. merge levels together, of a certain items in a scale object. Consolidated items will be removed
#' from the scale.
#' 
#' @param scale An irt_scale object
#' @param item_numbers A vector of item numbers to apply the same consolidation to
#' @param levels The levels to be merged together
#' @return A new irt_scale object
#' @export
consolidate_levels <- function(scale, item_numbers, levels) {
    stopifnot(length(levels) >= 2)
    for (item_number in item_numbers) {
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
    stopifnot(type == "ordcat" || type == "binary")
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

#' Get a vector of all items of certain type
#' 
#' @param scale An irt_scale object
#' @param types A vector of types (currently "binary" and "ordcat")
#' @export
items_by_type <- function(scale, types) {
    items <- c()
    for (item in scale$items) {
        if (item$type %in% types) {
            items <- c(items, item$number)   
        }
    }
    items
}

#' Get vector of parameter names of an item
#' 
#' @param scale An irt_scale object
#' @param item_number An item number
#' @return A character vector of parameter names
#' @export
item_parameter_names <- function(scale, item_number) {
    item <- get_item(scale, item_number)
    if (item$type == "ordcat") {
        dif_names <- paste0("DIF", 1:(length(item$levels) - 1))
        c("DIS", dif_names)
    } else {    # Currently binary
        c("DIS", "DIF", "GUE")
    }
}

#' Get the index of an item parameter relative to the first parameter of that item
#' 
#' DIS would have index 1, DIF1 would have 2 etc
#' 
#' @param scale An irt_scale object
#' @param item_number Number of the item
#' @param parameter The name of the parameter
#' @return The index of the parameter  starting from 1 or NA if not found
item_parameter_index <- function(scale, item_number, parameter) {
    names <- item_parameter_names(scale, item_number)
    match(parameter, names)
}

#' Get published initial estimate for a parameter of an item
#' 
#' @param scale An irt_scale object
#' @param item_number The number of an item
#' @param parameter Name of a parameter
#' @return The initial estimate or NULL if not available
#' @keywords internal
published_init <- function(scale, item_number, parameter) {
    item <- get_item(scale, item_number)
    if ("inits" %in% names(item)) {
        index <- item_parameter_index(scale, item_number, parameter)
        if (!is.na(index)) {
            return(item$inits[index])
        }
    }
    NULL
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

#' Get a list of all items in one or more categories
#' 
#' @param scale An irt_scale object
#' @param categories A vector of names of categories
#' @return A vector of item numbers
#' @export
items_in_categories <- function(scale, categories) {
    new_scale <- select_categories(scale, categories)
    items <- c()
    for (item in new_scale$items) {
        items <- c(items, item$number)
    }
    items
}