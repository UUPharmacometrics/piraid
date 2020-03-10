#' Constructor for an IRT scale object
#'
#' \code{irt_scale} returns a new empty scale object
#' @keywords internal
irt_scale <- function() {
    structure(list(items=list()), class="irt_scale")
}

#' Check wheter x is an irt_scale object
#' 
#' @param x An object to test
#' @return True if x is an irt_scale object
#' @export
is.irt_scale <- function(x) {
    inherits(x, "irt_scale")
}

#' Retrieve a list of all built in scales
#' 
#' @return A character vector with the names
#' @export
list_predefined_scales <- function() {
    dir <- system.file("extdata", package="piraid")
    files <- tools::file_path_sans_ext(list.files(dir, pattern = "\\.yaml$"))
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
load_predefined_scale <- function(scale_name) {
    scale_name <- tolower(scale_name)
    path <- system.file("extdata", paste0(scale_name, ".yaml"), package="piraid")
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
    stopifnot(is.irt_scale(scale))
    for (i in 1:(length(scale$items))) {
        scale$items[[i]]$levels <- levels_as_string(scale$items[[i]]$levels)
        if (!is.null(scale$items[[i]]$categories)) {
            scale$items[[i]]$categories <- paste(scale$items[[i]]$categories, collapse=",")
        }
    }
    yaml::write_yaml(scale, path)
}

#' Create subscale
#'
#' \code{create_subscale} Create a new scale with items from selected categories only
#'
#' @param scale An irt_scale object
#' @param categories A vector of category names to include in the new scale
#' @return A new irt_scale object
#' @export
create_subscale <- function(scale, categories) {
    stopifnot(is.irt_scale(scale))
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
#' These functions allow to infer a scale definition from a dataset (provided as data.frame or CSV file) if it follows a general data specification template.  
#' 
#' 
#' In order for the automatic inference to work, the dataset needs to contain the columns ITEM and DV. The function will then use the observed levels for each 
#' item to determine the item type (binary or ordered categorical) while taking MDV and EVID columns into account (records with MDV or EVID not equal to 0 are
#' ignored) . The automatic recognition can be over-written by providing a type column which can specify the item type using the keywords 'ordcat' and 'binary'.
#' These keywords are available through the item_type list, i.e. \code{item_type$ordered_categorical} and \code{item_type$binary}.
#' Additionally, a label for each item can be provided through the column name.
#' 
#' 
#' @param df A data.frame 
#' @param file The path to a csv file
#' @param item Name of the item columns. Default is \code{ITEM}
#' @param dv Name of the dv column. Default is \code{DV}
#' @param name Name of an optional name column. Each item can be given a name (mainly for plotting) through this column. Only the first row of a new item will be used for the name.
#' @param type Name of an optional type column. Each item can be given a type (either \code{item_type$ordered_categorical} or \code{item_type$binary}) through this column. If the type option isn't used the type is 
#' inferred from the number of distinct DV values
#' @return An irt_scale object
#' @export
create_scale_from_df <- function(df, item='ITEM', dv='DV', name=NULL, type=NULL) {
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
        type_of_item <- item_type$ordered_categorical
        
        first_row <- input_df %>% 
            dplyr::filter(!!item_sym == !!item_no) %>% 
            dplyr::slice(1)
        if (!is.null(name)){
            item_name <- first_row[[name]]
        } else {
            item_name <- ""
        }
        if(!is.null(type)){
                type_of_item <- first_row[[type]]
        }else{
            if(length(levels)>2){
                type_of_item <- item_type$ordered_categorical
            }else{
                type_of_item <- item_type$binary
            }
        }
        new_item <- irt_item(as.numeric(item_no), item_name, levels, type_of_item)
        scale <- add_item(scale, new_item)
    }
    scale
}

#' @export
#' @rdname create_scale_from_df 
create_scale_from_csv <- function(file, item='ITEM', dv='DV', name=NULL, type=NULL){
    df <- read_dataset(file, item=item, dv=dv)
    df <- prepare_dataset(df)
    scale <- create_scale_from_df(df, item, dv, name, type)
    scale$source_file <- file
    scale
}

#' Format a vector containing integers to a readable string
#' 
#' The string will use a-b if there is a range of consecutive integers
#' between a and b inclusive. Commas will be used between such groups.
#' A lone integer will be written by it self.
#' 
#' @examples
#' piraid:::format_integers(c(1,2,3,5,7,8)) #will return "1-3, 5, 7-8"
#' 
#' @param x A vector containing integers
#' @return A readable string
format_integers <- function(x) {
    x <- sort(x)
    string <- ""
    partition <- split(x, cumsum(c(1, diff(x) != 1)))
    format_one_partition <- function(x) {
        if (length(x) == 1) {
            as.character(x)
        } else {
            paste0(x[1], "-", x[length(x)])
        }
    }
    paste0(lapply(partition, format_one_partition), collapse=', ')
}

#' Print a short overview of a scale
#' 
#' @param x An irt_scale object
#' @param ... No additional arguments are supported
#' @export
print.irt_scale <- function(x, ...) {
    print_scale_info(x)
}

#' Print a short overview of a scale
#'
#' @param scale An irt_scale object
#' @param header Set to print the header (set to FALSE if called from print.irt_model)
print_scale_info <- function(scale, header=TRUE) {
    stopifnot(is.irt_scale(scale))
    items <- all_items(scale)
    binary_items <- items_by_type(scale, item_type$binary)
    ordcat_items <- items_by_type(scale, item_type$ordered_categorical)
    ordcat_levels <- ordcat_level_arrays(scale)
    if (header) {
        cat("A scale object from ", utils::packageName(), "\n\n", sep="")
    }
    cat("Total number of items: ", length(items), "\n", sep="")
    if(length(ordcat_items)>0)  cat("    Ordered categorical items: ", format_integers(ordcat_items), "\n", sep="")
    if(length(binary_items)>0) cat("    Binary items: ", format_integers(binary_items), "\n", sep="")
}

#' Print a summary overview of a scale
#' 
#' Generate a table with the columns Item, Levels, Type, Categories and Name to give an
#' overview of what a scale object contains.
#' 
#' @param object An irt_scale object
#' @param ... No additional arguments are supported
#' @return A data.frame with scale information
#' @export
summary.irt_scale <- function(object, ...) {     # ... needed to match the generic function
    scale <- object
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

#' Add or replace an item to a scale
#'
#' \code{add_item} returns a new scale with item added
#' Will not add items with less than 2 levels and warn instead.
#' Will not add items with the same number as item already in scale unless replace is set to TRUE
#'
#' @param scale An irt_scale object
#' @param item An irt_item to add to scale
#' @param replace Set to overwrite an already existing item
#' @return A new scale with the item added
#' @keywords internal
add_item <- function(scale, item, replace=FALSE) {
    if (!is.null(get_item(scale, item$number))) {
        if (!replace) {
            warning(paste0("Item ", item$number, " is already present in the scale and will not be added."))
        } else {
            i <- get_item_index(scale, item$number)
            scale$items[[i]] <- item
        }
    } else if (length(item$levels) < 2) {
        warning(paste0("Item ", item$number, " has only 1 level and will not be added to the scale."))
    } else if (length(item$levels) != 2 && item$type == item_type$binary) {
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
    stopifnot(is.irt_scale(scale))
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

#' Get a sorted array of sizes of ordered categorical items
#' 
#' Will give a list of all unique number of items for all ordcat items
#' 
#' @param scale A scale object
#' @keywords internal
ordcat_levels <- function(scale) {
    levels <- c()
    for (item in scale$items) {
        if (item$type == item_type$ordered_categorical) {
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
        if (item$type == item_type$ordered_categorical) {
            levels[[i]] <- item$levels
            i = i + 1
        }
    }

    unique_levels <- unique(levels)

    # Sort the level arrays so that the shortest comes first
    new_order <- unique_levels %>%
        purrr::map(length) %>%
        purrr::flatten_int() %>%
        order()

    unique_levels[new_order]
}

#' A list enumerating all supported item types
#' 
#' @examples
#' item_type$binary
#' 
#' @export
item_type <- list(binary="binary", ordered_categorical="ordcat")


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
    stopifnot(type == item_type$ordered_categorical || type == item_type$binary)
    structure(list(number=number, name=name, levels=levels, type=type, categories=categories, inits=inits), class="irt_item")
}

#' Get vector of levels from character description or vector of levels
#' 
#' If a vector of levels is input it will simply be output as it is
#' If a string is input it will be parsed to get a list of all levels it describes
#' Allowed formats are [a,b] for an interval and (1,2,3,4) to explicitly write all possible levels
#' @examples 
#' levels <- piraid:::item_levels("[1,3]")   # levels will be 1, 2 and 3.
#' levels <- piraid:::item_levels("(0,1,2,3,4)")
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



#' Get published initial estimate for a parameter of an item
#' 
#' @param scale An irt_scale object
#' @param item_number The number of an item
#' @param parameter_index The index of a parameter
#' @return The initial estimate or NULL if not available
#' @keywords internal
published_init <- function(scale, item_number, parameter_index) {
    item <- get_item(scale, item_number)
    if ("inits" %in% names(item)) {
        if (!is.na(parameter_index)) {
            return(item$inits[parameter_index])
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

#' Calculate the minimum and maximum total score for a scale
#' 
#' @param scale An irt_scale object
#' @return Vector of min and max total score
#' @export
total_score_range <- function(scale) {
    lower <- 0
    upper <- 0
    for (item in scale$items) {
        lower <- lower + min(item$levels)
        upper <- upper + max(item$levels)
    }
    c(lower, upper)
}

max_level <- function(scale) {
    levels <- ordcat_level_arrays(scale)
    max <- 1
    for (level_array in levels) {
        if (length(level_array) - 1 > max) {
            max <- length(level_array) - 1
        }
        if (max(level_array) > max) {
            max <- max(level_array)
        }
    }
    max
}