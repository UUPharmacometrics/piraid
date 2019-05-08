#' Get a vector of item numbers 
#' 
#' These function return a vector of item numbers, either for all items (\code{all_items}), all items with a certain type (\code{items_by_type}) 
#' or all items in a certain category (\code{items_in_categories}) in a scale or model.  
#' 
#' @param o An irt_scale or irt_model object
#' @return A vector of item numbers
#' @export
all_items <- function(o) UseMethod("all_items")

#' @rdname all_items
#' @export
all_items.irt_scale <- function(o) {
    a <- c()
    for (item in o$items) {
        a <- c(a, item$number)
    }
    a
}

#' @rdname all_items 
#' @export
all_items.irt_model = function(o) return(all_items(o$scale))



#' Get a vector of all items of certain type
#' 
#' @rdname all_items
#' @param types A vector of item types
#' @export
items_by_type <- function(o, types) UseMethod("items_by_type")

#' @rdname all_items 
#' @export
items_by_type.irt_scale <- function(o, types) {
    items <- c()
    for (item in o$items) {
        if (item$type %in% types) {
            items <- c(items, item$number)   
        }
    }
    items
}
#' @rdname all_items 
#' @export
items_by_type.irt_model <- function(o, types) return(items_by_type(o$scale, types))


#' @rdname all_items 
#' @param categories A vector of names of categories
#' @export
items_in_categories <- function(o, categories) UseMethod("items_in_categories")


#' @rdname all_items 
#' @export
items_in_categories.irt_scale <- function(o, categories) {
    new_scale <- create_subscale(o, categories)
    items <- c()
    for (item in new_scale$items) {
        items <- c(items, item$number)
    }
    items
}

#' @rdname all_items 
#' @export
items_in_categories.irt_model <- function(o, categories) return(items_in_categories(o$scale, categories))