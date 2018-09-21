irt_scale <- function() {
    structure(list(items=list()), class="irt_scale")
}

predefined_scale <- function(scale_name) {
    scale <- irt_scale()
    scale_name <- tolower(scale_name)
    path <- system.file("extdata", paste0(scale_name, ".yaml"), package="nmIRT")
    if (path == "") {
        stop("Error: No such predefined scale. Available scale is UPDRS")
    }
    db <- yaml::read_yaml(path)
    for (item in db$items) {
        new_irt_item <- irt_item(number=item$number, levels=item$levels, type=item$type)
        scale <- add_item(scale, new_irt_item)
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
            levels <- c(levels, item$levels) 
        }
    }
    sort(unique(levels))
}


irt_item <- function(number, levels, type) {
    structure(list(number=number, levels=levels, type=type), class="irt_item")
}
