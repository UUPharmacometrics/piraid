code_generator <- function() {
    structure(list(indent_level=0, code=c()), class="code_generator")
}

indent_line <- function(line, indent_level) {
    paste0(strrep(" ", 4 * indent_level), line)
}

add_code <- function(generator, generator2) {
    code <- sapply(generator2$code, function(line) { indent_line(line, generator$indent_level) })
    generator$code = c(generator$code, code)
    generator
}


add_line <- function(generator, ...) {
    dots <- dots_list(...) 
    if(is.null(dots$sep)) dots$sep <- ""                          
    line <- rlang::exec(paste, !!!dots)
    generator$code = c(generator$code, indent_line(line, generator$indent_level))
    generator
}

add_lines <- function(generator, lines) {
    for (line in lines) {
        generator <- add_line(generator, line)
    }
    generator
}

add_empty_line <- function(generator) {
    generator$code = c(generator$code, "")
    generator
}

increase_indent <- function(generator) {
    generator$indent_level = generator$indent_level + 1
    generator
}

decrease_indent <- function(generator) {
    generator$indent_level = generator$indent_level - 1
    generator
}

get_code <- function(generator) {
    code <- paste(generator$code, collapse='\n')
    paste0(code, '\n')
}

banner_comment <- function(generator, s) {
    start <- strrep("-", 25)
    trailing_dashes <- 80 - 25 - nchar(s)
    if (trailing_dashes > 0) {
        end <- strrep("-", 80 - 25 - nchar(s))
    } else {
        end <- ""
    }
    string <- paste0(";", start, s, end)
    add_line(generator, string)
}

# comment out all code in generator
comment_code <- function(generator) {
    generator$code <- paste(";", generator$code)
    generator
}

#' Join strings together into lines. Each line cannot be longer than max.
#' 
#' @param strs A vector of strings to join together.
#' @param sep Separator for joining the strings.
#' @param max Maximum number of characters for each line.
#' @return A vector of strings. Each element represents a line of text.
join_with_max_length <- function(strs, sep=' ', max=100) {
    lines <- c()
    current_line <- ""
    first <- TRUE
    for (i in 1:length(strs)) {
        current_string <- strs[i]
        if (first) {    # Always add a string first to a line even if it is to long
            current_line <- current_string
            first <- FALSE
        } else if (nchar(current_line) + nchar(current_string) + nchar(sep) <= max) {
            current_line <- paste0(current_line, sep, current_string)
        } else {
            lines <- c(lines, current_line)
            current_line <- current_string
        }
    }
    if (nchar(current_line) > 0) {
        lines <- c(lines, current_line)
    }
    lines
}
