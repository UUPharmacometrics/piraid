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


add_line <- function(generator, line) {
    generator$code = c(generator$code, indent_line(line, generator$indent_level))
    generator
}

add_empty_line <- function(generator) {
    add_line(generator, "")
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
    end <- strrep("-", 80 - 25 - nchar(s))
    string <- paste0(";", start, s, end)
    add_line(generator, string)
}
