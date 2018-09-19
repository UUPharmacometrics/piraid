irt_model <- function() {
    structure(list(), class="irt_model")
}

render <- function(x) UseMethod("render")

render.irt_model <- function(x) {
    cat("Will render now!")
}


code_generator <- function() {
    structure(list(indent_level=0, code=c()), class="code_generator")
}

add_line <- function(generator, line) {
    generator$code = c(generator$code, paste0(strrep(" ", generator$indent_level * 4), line))
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
    paste(generator$code, collapse='\n')
}


irt_item <- function(type="ordered categorical", number, levels) {
    structure(list(type=type, number=number, levels=levels), class="irt_item")
}

irt_item_assignment_code <- function(item, next_theta) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("IF(ITEM.EQ.", item$number, ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, paste0("MODEL=OC", item$levels))
    cg <- add_line(cg, paste0("DIS=THETA(", next_theta, ")"))
    next_theta <- next_theta + 1
    for (i in 1:(item$levels - 1)) {
        cg <- add_line(cg, paste0("DIF", i, "=THETA(", next_theta, ")"))
        next_theta <- next_theta + 1
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    get_code(cg)
}

banner_comment <- function(s) {
    start <- strrep("-", 25)
    end <- sttrep("-", 80 - 25 - length(s))
    paste0(";", start, s, end)
}

ordered_categorical_data_model_code <- function(levels) {
    cg <- code_generator()
    cg <- add_line(cg, banner_comment(paste0("ordered categorical data model with ", levels, " levels")))
    cg <- add_line(cg, paste0("IF(MODEL.EQ.OC", levels, ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, "DIFG1=DIF1")
    for (i in 1:levels - 1) {
        cg <- add_line(cg, paste0("DIFG", i + 1, "=DIFG", i, "+DIF", i + 1))
    }
    cg <- add_empty_line(cg)
    for (i in 1:levels - 1) {
        cg <- add_line(cg, paste0("PGE", i, "=EXP(DIS*(PSI-DIFG", i, "))/(1+EXP(DIS*(PSI-DIFG", i))
    } 
    cg <- add_line(cg, "P0=1-PGE1")
    if (levels > 2) {
        for (i in 1:levels - 2) {
            cg <- add_line(cg, paste0("P", i, "=PGE", i, "-PGE", i + 1))
        }
    }
    cg <- add_line(cg, paste0("P", levels - 1, "=PGE", levels - 1))
    cg <- add_empty_line(cg)
    cg <- decrease_indent(cg)
    cg <- add_line("ENDIF")
    cg <- add_empty_line(cg)
    for (i in 0:levels - 1) {
        cg <- add_line(cg, "IF(MODEL.EQ.OC", levels, ".AND.DV.EQ.", i, ") P=P", i)
    }
    cg <- add_line(cg, "IF(P.LT.1E-16) P=1E-16")
    cg <- add_line(cg, "IF(P.GT.(1-1E-16)) P=1-1E-16")
    cg <- add_line(cg, paste0("IF(MODEL.EQ.OC", levels, ") Y=-2*LOG(P)"))
    get_code(cg)
}
