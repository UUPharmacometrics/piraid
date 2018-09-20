irt_model <- function() {
    structure(list(items=list()), class="irt_model")
}

render <- function(x) UseMethod("render")

render.irt_model <- function(model) {
    next_theta <- 1
    cg <- code_generator()
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_line(cg, "$PRED")
    cg <- add_line(cg, "PSI=THETA(1)+ETA(1)")
    cg <- add_code(cg, type_constants(model))
    cg <- add_empty_line(cg)
    cg <- banner_comment(cg, "assignment of item parameters")
    for (item in model$items) {
        res <- irt_item_assignment_code(item, next_theta)
        next_theta <- res$next_theta
        cg <- add_code(cg, res$code)
    }
    cg <- add_empty_line(cg)
    cg <- add_code(cg, data_models_code(model))
    cg <- add_code(cg, simulation_code(model))
    get_code(cg)
}

add_item <- function(model, item) {
    model$items <- c(model$items, list(item))
    model
}


# Get a sorted array of ordered categorical levels
ordcat_levels <- function(model) {
    levels <- c()
    for (item in model$items) {
        if (item$type == "ordcat") {
            levels <- c(levels, item$levels) 
        }
    }
    sort(unique(levels))
}

type_constants <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "constants to select model type")
    levels <- ordcat_levels(model)
    cg <- add_line(cg, "MODEL=0")
    if (length(levels) > 0) {
        for (l in levels) {
            cg <- add_line(cg, paste0("OC", l, "=", l))
        } 
    }
    cg
}

data_models_code <- function(model) {
    cg <- code_generator()
    ordcat_levels <- ordcat_levels(model)
    if (length(ordcat_levels) > 0) {
        for (l in ordcat_levels) {
            cg <- add_code(cg, ordered_categorical_data_model_code(l))
            cg <- add_empty_line(cg)
        }
    }
    cg
}


predefined_scale <- function(model, scale) {
    scale <- tolower(scale)
    path <- system.file("extdata", paste0(scale, ".yaml"), package="nmIRT")
    if (path == "") {
        stop("Error: No such predefined scale. Available scale is UPDRS")
    }
    db <- yaml::read_yaml(path)
    for (item in db$items) {
        new_irt_item <- irt_item(number=item$number, levels=item$levels, type=item$type)
        model <- add_item(model, new_irt_item)
    }
    model
}



irt_item <- function(number, levels, type) {
    structure(list(number=number, levels=levels, type=type), class="irt_item")
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
    list(code=cg, next_theta=next_theta)
}


ordered_categorical_data_model_code <- function(levels) {
    cg <- code_generator()
    cg <- banner_comment(cg, paste0("ordered categorical data model with ", levels, " levels"))
    cg <- add_line(cg, paste0("IF(MODEL.EQ.OC", levels, ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, "DIFG1=DIF1")
    if (levels > 2) {
        for (i in 1:(levels - 2)) {
            cg <- add_line(cg, paste0("DIFG", i + 1, "=DIFG", i, "+DIF", i + 1))
        }
    }
    cg <- add_empty_line(cg)
    for (i in 1:(levels - 1)) {
        cg <- add_line(cg, paste0("PGE", i, "=EXP(DIS*(PSI-DIFG", i, "))/(1+EXP(DIS*(PSI-DIFG", i, ")))"))
    } 
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "P0=1-PGE1")
    if (levels > 2) {
        for (i in 1:(levels - 2)) {
            cg <- add_line(cg, paste0("P", i, "=PGE", i, "-PGE", i + 1))
        }
    }
    cg <- add_line(cg, paste0("P", levels - 1, "=PGE", levels - 1))
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg <- add_empty_line(cg)
    for (i in 0:(levels - 1)) {
        cg <- add_line(cg, paste0("IF(MODEL.EQ.OC", levels, ".AND.DV.EQ.", i, ") P=P", i))
    }
    cg <- add_line(cg, "IF(P.LT.1E-16) P=1E-16")
    cg <- add_line(cg, "IF(P.GT.(1-1E-16)) P=1-1E-16")
    cg <- add_line(cg, paste0("IF(MODEL.EQ.OC", levels, ") Y=-2*LOG(P)"))
    cg
}


simulation_code <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "simulation code")
    cg <- add_line(cg, "IF(ICALL.EQ.4) THEN")
    cg <- increase_indent(cg)
    levels <- ordcat_levels(model)
    for (l in levels) {
        cg <- add_code(cg, ordered_categorical_simulation_code(l))
        cg <- add_empty_line(cg)
    }
    cg <- add_line(cg, "DV=SDV")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}

ordered_categorical_simulation_code <- function(levels) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("IF(MODEL.EQ.OC", levels, ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, "CALL RANDOM(2, R)")
    cg <- add_line(cg, "SDV=0")
    for (i in 1:(levels - 1)) {
        cg <- add_line(cg, paste0("IF(R.LT.PGE", i, ") SDV=", i))
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}
