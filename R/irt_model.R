irt_model <- function(scale) {
    structure(list(scale=scale), class="irt_model")
}


set_scale <- function(model, scale) {
    model$scale <- scale
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
    for (item in model$scale$items) {
        res <- irt_item_assignment_code(model$scale, item, next_theta)
        next_theta <- res$next_theta
        cg <- add_code(cg, res$code)
    }
    cg <- add_empty_line(cg)
    cg <- add_code(cg, data_models_code(model))
    cg <- add_code(cg, simulation_code(model))
    get_code(cg)
}


type_constants <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "constants to select model type")
    #levels <- ordcat_levels(model$scale)
    levels <- ordcat_level_arrays(model$scale)
    cg <- add_line(cg, "MODEL=0")
    for (i in 1:length(levels)) {
        cg <- add_line(cg, paste0("OC", i, "=", i, '    ; ', levels_as_string(levels[[i]])))
    }
    cg
}


data_models_code <- function(model) {
    cg <- code_generator()
    ordcat_levels <- ordcat_level_arrays(model$scale)
    if (length(ordcat_levels) > 0) {
        for (l in ordcat_levels) {
            cg <- add_code(cg, ordered_categorical_data_model_code(model$scale, l))
            cg <- add_empty_line(cg)
        }
    }
    cg
}

# Return code symbol for type constant given scale and item
model_type_constant <- function(scale, item) {
    ordcat_levels <- ordcat_level_arrays(scale)
    for (i in 1:length(ordcat_levels)) {
        if (length(item$levels) == length(ordcat_levels[[i]]) && all(item$levels == ordcat_levels[[i]])) {
            return(paste0("OC", i))
        }
    }
}


irt_item_assignment_code <- function(scale, item, next_theta) {
    cg <- code_generator()
    cg <- add_line(cg, paste0("IF(ITEM.EQ.", item$number, ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, paste0("MODEL=", model_type_constant(scale, item)))
    cg <- add_line(cg, paste0("DIS=THETA(", next_theta, ")"))
    next_theta <- next_theta + 1
    for (i in 1:(length(item$levels) - 1)) {
        cg <- add_line(cg, paste0("DIF", i, "=THETA(", next_theta, ")"))
        next_theta <- next_theta + 1
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    list(code=cg, next_theta=next_theta)
}


ordered_categorical_data_model_code <- function(scale, levels) {
    dummy_item <- irt_item(0, levels, "ordcat")
    cg <- code_generator()
    cg <- banner_comment(cg, paste0("ordered categorical data model with ", length(levels), " levels: ", levels_as_string(levels)))
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, "DIFG1=DIF1")
    if (length(levels) > 2) {
        for (i in 1:(length(levels) - 2)) {
            cg <- add_line(cg, paste0("DIFG", i + 1, "=DIFG", i, "+DIF", i + 1))
        }
    }
    cg <- add_empty_line(cg)
    for (i in 1:(length(levels) - 1)) {
        cg <- add_line(cg, paste0("PGE", i, "=EXP(DIS*(PSI-DIFG", i, "))/(1+EXP(DIS*(PSI-DIFG", i, ")))"))
    } 
    cg <- add_empty_line(cg)
    cg <- add_line(cg, paste0("P", levels[1], "=1-PGE1"))
    if (length(levels) > 2) {
        for (i in 1:(length(levels) - 2)) {
            cg <- add_line(cg, paste0("P", levels[i + 1], "=PGE", i, "-PGE", i + 1))
        }
    }
    cg <- add_line(cg, paste0("P", levels[length(levels)], "=PGE", i + 1))
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg <- add_empty_line(cg)
    for (e in levels[-length(levels)]) {
        cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ".AND.DV.EQ.", e, ") P=P", e))
    }
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ".AND.DV.GE.", levels[length(levels)], ") P=P", levels[length(levels)]))
    cg <- add_line(cg, "IF(P.LT.1E-16) P=1E-16")
    cg <- add_line(cg, "IF(P.GT.(1-1E-16)) P=1-1E-16")
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ") Y=-2*LOG(P)"))
    cg
}


simulation_code <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "simulation code")
    cg <- add_line(cg, "IF(ICALL.EQ.4) THEN")
    cg <- increase_indent(cg)
    levels <- ordcat_level_arrays(model$scale)
    for (l in levels) {
        cg <- add_code(cg, ordered_categorical_simulation_code(model$scale, l))
        cg <- add_empty_line(cg)
    }
    cg <- add_line(cg, "DV=SDV")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}


ordered_categorical_simulation_code <- function(scale, levels) {
    dummy_item <- irt_item(0, levels, "ordcat")
    cg <- code_generator()
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ") THEN"))
    cg <- increase_indent(cg)
    cg <- add_line(cg, "CALL RANDOM(2, R)")
    cg <- add_line(cg, paste0("SDV=", levels[1]))
    for (i in 1:(length(levels) - 1)) {
        cg <- add_line(cg, paste0("IF(R.LT.PGE", i, ") SDV=", levels[i + 1]))
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg
}
