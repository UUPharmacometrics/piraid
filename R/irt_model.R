#' Create an IRT model object
#' 
#' \code{irt_model} returns a newly created model object
#'
#' @param scale The scale
#' @param base_scale A base scale on which the scale is based
#' @return A model object
irt_model <- function(scale, base_scale) {
    if (missing(base_scale)) {
        base_scale <- scale
    }
    structure(list(scale=scale, base_scale=base_scale), class="irt_model")
}

#' Change the scale and/or base scale of an IRT model object
#' 
#' \code{set_scale} returns a newly created model object
#'
#' @param scale The scale
#' @param base_scale A base scale on which the scale is based
#' @return The updated model object
set_scale <- function(model, scale, base_scale) {
    model$scale <- scale
    if (missing(base_scale)) {
        base_scale <- scale
    }
    model$base_scale <- base_scale
    model
}


render <- function(x) UseMethod("render")

render.irt_model <- function(model) {
    next_theta <- 1
    cg <- code_generator()
    cg <- add_line(cg, "$SIZES LIM6=4000")
    cg <- add_line(cg, "$PROBLEM")
    cg <- add_code(cg, data_and_input_code(model))
    cg <- add_empty_line(cg)
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
    cg <- add_empty_line(cg)
    cg <- add_code(cg, estimation_task(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, omegas(model))
    cg <- add_empty_line(cg)
    cg <- add_code(cg, initial_thetas(model))
    cg <- add_code(cg, initial_item_thetas(model))
    get_code(cg)
}

add_dataset <- function(model, filename) {
    model$dataset <- filename
    model
}

data_and_input_code <- function(model) {
    cg <- code_generator()
    if (!is.null(model$dataset)) {
        df <- read.csv(model$dataset, nrows=0)
        cg <- add_line(cg, paste0("$INPUT ", paste(colnames(df), collapse=' ')))
        cg <- add_line(cg, paste0("$DATA ", normalizePath(model$dataset)))
    }
    cg
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
    cg <- add_line(cg, paste0("IF(MODEL.EQ.", model_type_constant(scale, dummy_item), ".AND.DV.LE.", levels[1], ") P=P", levels[1]))
    remaining <- levels[-length(levels)]
    remaining <- remaining[-1]
    for (e in remaining) {
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

estimation_task <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "estimation task")
    cg <- add_line(cg, "$ESTIMATION METHOD=COND LAPLACE -2LL MAXEVAL=99999")
    cg <- add_line(cg, "$COVARIANCE")
    max <- 0
    for (item in model$scale$items) {
        if (length(item$levels) > max) {
            max <- length(item$levels)
        }
    }
    dif_numbers <- seq(1, max - 1)
    columns <- c("ID", "ITEM", "DV", "PSI", "DIS", paste0("DIF", dif_numbers), paste0("DIFG", dif_numbers))
    columns_str <- paste(columns, collapse=" ")
    cg <- add_line(cg, paste0("$TABLE ", columns_str))
    cg <- add_line(cg, "       FILE=item_parameters_tab1")
    cg
}

omegas <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "random effects")
    cg <- add_line(cg, "$OMEGA 0.1")
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

initial_thetas <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "item parameters")
    cg <- add_line(cg, "$THETA")
    cg <- add_line(cg, "0.1  ; PSI")
    cg
}

initial_item_thetas <- function(model) {
    #stopifnot('dataset' %in% names(model))
    #df <- read.csv(model$dataset, stringAsFactors=FALSE)
    #initial_thetas_from_dataset(df, model$scale)
    cg <- code_generator()
    for (base_item in model$base_scale$items) {
        item <- get_item(model$scale, base_item$number) 
        if (is.null(item)) {
            cg <- theta_placeholder(cg, base_item)
        } else {
            cg <- add_line(cg, paste0("(0,1) ; I", item$number, "DIS"))
            for (i in seq(1, length(item$levels) - 1)) {
                cg <- add_line(cg, paste0("(0.1) ; I", item$number, "DIF", i))
            }
        }
    }
    cg
}

theta_placeholder <- function(cg, item) {
    for (dummy in item$levels) {
        cg <- add_line(cg, "0 FIX; THETA PLACEHOLDER")
    }
    cg 
}

initial_thetas_from_dataset <- function(df, scale) {
    wide <- df %>%
        dplyr::select(DV, ITEM) %>%      
        dplyr::mutate(DV=as.numeric(replace(DV, DV=='.', '0'))) %>%
        dplyr::mutate(DUMMYCOL=dplyr::row_number()) %>%    # Needed this dummy column for spread to work
        tidyr::spread(ITEM, DV, fill=0) %>%
        dplyr::select(-DUMMYCOL)
    item_df <- data.frame(row.names=1:nrow(wide))
    for (col in colnames(wide)) {
        item <- nmIRT:::get_item(scale, as.numeric(col))
        if (is.null(item)) {
            next
        }
        levels <- item$levels
        data <- wide[[col]]
        # Successively binarize the levels like (0)-(1,2),(0,1)-(2)
        for (i in 1:(length(levels) - 1)) {
            cur_levels <- levels[1:i]
            item_df[paste0(col, "_", i)] <- as.integer(! (data %in% cur_levels))
        }
    }
    fit <- ltm::rasch(item_df, constraint = cbind(length(item_df) + 1, 1))
    fit
}
