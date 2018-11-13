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


print_model <- function(model) {
    cat(str_irt_model(model))
}

save_model <- function(model, filename) {
    fp <- file(filename)
    writeLines(str_irt_model(model), fp)
    close(fp)
}

str_irt_model <- function(model) {
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
    cg <- add_code(cg, response_probability_prediction_code())
    cg <- add_empty_line(cg)
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
    levels <- ordcat_level_arrays(model$scale)
    cg <- add_line(cg, "MODEL=0")
    for (i in 1:length(levels)) {
        cg <- add_line(cg, paste0("OC", i, "=", i, '    ; ', levels_as_string(levels[[i]])))
    }
    i = i + 1
    for (item in model$scale$items) {
        if (item$type == "binary") {
            cg <- add_line(cg, paste0("BIN=", i, '    ; binary ', levels_as_string(item$levels)))
            break
        }
    }
    cg
}


data_models_code <- function(model) {
    cg <- code_generator()
    bin_items <- unique_binary_items(model$scale)
    if (length(bin_items) > 0) {
        cg <- add_code(cg, binary_data_model_code())    # Only one type of binary allowed (0, 1)
    }
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
    if (item$type == "binary") {
        return("BIN")
    }
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
    if (item$type == "ordcat") {
        cg <- add_line(cg, paste0("DIS=THETA(", next_theta, ")"))
        next_theta <- next_theta + 1
        for (i in 1:(length(item$levels) - 1)) {
            cg <- add_line(cg, paste0("DIF", i, "=THETA(", next_theta, ")"))
            next_theta <- next_theta + 1
        }
    } else { # type == "binary"
        cg <- add_line(cg, paste0("DIS=THETA(", next_theta, ")"))
        cg <- add_line(cg, paste0("DIF=THETA(", next_theta, ")"))
        cg <- add_line(cg, paste0("GUE=THETA(", next_theta, ")"))
    }
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    list(code=cg, next_theta=next_theta)
}

binary_data_model_code <- function() {
    cg <- code_generator()
    cg <- banner_comment(cg, "binary data model")
    cg <- add_line(cg, "IF(MODEL.EQ.BIN) THEN")
    cg <- increase_indent(cg)
    cg <- add_line(cg, "P1=GUE+(1-GUE)*EXP(DIS*PSI-DIF))/(1+EXP(DIS*(PSI-DIF)))")
    cg <- add_line(cg, "P0=1-P1")
    cg <- decrease_indent(cg)
    cg <- add_line(cg, "ENDIF")
    cg <- add_line(cg, "IF(MODEL.EQ.BIN.AND.DV.EQ.0) P=P0")
    cg <- add_line(cg, "IF(MODEL.EQ.BIN.AND.DV.EQ.1) P=P1")
    cg <- add_empty_line(cg)
    cg <- add_line(cg, "IPRED = P1*1")
    cg <- add_empty_line(cg)
    cg
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
    cg <- add_empty_line(cg)
    cg <- add_line(cg, paste0("IPRED=", levels_probability_sum(levels)))
    cg
}

levels_probability_sum <- function(levels) {
    levels <- levels[levels!=0]
    term_func <- function(level) {
        paste0("P", level, "*", level)
    }
    terms <- sapply(levels, term_func)
    paste(terms, collapse=" + ")
}

response_probability_prediction_code <- function() {
    cg <- code_generator()
    cg <- banner_comment(cg, "Response probability prediction and residual")
    cg <- add_line(cg, "IF(P.LT.1E-16) P=1E-16  ; To protect for P->0")
    cg <- add_line(cg, "IF(P.GT.(1-1E-16)) P=1-1E-16  ; To protect for P->1")
    cg <- add_line(cg, "Y=-2*LOG(P)")
    cg <- add_line(cg, "RES=DV-IPRED")
    cg
}

simulation_code <- function(model) {
    cg <- code_generator()
    cg <- banner_comment(cg, "simulation code")
    cg <- add_line(cg, "IF(ICALL.EQ.4) THEN")
    cg <- increase_indent(cg)
    bin_items <- unique_binary_items(model$scale)
    if (length(bin_items) > 0) {
        cg <- add_line(cg, "IF(MODEL.EQ.BIN) THEN")
        cg <- increase_indent(cg)
        cg <- add_line(cg, "CALL RANDOM (2,R)")
        cg <- add_line(cg, "SDV=0")
        cg <- add_line(cg, "IF(P1.GT.R) SDV=1")
        cg <- decrease_indent(cg)
        cg <- add_line(cg, "ENDIF")
    }
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
    cg <- add_line(cg, "$ESTIMATION METHOD=COND LAPLACE -2LL MAXEVAL=999999 PRINT=1")
    cg <- add_line(cg, "$COVARIANCE")
    max <- 0
    binary <- ""
    for (item in model$scale$items) {
        if (length(item$levels) > max) {
            max <- length(item$levels)
        }
        if (item$type == "binary") {
            binary <- " GUE "   
        }
    }
    dif_numbers <- seq(1, max - 1)
    cg <- add_line(cg, "$TABLE ID ITEM DV PSI TIME IPRED RES FILE=psi_estimates NOAPPEND ONEHEADER NOPRINT")
    columns <- c("ITEM", "DIS", paste0("DIF", dif_numbers), paste0("DIFG", dif_numbers))
    columns_str <- paste(columns, collapse=" ")
    cg <- add_line(cg, paste0("$TABLE ", columns_str, binary))
    cg <- add_line(cg, "       FILE=item_parameters_tab1 NOAPPEND ONEHEADER NOPRINT")
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

initial_thetas_from_data <- function(df) {
    wide <- df %>%
        nmIRT:::prepare_dataset() %>%
        wide_item_data(baseline=TRUE)

    mirt_model <- mirt::mirt(data=wide, model=1, itemtype="graded")
    coeffs <- coef(mirt_model, IRTpars=TRUE)
    dataset_scale <- scale_from_dataset(df)
    inits <- list()
    
    for (item in dataset_scale$items) {
        scale_levels <- item$levels
        print(item$number)
        dataset_levels_with_na <- unique(wide[[item$number]])
        dataset_levels <- sort(dataset_levels_with_na[!is.na(dataset_levels_with_na)])
        item_coeffs <- as.numeric(coeffs[[item$number]])
        if (length(scale_levels) == length(dataset_levels)) {
            # Here the levels must be the same. This should be a previously checked invariant when 
            # attaching the dataset.
            inits <- c(inits, list(item_coeffs))
        } else {
            if (all(scale_levels[1:length(dataset_levels)] == dataset_levels)) {
                # Missing level above
                edge_initials <- rep("0 FIX", length(scale_levels) - length(dataset_levels))
                inits <- c(inits, list(c(item_coeffs, edge_initials)))
            } else if (all(tail(scale_levels, length(dataset_levels)) == dataset_levels)) {
                # Missing level below
                edge_initials <- rep("1 FIX", length(scale_levels) - length(dataset_levels))
                inits <- c(inits, list(c(item_coeffs[1], edge_initials, item_coeffs[-1])))
            } else {
                # Level gaps
                
            }
        }
    }
    inits
}

# Supports filename or data.frame as data
data_check <- function(model_or_data, scale=NULL) {
    if (class(model_or_data) == "irt_model") {
        dataset <- model_or_data$dataset
        scale <- model_or_data$scale
    } else {
        dataset <- model_or_data
    }

    df <- read_dataset(dataset) %>%
        prepare_dataset()

    all_dataset_items <- unique(df$ITEM)
    all_scale_items <- all_items(scale)
    dataset_in_scale <- all_dataset_items %in% all_scale_items
    if (!all(dataset_in_scale)) {
        missing_items <- all_dataset_items[!dataset_in_scale]
        cat("Items present in dataset but not in scale:", paste(missing_items, collapse=", "), "\n")
    }
    scale_in_dataset <- all_scale_items %in% all_dataset_items
    if (!all(scale_in_dataset)) {
        missing_items <- all_scale_items[!scale_in_dataset]
        cat("Items present in scale but not in dataset:", paste(missing_items, collapse=", "), "\n")
    }

    wide <- wide_item_data(df)
    missing_items <- c()
    for (item in scale$items) {
        n <- as.character(item$number)
        if (n %in% colnames(wide)) {
            dataset_levels <- sort(unique(wide[[n]]))
            scale_levels <- item$levels
            dataset_in_scale <- dataset_levels %in% scale_levels
            if (!all(dataset_in_scale)) {
                missing_levels <- dataset_levels[!dataset_in_scale]
                cat("Levels present in dataset but not in scale for item", n, ":", paste(missing_levels, collapse=", "), "\n")
            }
            scale_in_dataset <- scale_levels %in% dataset_levels
            if (!all(scale_in_dataset)) {
                missing_levels <- scale_levels[!scale_in_dataset]
                cat("Levels present in scale but not in dataset for item", n, ":", paste(missing_levels, collapse=", "), "\n")
            }
        }
    }
}