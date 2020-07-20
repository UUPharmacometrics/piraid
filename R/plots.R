#' Graded response model for an unknown number of DIF columns
#' 
#' @param data A dataset the columns DIS, PSI, DIFi and CAT (level) will be used
#' @return Vector of response values
#' @keywords internal
graded_response_model <- function(data) {
    column_names <- colnames(data)
    #which DIF columns exist in the dataset
    DIF_columns <- column_names[grepl("^DIF[0-9]$",column_names)]
    DIF_numbers <- sub("DIF", "", DIF_columns) %>% as.numeric()
    w <- 0
    # idea of w calculations: w = (CAT >= 1) * DIF1 + (CAT >= 2) * DIF2 + (CAT >= 3) * DIF3 + ...
    for(DIF_nr in unique(DIF_numbers)) {
        DIF_name <- paste0("DIF", DIF_nr)
        w_DIF_nr <- (data$CAT >= DIF_nr) * data[,DIF_name]
        w <- w + w_DIF_nr
    }
    exp(data$DIS * (data$PSI - w)) / (1 + exp(data$DIS * (data$PSI - w)))
}


#' Plot item characteristic curves diagnostic
#' 
#' MDV and EVID will be used to filter out non-observations before plotting
#' 
#' @param model The model that was used to create the output table
#' @param nmtab A data.frame from the item_parameters_tab of a model run. PSI, ITEM, DV, DIS, DIFn are needed
#' @param resample_psi Whether to use the resampling based diagnostic
#' @param psi_range The range of psi values to use for the plot
#' @param samples The number of samples to use when resample_psi = T
#' @param items_per_page The number of items to display on one page (default NULL prints all items)
#' @return A list of plots
#' @export
diagnose_icc_fit <- function(model, nmtab, psi_range = c(-4,4), resample_psi = FALSE, samples = 10,
                       items_per_page=NULL){
  
    required_columns <- c("ID", "TIME", "PSI") 
    is_present <- required_columns  %in% colnames(nmtab)
    if(!all(is_present)) stop("Column(s) ", paste(required_columns[!is_present], sep = " "), " are required but not present in the data frame.", call. = F) 
    if(!"PSI_SE" %in% colnames(nmtab) && resample_psi) stop("The column PSI_SE is required for resamples=TRUE.", call. = F)
    
    if(any(!c("DV","ITEM")  %in% colnames(nmtab))){
      rlang::inform("ITEM and DV were not present in the provided nmtab and will be extracted from the model dataset.")
      nmtab <-  dplyr::left_join(read_dataset(model$dataset), 
                                 dplyr::select_at(nmtab, c("ID","TIME", "PSI", "PSI_SE")), by = c("ID", "TIME"))
    }
    
    nmtab <- nmtab %>%
        filter_observations() %>%
        consolidate_data(model) 

    if(resample_psi){
        psi_samples <- nmtab %>% 
          dplyr::group_by(.data$ID) %>% 
          dplyr::slice(1) %>% 
          dplyr::select(.data$ID, .data$PSI, .data$PSI_SE) %>% 
          dplyr::mutate(PSI_SAMPLES = purrr::map2(.data$PSI, .data$PSI_SE, ~rnorm(samples, .x, .y)))
    }else{
        psi_samples <- nmtab %>% 
            dplyr::group_by(.data$ID) %>% 
            dplyr::slice(1) %>% 
            dplyr::select(.data$ID, PSI_SAMPLES = .data$PSI)
    }
    
    if(interactive()){
      if(resample_psi){
        cat("Calculating GAM-based ICCs for",length(unique(nmtab$ITEM)),
            "items and", length(unique(nmtab$ID)), 
            "subjects with", 
            samples, "samples per subject...\n")
      }else{
        cat("Calculating GAM-based ICCs for",length(unique(nmtab$ITEM)),
            "items and", length(unique(nmtab$ID)), 
            "subjects...\n")
      }
    } 
    if(interactive()) pb <- utils::txtProgressBar(max = max(nmtab$ITEM), style = 3)
        item_names <- item_name_list(model$scale)
        problematic_fits <- NULL
        res <- nmtab %>% 
          dplyr::select("ID", "DV", "ITEM") %>% 
          dplyr::left_join(psi_samples, by = "ID") %>% 
          dplyr::group_by(.data$ITEM) %>% 
          dplyr::group_modify(function(df, group){

            if(resample_psi){
                psi <- df$PSI_SAMPLES %>% 
                  purrr::transpose() %>% 
                  purrr::map(purrr::flatten_dbl) %>% 
                  purrr::map(matrix)
            }else{
                psi <- df$PSI_SAMPLES %>% 
                    matrix()
            }
            dvs <- unique(df$DV) %>% 
                sort() %>% 
                {purrr::set_names(., paste0("cat_", seq_along(.)))}
              rlang::with_handlers(
                {
                    gamres <- mirt::itemGAM(df$DV, psi, theta_lim = psi_range) %>% 
                    unclass() %>% 
                    tibble::as_tibble() %>% 
                        dplyr::mutate(
                            dv = dvs[as.character(.data$cat)],
                            cat = NULL
                        )
                },
                warning = rlang::calling(function(cnd){
                  problematic_fits <<- union(problematic_fits, group$ITEM)
                  cnd_muffle(cnd)
                  TRUE
                }
                )
              )
            if(interactive()) utils::setTxtProgressBar(pb, group$ITEM)
            return(gamres)
          })%>% 
            dplyr::ungroup() %>% 
            dplyr::rename(psi = .data$Theta,
                          item = .data$ITEM,
                          probability = .data$Prob,
                          probability_lower = .data$Prob_low,
                          probability_higher = .data$Prob_high) 
      
    if(interactive()) close(pb)    
    if(!rlang::is_empty(problematic_fits)) 
      rlang::warn(paste0("Problems occured during calculation of the following items: ", 
                         paste0(problematic_fits, collapse =  ",")))
  
    prob_labels <- purrr::map(unique(res$item), 
               ~item_categories_probability_labels(model, get_item(model$scale, .x)))
    item_labels <- item_name_list(model$scale)
    
    prob_levels <- purrr::flatten_chr(prob_labels) %>% unique()
    
    res <- res %>% 
        dplyr::mutate(
            category = purrr::map2_chr(.data$item, .data$dv, ~purrr::pluck(prob_labels, .x, as.character(.y))),
            item = item_labels[.data$item]
        )

    if(!is.null(nmtab)){
      model <-  update_parameters(model, nmtab)
    }
    df_iccs <- model %>% 
        calculate_iccs() %>% 
        dplyr::mutate_if(is.factor, as.character)

    df_combined <- dplyr::bind_rows(
        `GAM smooth` = res,
        `Model fit` = df_iccs,
        .id = "type") %>% 
        dplyr::mutate(
            item = factor(item, levels = item_labels),
            category = factor(category, levels = prob_levels)
        )
    
    if(is.null(items_per_page)){
      n_pages <- 1
    }else{
      n_pages <- ceiling(length(item_labels)/items_per_page)
    }
    purrr::map(seq_len(n_pages),
               ~ggplot2::ggplot(df_combined, 
                                aes(psi, 
                                    probability, 
                                    ymin = probability_lower, 
                                    ymax = probability_higher, 
                                    color = type, 
                                    fill = type)) +
        geom_ribbon(data = dplyr::filter(df_combined, type == "GAM smooth"), alpha=0.5, linetype = "blank")+
        geom_line(na.rm = TRUE)+
        scale_color_manual("", values = c("darkgray", "darkred"))+
        scale_fill_manual("", values = c("darkgray", NA))+
        ggforce::facet_grid_paginate(item~category, labeller = labeller(item = label_wrap_gen(), category = label_value),
                                   nrow = items_per_page, ncol = length(prob_levels), page = .x, byrow = F)+
        theme_bw(base_size=14, base_family="") +
        theme(legend.position = "bottom", legend.margin = ggplot2::margin())+
        labs(x="PSI", y="Probability"))
}

#' @export
#' @rdname diagnose_icc_fit
icc_plots <- diagnose_icc_fit

#' Mirror plots for comparison of original data and simulated data
#'
#' MDV and EVID will be used to filter out non-observations before plotting
#'
#' @param model The model used to generate the data
#' @param simdata The simulated data. Will plot only original data if this is missing
#' @param nrow The number of rows per page to use for the matrix of plots
#' @param ncol The number of columns per page to use for the matrix of plots
#' @return A list of plots. One page per item.
#' @export
diagnose_marginal_probability <- function(model, simdata=NULL, nrow=4, ncol=5) {
    scale <- model$scale

    origdata <- read_dataset(model$dataset) %>% 
        filter_observations() %>%
        consolidate_data(model)
    unique_items <- sort(unique(origdata$ITEM))
    item_labels <- item_name_list(scale)
    origdata <- dplyr::select(origdata, "DV", "ITEM")
    origdata$type <- "observed"

    if (is.null(simdata)) {
        df <- origdata
    } else {
        simdata <- filter_observations(simdata)
        simdata <- dplyr::select(simdata, "DV", "ITEM")
        simdata$type <- "simulated"
        df <- rbind(origdata, simdata)
    }
    k <- 1
    plot_list <- list()
    for (i in 1:ceiling(length(unique_items) / (nrow * ncol))) {
        plot <- ggplot(df, aes(x=.data$DV, fill=.data$type)) +
            #stat_count(mapping=aes(x=DV, y=..prop.., group=1), width=0.3) +
            geom_bar(aes(x=.data$DV, y=UQ(sym("..prop..")), fill=.data$type), width=0.75, position=position_dodge()) +
            scale_y_continuous(labels=scales::percent) +
            ggforce::facet_wrap_paginate(~ITEM, nrow=nrow, ncol=ncol, page=i, labeller=
                labeller(ITEM=as_labeller(item_labels), ITEM=label_wrap_gen(20))) +
            ylab("Percent of total") +
            xlab("Response") +
            labs(fill="Type") + 
            theme_bw(base_size=14, base_family="")
        plot_list[[k]] <- plot
        k <- k + 1
    }
    plot_list
}

#' @export
#' @rdname diagnose_marginal_probability
mirror_plots <- diagnose_marginal_probability

#' Item response correlation plot
#' 
#' Visualizes the correlation of residuals between items as a correlation coefficients heatmap. 
#' 
#' The residuals need to be available as PWRES in the data and are assumed to be standard Pearson residuals (i.e., difference between 
#' observed and expected devided by the expected standard devition). Models that are generated with this package will include the 
#' proper calculation of PWRES by default.
#'
#' @param df A data.frame from the item_parameters_tab of a model run. ID, ITEM, TIME and PWRES will be used.
#' @return A plot object
#' @export
correlation_plot <- function(df) {
    resplot <- df %>%
        dplyr::select("ID", "ITEM", "TIME", "PWRES") %>%
        tidyr::spread(.data$ITEM, .data$PWRES) %>%
        dplyr::select(-"ID", -"TIME")

    # create the correlation matrix
    cormat <- resplot %>%
        stats::cor(use="pairwise.complete.obs") %>%
        round(2)

    # Set lower part of cormat to NA
    cormat[lower.tri(cormat)] <- NA

    # Melt the correlation matrix so as to facilitate the plotting
    melted_cormat <- tidyr::gather(as.data.frame(cormat), "Var2", "value")
    melted_cormat$Var1 <- as.numeric(rownames(cormat))
    melted_cormat$Var2 <- as.numeric(melted_cormat$Var2)
    melted_cormat <- stats::na.omit(melted_cormat)

    lowest_item <- min(df$ITEM)
    if (lowest_item %% 2 == 1) {        # Make sure that the axes always have even numbers
        lowest_item <- lowest_item - 1
    }
    highest_item <- max(df$ITEM)
    if (highest_item %% 2 == 1) {
        highest_item <- highest_item + 1
    }
    
    plot <- ggplot(data=melted_cormat, aes(.data$Var2, .data$Var1, fill=.data$value)) +
        geom_tile(color="white") +
        scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1, 1), name="Pearson\nCorrelation") +
        scale_x_continuous(breaks=seq(lowest_item, highest_item, 2)) +
        scale_y_continuous(breaks=seq(lowest_item, highest_item, 2)) +
        labs(title="") +
        theme_bw() +
        theme(
            axis.title=element_blank(),
            panel.grid.major=element_line(size=0.5, linetype=1),
            panel.grid.minor=element_blank(),
            legend.position="bottom"
        )

    return(plot)
}

#' Plot latent variable versus time
#'
#' @param lv_dataset A dataframe 
#' @param lv_col Name of the latent variable column
#' @param time_col Name of the time column
#' @param se_col Name of the standard error of PSI column
#' @param grouping_col Name of optional grouping column
#'
#' @return A ggplot plot object
#' @export
lv_vs_time_plot <- function(lv_dataset, lv_col = "PSI", time_col = "TIME", 
                            se_col = "SE_PSI", grouping_col = NULL){
    lv_sym <- rlang::sym(lv_col)
    time_sym <- rlang::sym(time_col)
    se_sym <- rlang::sym(se_col) 

    if(!se_col %in% colnames(lv_dataset)) lv_dataset <- dplyr::mutate(lv_dataset, !!se_sym := 1)
    lv_dataset <- dplyr::group_by(lv_dataset, !!time_sym)
    if(!is.null(grouping_col)){
        grouping_sym <- rlang::sym(grouping_col)
        lv_dataset <- lv_dataset %>%
            dplyr::mutate(!!grouping_sym:=factor(!!grouping_sym)) %>% 
            dplyr::group_by(!!grouping_sym, !!time_sym)
    }
    weighted_average <- lv_dataset %>% 
        dplyr::summarise(PSI = stats::weighted.mean(!!lv_sym, 1/(!!se_sym)^2))
    
    if(!is.null(grouping_col)){
        plot <- ggplot(lv_dataset, aes(!!time_sym, !!lv_sym, group = .data$ID, color = !!grouping_sym)) + 
            geom_line(alpha = 0.3)+
            geom_line(data=weighted_average, mapping = aes(.data$TIME, .data$PSI, group =NULL), size = 2)
    }else{
        plot <- ggplot(lv_dataset, aes(!!time_sym, !!lv_sym, group = .data$ID)) + 
            geom_line(alpha = 0.3)+
            geom_line(data=weighted_average, mapping = aes(.data$TIME, .data$PSI, group =NULL), size = 2, color = "darkred")
    }
    
    plot <- plot + 
        scale_y_continuous("Latent variable value")+
        scale_x_continuous("Time")
    
    return(plot)
}