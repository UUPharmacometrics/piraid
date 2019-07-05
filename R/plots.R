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
#' Consolidation is handled by moving consolidated levels into the closest level blow.
#' 
#' @section Warning:
#' Binary items are not handled well.
#' 
#' @param nmtab A data.frame from the item_parameters_tab of a model run. PSI, ITEM, DV, DIS, DIFn are needed
#' @param model The model that was used to create the output table
#' @param resample_psi Whether to use the resampling based diagnostic
#' @param items_per_page The number of items to display on one page (default NULL prints all items)
#' @param page The page to print
#' @param samples The number of samples to use when resample_psi = T
#' @return A list of pages
#' @export
icc_plots <- function(nmtab, model, resample_psi = FALSE, 
                       items_per_page=NULL, page = 1, samples = 50){
    required_columns <- c("ITEM", "DV", "PSI") 
    is_present <- required_columns  %in% colnames(nmtab)
    if(!all(is_present)) stop("Column(s) ", required_columns[!is_present], " are required but not present in the data frame.", call. = F) 
    if(!"PSI_SE" %in% colnames(nmtab) && resample_psi) stop("The column PSI_SE is required for resamples=TRUE.", call. = F)
    if(!"PSI_SE" %in% colnames(nmtab)) rlang::warn("No standard error column (PSI_SE) was provided, the diagnostic might be affected by shrinkage.")
    
    nmtab <- nmtab %>%
        filter_observations() %>%
        consolidate_data(model) 
    
    pred_df <- data.frame(PSI=seq(min(nmtab$PSI), max(nmtab$PSI), length.out = 20), P = 0)
    possible_responses <- purrr::imap_dfr(model$scale$items, ~tibble::tibble(ITEM = .y, response = .x$levels[-1]))
 
    grouped_df <- nmtab %>% 
        dplyr::right_join(possible_responses, by = "ITEM") %>% 
        dplyr::mutate(y = as.integer(.data$DV>=.data$response)) %>% 
        dplyr::group_by(.data$ITEM, .data$response)
    
    ngroups <- dplyr::n_groups(grouped_df)
    cat("Calculating smoothed GAM ICCs...\n")
    pb <- utils::txtProgressBar(min = 0, max = ngroups, style = 3)
    df_npar_fit <- grouped_df %>% 
        dplyr::group_map(
            function(group_df, group){
                if(resample_psi) {
                    psi_samples <- mapply(rnorm, samples, group_df$PSI, group_df$PSI_SE)
                    p_matrix <- apply(psi_samples, 1, function(psi){
                        fit_df <- group_df
                        fit_df$PSI <- psi
                        suppressWarnings(
                            fit <- mgcv::gam(y~s(PSI), family = binomial(), data = fit_df)
                        )
                        predict(fit, newdata = pred_df, type = "response")
                    })
                    pb$up(pb$getVal()+1)
                    quantile_matrix <- apply(p_matrix, 1, quantile, probs = c(0.05,0.95))
                    mean_p <- rowMeans(p_matrix)
                    return(tibble::tibble(PSI = pred_df$PSI, P =  mean_p,
                                   lower = quantile_matrix[1,], upper = quantile_matrix[2, ]))
                }else{
                    if(!"PSI_SE" %in% colnames(group_df)) {
                        weights <- NULL
                    }else{
                        weights <- (1/group_df$PSI_SE)/sum((1/group_df$PSI_SE))
                    }
                    suppressWarnings(
                        fit <- mgcv::gam(y~s(PSI), family = binomial(), data = group_df, weights = weights)
                    )
                    pred_df$P <- predict(fit, newdata = pred_df, type = "response")
                    pb$up(pb$getVal()+1)
                    return(pred_df)
                }
            }
        )
    close(pb)
    
    item_prms <- dplyr::filter(nmtab, !duplicated(.data$ITEM)) %>% 
        dplyr::select(ITEM, dplyr::matches("^(DIS|DIF\\d*$|GUE$)")) 
    
    icc_fit <- pred_df %>%
        merge(possible_responses) %>%
        dplyr::full_join(item_prms, by="ITEM") %>%
        dplyr::mutate(CAT=response) %>% 
        dplyr::mutate(P=graded_response_model(UQ(rlang::sym(".")))) %>%
        dplyr::select("ITEM", "response", "PSI", "P")
    
    item_labels <- item_name_list(model$scale)
    score_labels <- sort(unique(icc_fit$response)) %>% 
        {set_names(paste0("score: ", .), .)} 
    p <- ggplot()+
        {if(resample_psi) geom_ribbon(data = df_npar_fit, mapping = aes(PSI,ymin = lower, ymax=upper), alpha = 0.6)}+
        geom_line(data = df_npar_fit, mapping = aes(PSI, P))+
        geom_line(data = icc_fit, mapping = aes(PSI, P),  color = "darkred")+
        ggforce::facet_grid_paginate(response~ITEM, 
                                     labeller=labeller(ITEM=as_labeller(item_labels), ITEM=label_wrap_gen(20), 
                                                       response=as_labeller(score_labels)),
                                     ncol = items_per_page, nrow = length(score_labels), page = page, byrow = F) +
        theme_bw(base_size=14, base_family="") +
        labs(x="PSI", y="P(Y>=score)")
    return(p)
}

#' Mirror plots for comparison of original data and simulated data
#'
#' MDV and EVID will be used to filter out non-observations before plotting
#'
#' @param origdata The original dataset
#' @param model The model used to generate the data
#' @param simdata The simulated data. Will plot only original data if this is missing
#' @param nrow The number of rows per page to use for the matrix of plots
#' @param ncol The number of columns per page to use for the matrix of plots
#' @return A list of plots. One page per item.
#' @export
mirror_plots <- function(origdata, model, simdata=NULL, nrow=4, ncol=5) {
    scale <- model$scale
    unique_items <- sort(unique(origdata$ITEM))
    item_labels <- item_name_list(scale)

    origdata <- origdata %>%
        filter_observations() %>%
        consolidate_data(model)
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

#item.parameters <- read.table("/home/rikard/devel/ICC_plot/item_parameters_tab1", skip=1, header=T,sep=",")
#scale <- predefined_scale("MDS-UPDRS")
#mirror_plots(item.parameters, scale)
#icc_plots(item.parameters)

# load PSI value & item parameters
#psi.estimates <- read.table("/home/rikard/projects/irt/corrplot/psi_estimates_tab13", skip=1, header=T)
#correlation_plot(psi.estimates)