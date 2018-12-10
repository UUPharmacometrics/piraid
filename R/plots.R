# The Graded response model for an unknown number of DIF columns
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


icc_plots <- function(df, scale, items_per_page=8) {
    max_levels <- df %>%
        dplyr::group_by(ITEM) %>%
        dplyr::summarise(max_level=max(DV))
    global_max_level <- max(max_levels$max_level)
    unique_items <- sort(unique(df$ITEM))
    score_combinations <- expand.grid(CAT=1:global_max_level, ITEM=unique_items)
    score_labels <- paste0("score:", 1:global_max_level)
    names(score_labels) <- seq(1:global_max_level)

    parameters <- df[!duplicated(df$ITEM),  ] %>% dplyr::select(-PSI)
    psi_grid <- data.frame(PSI=seq(min(df$PSI), max(df$PSI), by=0.1)) %>%
        merge(score_combinations) %>%
        dplyr::full_join(parameters, by="ITEM") %>%
        dplyr::mutate(P=graded_response_model(.)) %>% #PSI, CAT, DIS, DIF1, DIF2, DIF3)) %>%
        dplyr::select(ITEM, CAT, PSI, P)

    full_df <- dplyr::full_join(df, score_combinations, by="ITEM")

    item_labels <- item_name_list(scale)

    k <- 1
    plot_list <- list()
    for (i in seq(1, length(unique_items), by=items_per_page)) {
        if (length(unique_items) - i + 1 < items_per_page) {
            # This is the final iteration
            current_items <- unique_items[i:length(unique_items)]
        } else {
            # Next chunk of items
            current_items <- unique_items[i:(i + items_per_page - 1)]
        }
        # FIXME: We could do away with this by using nrows and page options to facet_grid_paginate
        partial_df <- dplyr::filter(full_df, ITEM %in% current_items)
        partial_psi_grid <- dplyr::filter(psi_grid, ITEM %in% current_items)

        plot <- ggplot(partial_df, aes(PSI, as.numeric(DV>=CAT))) +
            geom_point() +
            geom_smooth(method="gam", method.args=list(family="binomial"), formula=y~s(x, bs="cs")) +
            geom_line(data=partial_psi_grid, aes(PSI, P), size=1, colour="darkred") +
            ggforce::facet_grid_paginate(ITEM~CAT, labeller=
                labeller(ITEM=as_labeller(item_labels), ITEM=label_wrap_gen(20), CAT=as_labeller(score_labels))) +
            theme_bw(base_size=14, base_family="") +
            labs(y="Y>=score")
        plot_list[[k]] <- plot
        k <- k + 1
    }
    plot_list
}

# Input: origdata is a data.frame with DV and ITEM
mirror_plots <- function(origdata, scale, simdata=NULL, nrow=4, ncol=5) {
    unique_items <- sort(unique(origdata$ITEM))
    item_labels <- item_name_list(scale)

    origdata <- dplyr::select(origdata, DV, ITEM)
    origdata$type <- "observed"

    if (is.null(simdata)) {
        df <- origdata
    } else {
        simdata <- dplyr::select(simdata, DV, ITEM)
        simdata$type <- "simulated"
        df <- rbind(origdata, simdata)
    }
    k <- 1
    plot_list <- list()
    for (i in 1:ceiling(length(unique_items) / (nrow * ncol))) {
        plot <- ggplot(df, aes(x=DV, fill=type)) +
            #stat_count(mapping=aes(x=DV, y=..prop.., group=1), width=0.3) +
            geom_bar(aes(x=DV, y=..prop.., fill=type), width=0.75, position=position_dodge()) +
            scale_y_continuous(labels=scales::percent) +
            ggforce::facet_wrap_paginate(~ITEM, nrow=nrow, ncol=ncol, page=i, labeller=
                labeller(ITEM=as_labeller(item_labels), ITEM=label_wrap_gen(20))) +
            ylab("Percent of total") +
            xlab("Response") +
            theme_bw(base_size=14, base_family="")
        plot_list[[k]] <- plot
        k <- k + 1
    }
    plot_list
}


#item.parameters <- read.table("/home/rikard/devel/ICC_plot/item_parameters_tab1", skip=1, header=T,sep=",")
#mirror_plots(item.parameters)
#icc_plots(item.parameters)
