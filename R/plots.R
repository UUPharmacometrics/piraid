items1 <- c("14"="II.14.\n Speech",
            "15"="II.15.\n Saliva \n and drooling",
            "16"="II.16.\n Chewing and \n Swallowing",
            "17"="II.17.\n Eating tasks",
            "18"="II.18.\n Dressing",
            "19"="II.19.\n Hygiene",
            "20"="II.20.\n Handwriting",
            "21"="II.21.\n Doing Hobbies\n activities",
            "22"="II.22.\n Turning in bed",
            "24"="II.24.\n Getting out\n of bed",
            "25"="II.25.\n Walking and\n balance",
            "26"="II.26.\n Freezing",
            "27"="III.27.\n Speech",
            "28"="III.28.\n Facial Expression",
            "29"="III.29.\n Rigidity-Neck",
            "30"="III.30.\n Rigidity-RUE",
            "31"="III.31.\n Rigidity-LUE",
            "32"="III.32.\n Rigidity-RLE",
            "33"="III.33.\n Rigidity-LLE",
            "34"="III.34.\n Finger tapping \n-Right hand",
            "35"="III.35.\n Finger tapping\n-Left hand",
            "36"="III.36.\n Hand movements\n-Right hand",
            "37"="III.37.\n Hand movements\n-Left hand",
            "38"="III.38.\n Pronation-supination \n movements\n- Right hand",
            "39"="III.39.\n Pronation-supination \nmovements\n- Left hand",
            "40"="III.40.\n Toe tapping\n-Right foot",
            "41"="III.41.\n Toe tapping\n-Left foot",
            "42"="III.42.\n Leg agility\n-Right leg",
            "43"="III.43.\n Leg agility\n-Left leg",
            "44"="III.44.\n Arising \n from chair",
            "45"="III.45.\n Gait",
            "47"="III.47.\n Postural stability",
            "48"="III.48.\n Posture",
            "49"="III.49.\n Global spontaneity\n of movement")


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

# TODO:
# * Implement names of all items
# * Put names into scale yaml
# * Return plots instead of print
# * Use ggforce paginate? or Benjamin's extra thingamajig

icc_plots <- function(df, items_per_page=8) {
    max_levels <- df %>%
        dplyr::group_by(ITEM) %>%
        dplyr::summarise(max_level=max(DV))
    global_max_level <- max(max_levels$max_level)
    unique_items <- sort(unique(df$ITEM))
    score_combinations <- expand.grid(CAT=1:global_max_level, ITEM=unique_items)
    score_labels <- paste0("score:", 1:global_max_level)
    names(score_labels) <- seq(1:global_max_level)

    all_difs <- c("DIF1", "DIF2", "DIF3")
    dif_symbols <- dplyr::quo_name(rlang::quo("DIF1"))
    one_dif <- "P"

    parameters <- df[!duplicated(df$ITEM),  ] %>% dplyr::select(-PSI)
    psi_grid <- data.frame(PSI=seq(min(df$PSI), max(df$PSI), by=0.1)) %>%
        merge(score_combinations) %>%
        dplyr::full_join(parameters, by="ITEM") %>%
        dplyr::mutate(P=graded_response_model(.)) %>% #PSI, CAT, DIS, DIF1, DIF2, DIF3)) %>%
        dplyr::select(ITEM, CAT, PSI, P)

    full_df <- dplyr::full_join(df, score_combinations, by="ITEM")

    for (i in seq(1, length(unique_items), by=items_per_page)) {
        if (length(unique_items) - i + 1 < items_per_page) {
            # This is the final iteration
            current_items <- unique_items[i:length(unique_items)]
        } else {
            # Next chunk of items
            current_items <- unique_items[i:(i + items_per_page - 1)]
        }

        partial_df <- dplyr::filter(full_df, ITEM %in% current_items)
        partial_psi_grid <- dplyr::filter(psi_grid, ITEM %in% current_items)

        plot <- ggplot(partial_df, aes(PSI, as.numeric(DV>=CAT))) +
            geom_point() +
            geom_smooth(method="gam", method.args=list(family="binomial"), formula=y~s(x, bs="cs")) +
            geom_line(data=partial_psi_grid, aes(PSI, P), size=1, colour="darkred") +
            ggforce::facet_grid_paginate(ITEM~CAT, labeller=
                labeller(ITEM=as_labeller(items1), ITEM=label_wrap_gen(20), CAT=as_labeller(score_labels))) +
            theme_bw(base_size=14, base_family="") +
            labs(y="Y>=score")
        print(plot)
    }
}


#item.parameters <- read.table("/home/rikard/devel/ICC_plot/item_parameters_tab1", skip=1, header=T,sep=",")

#icc_plots(item.parameters)
