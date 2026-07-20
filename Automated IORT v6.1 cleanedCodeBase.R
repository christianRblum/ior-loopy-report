# Title: Automated Inter Observer Reliability  and Training Script for Loopy Scorings including summary report on differences per
# Video and Behaviour.
# Author: Christian Blum
# last updated: 2026-07-17
#
# This is a script to calculate Inter Observer Reliability Test Scores using ICCs and explore differences between coders.
#
# Before running: download all the CSV files from Loopy for all the videos of all the coders you want to compare 
# and place them in the folders in the specified paths in the chapter "SET PROJECT" (line 50 or so)
#
# This script then merges all information into one dataframe and assigns Coder ID to every event, based on the title
# (that is the title on Loopy at the time of downloading the csv, not the filename of the csv).
#
# It also calculates durations and frequencies for all behaviours and corrects directionality for group protocols
# (relevant for ad libitum coding). In doing so "A chasing B" and "B being chased by A" are no longer interpreted 
# as two different events.
#
# It will then provide ICC results for several categories of data (always for durations and frequencies separately):
#   - all data, only social, only non-social, without termination, without modifiers
# This gives a quick summary on where potential differences come from (relevant only for further training, not for 
# reporting IOR scores in publications etc.)
#   - report only "all data durations" and "all data frequencies" results in your publications
#
# After that, the script does some additional data exploration, showing how much each behaviour differed between coders 
# in duration and frequency (and bouts of duration behaviours), for the complete dataset and per video.
#
# It then sums up all the findings in a PDF (for all plots) and an Excel file (for all data), wich are exported to
# the work-directory.
#
# Manual edits are required only in "SET PROJECT" (just below), to specify if raven or crow or other reference scorings are used
#
# This script was written by Christian Blum for the labs of Thomas Bugnyar and Barbara Klump. 
# Intended to compare 2 coders at a time, one reference, one trainee

rm(list=ls()) # remove all objects from workspace, in case you want to clean house before running this script


# SET PROJECT ####

## manual entry required ####
project_choice <- "walnut"   # change as needed, see available options in comment below
                            # this defines what reference codings are used

# "crow" refers to "focal ontogeny crow" ethogram, training for crow focals of klump lab
# "C:\ucloud\Work\TA Palmyre\Loopy Coding\Automated IORT\CSV Input\Crows FOC"

# "crow2" refers to second batch of "focal ontogeny crow" ethogram, after initial training was completed
# "C:\ucloud\Work\TA Palmyre\Loopy Coding\Automated IORT\CSV Input\Crows FOC second step"

# "raven" refers to "raven social short" ethogram, training for raven focals of bugnyar lab
# "C:\ucloud\Work\TA Palmyre\Loopy Coding\Automated IORT\CSV Input\Ravens RSS"

# "other" refers to any other set of reference codings used for other purposes of a more temporary nature
# "C:\ucloud\Work\TA Palmyre\Loopy Coding\Automated IORT\CSV Input\Other"

# "walnut" refers to valentine walnut training videos
# "C:\ucloud\Work\TA Palmyre\Loopy Coding\Automated IORT\CSV Input\WAL walnut training valentine"


# 0. SET UP WORKSPACE, LOAD PACKAGES ####
    setwd("C:/ucloud/Work/TA Palmyre/Loopy Coding/Automated IORT") # set working directory
   

  # load packages
    #library(plyr) 
    library(dplyr) 
    library(readr) 
    library(tidyr)
    library(irr)
    library(ggplot2)
    library(gridExtra)
    library(grid)
    library(stringr)
    library(writexl)
    library(patchwork)
    library(purrr)
    library(tibble)
    library(scales)
    library(ggtext)
  
    
# 1. IMPORT AND MERGE ALL .CSV FILES FROM ONE FOLDER ####
    
    ## a) load data ####
    # create helper function to import CSVs
    read_folder_csvs <- function(path) {
      if (!dir.exists(path)) {
        stop(sprintf("Directory does not exist: %s", path))
      }
      files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
      if (length(files) == 0) {
        warning(sprintf("No CSV files found in: %s", path))
        return(tibble())
      }
      map_dfr(files, ~ read_csv(.x, show_col_types = FALSE))
    }
    
    # define paths
    base_dir <- "C:/ucloud/Work/TA Palmyre/Loopy Coding/Automated IORT/CSV Input"
    
    # map project to folder
    ref_dir_map <- list(
      crow  = "Crows FOC",
      crow2 = "Crows FOC second step",
      raven = "Ravens RSS",
      other = "Other",
      walnut = "WAL walnut training valentine"
    )
    
    # error if wrong input
    if (!project_choice %in% names(ref_dir_map)) {
      stop(sprintf(
        "Invalid project_choice: '%s'. Use 'crow', 'crow2', 'raven', 'walnut' or 'other'.",
        project_choice
      ))
    }
    
    ref_path     <- file.path(base_dir, ref_dir_map[[project_choice]])
    trainee_path <- file.path(base_dir, "_TRAINEE INPUT DATA")  # adjust if your trainee folder differs
    
    # load data
    reference_data <- read_folder_csvs(ref_path)
    trainee_data   <- read_folder_csvs(trainee_path)
    
    # print a summary of data origins
    message(sprintf("Loaded reference data for '%s' from: %s", project_choice, ref_path))
    message(sprintf("Loaded trainee data from: %s", trainee_path))
    
    
    ## b) check data ####
    
    # glimpse data
    glimpse(reference_data)
    glimpse(trainee_data)
    unique(trainee_data$Scoring)
    

        # ensure reference and trainee tables are structurally identical
    stopifnot(
      # same number of columns
      ncol(reference_data) == ncol(trainee_data),
      # same column names in the same order
      identical(names(reference_data), names(trainee_data)),
      # same column classes per column
      identical(
        sapply(reference_data, \(x) paste(class(x), collapse = "/")),
        sapply(trainee_data,   \(x) paste(class(x), collapse = "/"))
      )
    )
    
    ## c) merge data ####
    
    # Merge: row-bind both datasets and tag the source of each row
    df <- dplyr::bind_rows(
      dplyr::mutate(reference_data, coder_source = "reference"),
      dplyr::mutate(trainee_data,   coder_source = "trainee")
    )
    
    
    ## d) clean data ####
    
    unique(df$Scoring) # sometimes contains "part" info, or "IORT" or "reference" markers
    
    # replace "_part" with "-part"
    df$Scoring <- gsub("_part", "-part", df$Scoring, ignore.case = TRUE)
    df$Scoring <- gsub("_part", "-part", df$Scoring, ignore.case = TRUE)
    
    # remove IORT and reference markers from scoring titles
    df$Scoring <- gsub("_IORT-reference", "", df$Scoring, ignore.case = TRUE)
    df$Scoring <- gsub("_IORT", "", df$Scoring, ignore.case = TRUE)
    
    unique(df$Scoring)
    
    
    # typically scoring titles follow a defined pattern of units of information separated by underscores
    # therefore titles from the same project should have identical numbers of underscores
    
    underscore_count = str_count(df$Scoring, "_")
    unique(underscore_count)
    
    # confirm all titles have the same number of underscores
    if (any(is.na(underscore_count)) || length(unique(underscore_count)) != 1) {
      warning("Inconsistent underscore counts (or NAs present) in the scoring titles. Clean input data.")
    } else {
      message(sprintf("All scoring titles contain %d underscores. Confirm this is correct for your project.",
                      unique(underscore_count)))
    }
    
    
    glimpse(df) # check

  # optional: export all data for manual clean up if necessary (e.g. due to different Loopy versions)
  # (correction for differences in directionality in group codings (A>B vs, B<A) is taken care of by this script)
    # write.csv(df,"combined.csv", row.names = FALSE) # export data into one .csv for manual cleaning
    # df <- read.csv("combined_cleaned.csv") # import cleaned data

        
# 2. CREATE NEW COLUMNS FOR ADDITIONAL INFORMATION ####
  # add more columns
    df$Coder = gsub(".*_","",df$Scoring) # create column for Coder, only works if title is correctly named
                                         # (i.e. coder-Name is last word after last "_")
    df$Type = df$Stop # create column for Type by copying Stop column
    df$Type[!is.na(df$Type)] <- "continuous" # label "continuous" if Stop data is available
    df$Type[is.na(df$Type)] <- "instant" # label "instant" if no Stop data available
    df$Social = as.character(df$Partner) # create column for Social by copying Partner column
    df$Social[!is.na(df$Social)] <- "social" # label as "social" if Parnter data is available
    df$Social[is.na(df$Social)] <- "non_social" # label as "non_social" if no Partner data available
    df$Duration = df$Stop-df$Start # create colum for Duration (for continuous events)
  # scoring title still has name of coder attached:
    unique(df$Scoring) 
    # because we have this info now in "Coder", we don't need it here anymore, and keeping it in
    # would always create two separate datasets, even though it refers to the same video, creating false negatives.
    df$Scoring <- sub("_[^_]+$", "", df$Scoring) # remove coder names from Scoring titles
    
    
    
    unique(df$Scoring)# this should be the videotitles only, equal to scoring titles excluding coder names
    
    
    unique(df$Coder)
    
    table(df$coder_source, df$Coder) # confirm that coder name and data source match, also check number of rows of data
    
    
    # for raven focal protocols we do not distinguish between the different vocalisation types (loud, soft etc.),
    # i still am for training purposes though, so this code is commented out:
    # df$Value <- gsub('Loud', 'Vocalisation', df$Value)
    # df$Value <- gsub('Soft', 'Vocalisation', df$Value)
    # df$Value <- gsub('Haa call (food)', 'Vocalisation', df$Value)
    # unique(df$Value)
    
  # adjustment for group protocols: 
    # in focal protocols everybody chooses the same subject (the focal), therefore coding is standardised
    # in group protocols coding A>B and B<A are both valid, therefore standardisation is not guaranteed
    # to make sure the script doesn't interpret one identical event (CoderA: A<B versus CoderB: B>A) as two different events, I:
      # created new columns for the ID of emitter and receiver, per initiation and termination
      # initiation: emit means initiation_emitter = subject
      #             receiver means initiation_emitter = partner
      #             undirected means initiation_emitter = subject_OR_partner in alphabetical order
      #             NA means initiation_emitter = subject (for non-directed i.e. non-social behaviours)  
    # I applied the same logic to initiation_receiver, termination_emitter and termination_receiver
    # finally I pasted emitter and receiver into a dyad column:
      # initiation_dyad = "initation_emitter_initiates_initiation_receiver"
      # termination_dyad = "termination_emitter_terminates_termination_receiver"
    
  # creating the emitter and receiver columns:
   
    df$initiation_emitter = as.character(df$Subject)
    df$initiation_emitter = ifelse(df$Initiation %in% "undirected" , paste(df$Subject, df$Partner, sep=", "),
                            ifelse(df$Initiation %in% "receive", as.character(df$Partner),
                            df$initiation_emitter))
    
    df$initiation_receiver = as.character(df$Partner)
    df$initiation_receiver = ifelse(df$Initiation %in% "undirected" , paste(df$Subject, df$Partner, sep=", "),
                             ifelse(df$Initiation %in% "receive", as.character(df$Subject),
                             df$initiation_receiver))
    
    df$termination_emitter = as.character(df$Subject)
    df$termination_emitter[is.na(df$Termination)] <- "NA" 
    df$termination_emitter = ifelse(df$Termination %in% "undirected" , paste(df$Subject, df$Partner, sep=", "),
                             ifelse(df$Termination %in% "receive", as.character(df$Partner),
                             df$termination_emitter))
    
    df$termination_receiver = as.character(df$Partner)
    df$termination_receiver[is.na(df$Termination)] <- "NA" 
    df$termination_receiver = ifelse(df$Termination %in% "undirected" , paste(df$Subject, df$Partner, sep=", "),
                              ifelse(df$Termination %in% "receive", as.character(df$Subject),
                              df$termination_receiver))
      
  # sorting the undirected cases alphabetically:
    # create a function that takes a column (vector of strings) and alphabetizes comma-separated names within each entry,
    # to standardize entries like "Anton, Heidi" and "Heidi, Anton" into the same string "Anton_OR_Heidi"
    # so that undirected dyads are always written in the same order regardless of how they were originally coded.
    
    alphabetize_col <- function(col) {
      # sapply applies the inner function to every element in the column
      unname(sapply(col, function(x) {
        # strsplit: split the string at every comma: e.g. "Heidi, Anton" becomes c("Heidi", " Anton")
        # [[1]]: extract the resulting character vector from the list that strsplit returns
        # trimws: remove leading/trailing whitespace from each name: c("Heidi", "Anton")
        # sort: put names in alphabetical order: c("Anton", "Heidi")
        # paste(..., collapse = "_OR_"): glue them back together: "Anton_OR_Heidi"
        paste(sort(trimws(strsplit(x, ",")[[1]])), collapse = "_OR_")
      }))
      # unname: removes element names that sapply auto-generates, returning a clean unnamed vector
    }
    
    df <- df %>%
      mutate(
        initiation_emitter2  = alphabetize_col(initiation_emitter),
        initiation_receiver2 = alphabetize_col(initiation_receiver),
        termination_emitter2 = alphabetize_col(termination_emitter),
        termination_receiver2 = alphabetize_col(termination_receiver),
        # fix blanks
        initiation_receiver2 = na_if(initiation_receiver2, "")
      )
    
    
  # and now we can combine those to the dyad columns for initiation and termination:
    df$initiation_dyad = paste(df$initiation_emitter2, df$initiation_receiver2, sep="_initiates_")
    df$termination_dyad = paste(df$termination_emitter2, df$termination_receiver2, sep="_terminates_")
    
  # optional: verify data (e.g. manually in Excel).
    # write.csv(df,"checkup.csv", row.names = FALSE) # export data into one .csv

    
# 3. RESHAPING THE DATAFRAME INTO A MORE USABLE STRUCTURE ####
        
  # create pivot tables with TotalDuration and TotalFrequency for different datasets 
  # to help identify potential error sources:
    
    summary(df)
    str(df)
    
    
  # all data
    names(df)
    pivot.all <- df %>% 
      select(Coder, Scoring, initiation_dyad, Value, Duration, termination_dyad, Modifiers, Social)%>% 
      group_by(Coder, Scoring, initiation_dyad, Value, termination_dyad, Modifiers, Social) %>% 
      summarise(TotalDuration = sum(Duration), TotalFrequency = length(Duration))
    head(pivot.all)
    glimpse(pivot.all)
    
  # separate counts of instant events from counts of bouts
    # in earlier versions of this script, TotalFrequency included count of instant events and duration bouts combined
    # here we now exclude bouts and store them in a separate column, which we don't use for IOR, but include in data exploration.
    pivot.all$TotalBouts = pivot.all$TotalFrequency
    pivot.all$TotalBouts[is.na(pivot.all$TotalDuration)]=NA
    pivot.all$TotalFrequency[!is.na(pivot.all$TotalDuration)]=NA  
    
    
  # Create subsets for IOR scores that won't be included in publications, but provide additional info on where trainees need
    # more focused training still.
  # only non-social
    pivot.all_non_social <- pivot.all %>% 
      filter(Social=="non_social")
    head(pivot.all_non_social)
  
  # only social
    pivot.all_social <- pivot.all %>% 
      filter(Social=="social")
    head(pivot.all_social)
  
  # no modifiers
    pivot.all.no.modifiers <- pivot.all %>%
      group_by(Coder, Scoring, initiation_dyad, Value, termination_dyad, Social) %>%
      summarise(
        TotalDuration  = sum(TotalDuration, na.rm = TRUE),
        TotalFrequency = sum(TotalFrequency, na.rm = TRUE),
        TotalBouts     = sum(TotalBouts, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        TotalDuration  = na_if(TotalDuration, 0),
        TotalFrequency = na_if(TotalFrequency, 0),
        TotalBouts     = na_if(TotalBouts, 0)
      )
    
  # without meter values for neighbours, this will only be used for FRQ 
    pivot.all.no.neighbour.meters <- pivot.all %>%
      mutate(
        Modifiers = if_else(
          Value == "Neighbour",
          str_remove(Modifiers, ".*\\+"),
          Modifiers
        )
      ) %>%
      group_by(Coder, Scoring, initiation_dyad, Value, termination_dyad, Modifiers, Social) %>%
      summarise(
        TotalDuration  = sum(TotalDuration, na.rm = TRUE),
        TotalFrequency = sum(TotalFrequency, na.rm = TRUE),
        TotalBouts     = sum(TotalBouts, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        TotalDuration  = na_if(TotalDuration, 0),
        TotalFrequency = na_if(TotalFrequency, 0),
        TotalBouts     = na_if(TotalBouts, 0)
      )
    
    
  # spread TotalDuration and TotalFrequency between Coders for separate datasets  
    IORT.data.all=as.data.frame(pivot.all %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.non_social=as.data.frame(pivot.all_non_social %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.social=as.data.frame(pivot.all_social %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.all.no.modifiers=
      as.data.frame(pivot.all.no.modifiers %>% 
                      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.all.no.neighbour.meters=
      as.data.frame(pivot.all.no.neighbour.meters %>% 
                      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    
    
    
# 4. RUN IORTs FOR ALL DATASETS USING ICCs #####
    
    # I calculate scores for Durations and Frequencies separately, due to the nature of the data 
    # Additional scores besides "all duration" and "all frequency" are created for subsets to identify areas in need of further training
    # Tests are run using a helper function
    # Test parameters are selected according to https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/
    
    ## a) set names ####
    names(IORT.data.all) 
    
    # get dynamic placeholders from df (names of reference and trainee coders)
    placeholder_reference <- df %>%
      filter(coder_source == "reference") %>%
      distinct(Coder) %>%
      pull() # to actually get the name as character, not as tibble
    
    placeholder_trainee <- df %>%
      filter(coder_source == "trainee") %>%
      distinct(Coder) %>%
      pull()
    
    # sanity check
    stopifnot(
      length(placeholder_reference) == 1L,  # exactly one reference coder found
      length(placeholder_trainee)  == 1L    # exactly one trainee coder found
    )
    
    # helper to build the two relevant column names for a given measure prefix
    cols_for <- function(prefix, ref, tra) {
      c(paste0(prefix, ref), paste0(prefix, tra))
    }
    
    dur_cols <- cols_for("TotalDuration_", placeholder_reference, placeholder_trainee)
    frq_cols <- cols_for("TotalFrequency_", placeholder_reference, placeholder_trainee)
    
    ## b) Safe ICC with sample size check and CI extraction ####
    run_icc <- function(data, dur_or_frq_cols, label, min_rows = 5, # adjust min rows to whatever seems plausible
                        model = "twoway", type = "agreement") {
      # Prepare data:
      clean <- data %>%
        mutate(across(all_of(dur_or_frq_cols), ~ tidyr::replace_na(., 0))) %>% # replace NA with 0
        select(all_of(dur_or_frq_cols)) %>% # select columns (ICC only needs coderA and coderB measurement columns)
        # remove all-zero rows: instant behaviours have no duration, so both coders listing duration=0s in those cases
        # would lead to lots of 0s in the dataset and inflated agreement scores
        filter(if_any(everything(), ~ . != 0)) 
      
      n <- nrow(clean) # get number of observations
      
      # Guard against too-small samples (otherwise the IOR scores become nonsensical)
      if (n < min_rows) {
        warning(sprintf("ICC for '%s' skipped: only %d rows (need >= %d)", label, n, min_rows))
        # when the sample size is too small for a reliable ICC, return a placeholder list that mimics the structure of a 
        # real result, so downstream code doesn't break.
        return(list(
          value = NA_real_, # ICC score, set to NA if we don't get an actual result
          lbound = NA_real_, # lower bound of 95% CI, also set to NA
          ubound = NA_real_, # upper bound of 95% CI, believe it or not, set to NA
          n = n, # number of rows that were available (for diagnostic reporting)
          label = label, # descriptive name of this ICC category (e.g. "all data (dur)")
          skipped = TRUE, # flag so downstream code knows this result was skipped, not failed
          data = clean # the cleaned data matrix, kept in case manual inspection is needed
        ))
      }
      
      result <- icc(clean, model = model, type = type)
      result$n <- n
      result$label <- label
      result$skipped <- FALSE
      result$data <- clean
      return(result)
    }
    
    ## c) Extract a one-row summary from an ICC result ####
    icc_summary_row <- function(result) {
      tibble(
        category = result$label,
        ICC      = round(as.numeric(result$value), 3),
        lower_CI = round(as.numeric(result$lbound), 3),
        upper_CI = round(as.numeric(result$ubound), 3),
        n_rows   = result$n,
        skipped  = result$skipped
      )
    }
    
    
    
    ## d) Run all ICCs ####
  
    
    # Main results (for publication)
    all.dur.result <- run_icc(IORT.data.all, dur_cols, "all data (dur)")
    all.frq.result <- run_icc(IORT.data.all, frq_cols, "all data (frq)")
    
    # Krippendorff alpha cross-check (frequencies only)
    # some people are hesitant to use ICC for count data and prefer Kripp alpha, but (provided sufficient data),
    # they perform the same, see here:
    all.frq.kripp <- kripp.alpha(t(all.frq.result$data), method = "interval")
    message(sprintf("ICC: %.3f vs Kripp alpha: %.3f. If the difference is more than 0.01, check the sample size", 
                    as.numeric(all.frq.result$value), all.frq.kripp$value))
    
    # Sub-category ICCs (for training, not publication)
    non_social.dur.result <- run_icc(IORT.data.non_social, dur_cols, "non-social (dur)")
    non_social.frq.result <- run_icc(IORT.data.non_social, frq_cols, "non-social (frq)")
    
    social.dur.result <- run_icc(IORT.data.social, dur_cols, "social (dur)")
    social.frq.result <- run_icc(IORT.data.social, frq_cols, "social (frq)")
    
    all.dur.no.modifiers.result <- run_icc(
      IORT.data.all.no.modifiers, dur_cols, "no modifiers (dur)")
    all.frq.no.modifiers.result <- run_icc(
      IORT.data.all.no.modifiers, frq_cols, "no modifiers (frq)")
    
    all.frq.no.neighbour.meters.result <- run_icc(
      IORT.data.all.no.neighbour.meters, frq_cols, "no neighbour meters (frq)")
    
    
     
# 5. ALL ICC RESULTS ####
    
    # Collect all results into one table automatically
    all_icc_results <- list(
      all.dur.result, all.frq.result,
      non_social.dur.result, non_social.frq.result,
      social.dur.result, social.frq.result,
      all.dur.no.modifiers.result, all.frq.no.modifiers.result,
      all.frq.no.neighbour.meters.result
    )
    
    icc.plot <- bind_rows(lapply(all_icc_results, icc_summary_row))
    
    # Add score classification
    icc.plot <- icc.plot %>%
      mutate(score = case_when(
        is.na(ICC)   ~ "skipped",
        ICC < 0.5    ~ "poor",
        ICC < 0.75   ~ "moderate",
        ICC < 0.9    ~ "good",
        TRUE         ~ "excellent"
      ))
    
    IORT.table <- icc.plot
    
    
    # Define the desired order (bottom to top after coord_flip, so reverse it)
    category_order <- c(
      "all data (dur)",
      "all data (frq)",
      "non-social (dur)",
      "non-social (frq)",
      "social (dur)",
      "social (frq)",
      "no modifiers (dur)",
      "no modifiers (frq)",
      "no neighbour meters (frq)"
    )
    
    plot_data <- icc.plot %>%
      filter(category %in% category_order) %>%
      mutate(
        category = factor(category, levels = rev(category_order))
      )
    
    plot_computed <- plot_data %>% filter(!skipped)
    plot_skipped  <- plot_data %>% filter(skipped)
    
    # bold labels for "all data" categories
    label_levels <- levels(plot_data$category)
    label_formatted <- ifelse(
      label_levels %in% c("all data (dur)", "all data (frq)"),
      paste0("**", label_levels, "**"),
      label_levels
    )
    
    IORT.plot <- ggplot(plot_data, aes(x = category, y = ICC)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,    ymax = 0.5,  fill = "red",       alpha = 0.1) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5,  ymax = 0.75, fill = "orange",    alpha = 0.1) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 0.9,  fill = "darkgreen", alpha = 0.1) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.9,  ymax = 1.0,  fill = "green",     alpha = 0.1) +
      geom_point(data = plot_computed, size = 3) +
      geom_errorbar(data = plot_computed, aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
      geom_text(data = plot_computed, aes(label = paste0("n=", n_rows)), 
                vjust = -1, size = 2.5, color = "grey40") +
      geom_point(data = plot_skipped, aes(y = -0.03), 
                 shape = 4, size = 4, color = "red", stroke = 1.5) +
      geom_text(data = plot_skipped, aes(y = -0.03, label = paste0("skipped (n=", n_rows, ")")),
                hjust = -0.15, size = 2.5, color = "red") +
      coord_flip() +
      scale_y_continuous(limits = c(-0.03, 1), breaks = seq(0, 1, 0.25)) +
      scale_x_discrete(labels = setNames(label_formatted, label_levels)) +
      labs(
        title = paste("Inter Observer Reliability Test for", placeholder_trainee,
                      "\ncalculated on", Sys.Date()),
        subtitle = "Automated IORT R script by Christian Blum (christian.blum@univie.ac.at)",
        x = NULL, y = "ICC (with 95% CI)",
        caption = "Only 'all data (dur)' and 'all data (frq)' are for publication.
Error bars show 95% confidence intervals. n = number of behaviour-context rows.
<0.5 poor (red), 0.5-0.75 moderate (orange), 0.75-0.9 good (green), >0.9 excellent (bright green).
Red X = skipped due to insufficient sample size."
      ) +
      theme(
        plot.caption = element_text(hjust = 0, size = 7),
        axis.text.y = ggtext::element_markdown()
      )
    
    IORT.plot # get plot
    IORT.table # get table
    
         
# 6. DATA EXPLORATION ####
  # to narrow down some sources of coding-differences I created a new table with all relevant information:
    names(df) # to see what columns are available
    exploration.data <- df %>% 
      select(Scoring, Coder, Subject, Behaviour, Value, Partner, Initiation, Termination, 
             initiation_dyad, termination_dyad, Modifiers, Type, Social, Duration)%>% 
      group_by(Scoring, Coder, Subject, Behaviour, Value, Partner, Initiation, Termination, 
               initiation_dyad, termination_dyad, Modifiers, Type, Social) %>% 
      summarise(TotalDuration = sum(Duration), TotalFrequency = length(Duration))
    unique(exploration.data$Scoring) 
  # as we see, the scoring still has coder-names in it and therefore differs,
  
  # separate counts of instant events from counts of bouts
    # in earlier versions of this script, TotalFrequency included count of instant events and duration bouts combined
    # here we now exclude bouts and store them in a separate column, which we don't use. just in case somebods want to use it.
    exploration.data$TotalBouts = exploration.data$TotalFrequency
    exploration.data$TotalBouts[is.na(exploration.data$TotalDuration)]=NA
    exploration.data$TotalFrequency[!is.na(exploration.data$TotalDuration)]=NA  
    
  
  # now we spread TotalDuration and TotalFrequency between Coders for separate datasets
    pivot.exploration.all=as.data.frame(exploration.data %>% 
                                pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency, TotalBouts)))
  # now let's calculate the differences between the coders' scored durations and frequencies
  # first we replace for all columns startint with "Total" every "NA" with "0"
    pivot.exploration.all <- pivot.exploration.all %>% mutate(across(starts_with("Total"), ~replace_na(., 0)))
  
  # before we calculate the differences, we want to make sure not to clutter up things with useless info:
  # all behaviours have frequencies, but only some behaviours have durations
  # when investigating duration differences, we don't need to see zeros listed for instant events
  # so let's create another dataframe for only continuous behaviours.
    pivot.exploration.continuous <- pivot.exploration.all %>% 
    filter(Type=="continuous") 
    unique(pivot.exploration.continuous$Value) # only shows behaviours with durations, good.
    
  # to simplify coding from here on out, lets create three separate, properly named datasets
    diff.dur = pivot.exploration.continuous # no instant event behaviours (they don't have durations)
    diff.dur <- diff.dur %>% select(-starts_with("TotalFrequency"), -starts_with("TotalBouts")) # remove Frequencies and Bouts
    
    diff.frq = pivot.exploration.all%>% 
      filter(Type!="continuous") # all behaviours (continuous behaviours have frequencies too (=bouts))
    diff.frq <- diff.frq %>% select(-starts_with("TotalDuration"), -starts_with("TotalBouts")) # remove Durations and Bouts
    
    diff.bout = pivot.exploration.all%>% 
      filter(Type=="continuous") # all behaviours (continuous behaviours have frequencies too (=bouts))
    diff.bout <- diff.bout %>% select(-starts_with("TotalDuration"), -starts_with("TotalFrequency")) # remove Durations and Frequencies
    
    
    
  # calculate the absolutes of the differences (we don't want negative values)
    names(pivot.exploration.all) # to see the names of the coders
    
    diff.dur$TotalDuration_Differences <-
      abs(
        diff.dur[[paste0("TotalDuration_",  placeholder_reference)]] -
          diff.dur[[paste0("TotalDuration_",  placeholder_trainee)]]
      )
    
    diff.frq$TotalFrequency_Differences <-
      abs(
        diff.frq[[paste0("TotalFrequency_", placeholder_reference)]] -
          diff.frq[[paste0("TotalFrequency_", placeholder_trainee)]]
      )
    
    diff.bout$TotalBouts_Differences <-
      abs(
        diff.bout[[paste0("TotalBouts_", placeholder_reference)]] -
          diff.bout[[paste0("TotalBouts_", placeholder_trainee)]]
      )
    
   
    
    
    # sum differences by Value (=Behaviour)
    diff.dur.by_value <- diff.dur %>%
      group_by(Value) %>%
      summarise(TotalDuration_Differences = sum(TotalDuration_Differences)) %>%
      arrange(desc(TotalDuration_Differences))
    
    diff.frq.by_value <- diff.frq %>%
      group_by(Value) %>%
      summarise(TotalFrequency_Differences = sum(TotalFrequency_Differences)) %>%
      arrange(desc(TotalFrequency_Differences))
    
    diff.bout.by_value <- diff.bout %>%
      group_by(Value) %>%
      summarise(TotalBouts_Differences = sum(TotalBouts_Differences)) %>%
      arrange(desc(TotalBouts_Differences))
    
    # sum differences by Value (=Behaviour) and Scoring
    diff.dur.by_value_and_scoring <- diff.dur %>%
      group_by(Scoring, Value) %>%
      summarise(TotalDuration_Differences = sum(TotalDuration_Differences), .groups = "drop") %>%
      arrange(desc(TotalDuration_Differences))
    
    diff.frq.by_value_and_scoring <- diff.frq %>%
      group_by(Scoring, Value) %>%
      summarise(TotalFrequency_Differences = sum(TotalFrequency_Differences), .groups = "drop") %>%
      arrange(desc(TotalFrequency_Differences))
    
    diff.bout.by_value_and_scoring <- diff.bout %>%
      group_by(Scoring, Value) %>%
      summarise(TotalBouts_Differences = sum(TotalBouts_Differences), .groups = "drop") %>%
      arrange(desc(TotalBouts_Differences))
    
   
  # let's also plot them:
  
    # Overall difference bar plot
    plot_diff_total <- function(data, value_col, y_label, subtitle_text) {
      ggplot(data, aes(x = reorder(Value, -!!sym(value_col)), y = !!sym(value_col))) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
              plot.caption = element_text(hjust = 0, size = 5)) +
        ggtitle("Coding Differences") +
        ylab(y_label) + xlab("Behaviours") +
        labs(subtitle = subtitle_text,
             caption = "Sum of all differences of coded durations, per behaviour, across all videos.
This plot doesn't inform on who coded more than the other, because absolute values had to be used.
Otherwise differences might cancel each other out (20s more in one and 20s less in another video/bout
would add up to 0s differences).")
    }
    
    # Per-video difference bar plot
    plot_diff_per_video <- function(data, value_col, y_label, subtitle_text) {
      ggplot(data, aes(x = reorder(Value, -!!sym(value_col)), y = !!sym(value_col))) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
              strip.text = element_text(size = 5),
              plot.caption = element_text(hjust = 0, size = 5)) +
        facet_wrap(~ Scoring, scales = "free") +
        scale_y_continuous(breaks = breaks_pretty(n = 5)) +
        ggtitle("Coding Differences") +
        ylab(y_label) + xlab("Behaviours") +
        labs(subtitle = subtitle_text,
             caption = "Also here, sum of absolute differences was used. Be aware of different y-axis limits.")
    }
    
    
# 7. PLOT DIFFERENCES ####
    
    ## a) total differences across all scorings ####
    
    DUR.total  <- plot_diff_total(diff.dur.by_value, "TotalDuration_Differences",
                                  "Sum of Differences in coded Durations (s)",
                                  "Duration Differences across all Videos")
    
    FRQ.total  <- plot_diff_total(diff.frq.by_value, "TotalFrequency_Differences",
                                  "Sum of Differences in coded Frequencies (Events)",
                                  "Frequency Differences across all Videos")
    
    BOUT.total <- plot_diff_total(diff.bout.by_value, "TotalBouts_Differences",
                                  "Sum of Differences in coded Bouts (of durations)",
                                  "Bout Differences across all Videos")
    
     
    ## b) differences per scoring #### 
     
      # Get unique Scoring categories
      scoring_categories <- sort(unique(diff.dur.by_value_and_scoring$Scoring))
      
      # Split Scoring categories into groups of 3
      scoring_groups <- split(scoring_categories, ceiling(seq_along(scoring_categories) / 3))
      
      # Create a list to store the final plots
      final_plots <- list()
      
      # Loop through each group of Scoring categories
      for (i in seq_along(scoring_groups)) {
        # Get the current group of Scoring categories
        current_group <- scoring_groups[[i]]
        
        # Create a list to store paired plots for the current group
        paired_plots <- list()
        
        # Loop through each Scoring category in the current group
        for (scoring in current_group) {
          # Filter data for the current Scoring category
          dur_data <- diff.dur.by_value_and_scoring %>% filter(Scoring == scoring)
          frq_data <- diff.frq.by_value_and_scoring %>% filter(Scoring == scoring)
          bout_data <- diff.bout.by_value_and_scoring %>% filter(Scoring == scoring)
          
          # Create the duration plot for the current Scoring category
          dur_plot <- ggplot(dur_data, aes(x = reorder(Value, -TotalDuration_Differences), y = TotalDuration_Differences)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
                  strip.text = element_text(size = 5),
                  plot.caption = element_text(hjust = 0, size = 5),
                  plot.title = element_text(size = 6),
                  axis.title.y = element_text(size = 8))+
            ggtitle(scoring) +
            ylab("DUR Difference (s)") +
            xlab(NULL)
          
          # Create the frequency plot for the current Scoring category
          frq_plot <- ggplot(frq_data, aes(x = reorder(Value, -TotalFrequency_Differences), y = TotalFrequency_Differences)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
                  strip.text = element_text(size = 5),
                  plot.caption = element_text(hjust = 0, size = 5),
                  plot.title = element_text(size = 6),
                  axis.title.y = element_text(size = 8)) +
            ggtitle(scoring) +
            ylab("FRQ Difference (count)") +
            xlab(NULL)
          
          # Create the frequency plot for the current Scoring category
          bout_plot <- ggplot(bout_data, aes(x = reorder(Value, -TotalBouts_Differences), y = TotalBouts_Differences)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
                  strip.text = element_text(size = 5),
                  plot.caption = element_text(hjust = 0, size = 5),
                  plot.title = element_text(size = 6),
                  axis.title.y = element_text(size = 8)) +
            ggtitle(scoring) +
            ylab("BOUT Difference (count)") +
            xlab(NULL)
          
          # Combine the three plots side by side
          paired_plots[[scoring]] <- dur_plot | frq_plot | bout_plot
        }
        
        # Combine all paired plots for the current group into a single stacked layout
        final_plots[[i]] <- wrap_plots(paired_plots, ncol = 1)
      }
      
      # Display all the plots (one at a time)
      for (plot in final_plots) {
        print(plot)
      }
    
      
  # these are the seprate categories that inform you on what behaviours differed the most:
    diff.dur.by_value # differences in durations overall
    diff.frq.by_value # differences in frequencies overall
    diff.bout.by_value # differences in bouts overall
    diff.dur.by_value_and_scoring # differences in durations per scoring (= focal video)
    diff.frq.by_value_and_scoring # differences in frequencies per scoring (=focal)
    diff.bout.by_value_and_scoring # differences in frequencies per scoring (=focal)
    
    # diff.dur.by_value_no_focal # differences in durations overall
    # diff.dur.by_value_and_scoring_no_focal # differences in durations per scoring (= focal video)
    
    # diff.dur.by_value_only_focal
    # diff.dur.by_value_and_scoring_only_focal

    
    
    
# 8. EXPORT ALL RESULTS ####
    grid.table(IORT.table)
    
    text_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "For the IOR tests only durations of duration behaviours and frequencies of 
               instant behaviours were used. In the following plots I show differences to the reference scoring per scoring,
                but I now also plot the Bouts (frequency differences of duration behaviours). Bouts generally are not 
               included in statistical analysis of Loopy data, but they can still inform on differences in coding approaches.",
               size = 3, fontface = "plain", hjust = 0.5, vjust = 0.5) +
      theme_void()+  # Remove all axes and background
      theme(plot.margin = margin(10, 10, 10, 10))  # Add some padding around the text


    text_plot

    
    
  ## a) create pdf report file ####
    # all plots to one pdf:
    date_tag <- format(Sys.Date(), "%Y-%m-%d")
    safe_name <- gsub("[^A-Za-z0-9._-]", "_", placeholder_trainee)
    
    
    # Using cairo_pdf() instead of pdf() because the default pdf() device maps ASCII hyphens (-) 
    # to typographic minus signs (−) in certain fonts. Cairo uses modern font rendering that 
    # preserves the original characters as-is.
    # I spent wayyy longer than I'd care to admit to figure this out. This way you can copy paste the 
    # scoring titles directly out of the facet plot titles in the pdf. This was not possible before, or, 
    # if you did, you always had to manually change the symbols in the Loopy search.
    # Finally fixed!
    cairo_pdf(paste0("IORT_", safe_name, "_results_", date_tag, ".pdf"), onefile = TRUE)
    IORT.plot
    plot.new()
    grid.table(IORT.table)
    DUR.total
    FRQ.total
    BOUT.total
    # DUR.per_video
    # FRQ.per_video
    
    # DUR.total_no_focal
    # DUR.per_video_no_focal
    
    # DUR.total_only_focal
    # DUR.per_video_only_focal
    
    # Add the final_plots (each plot with 3 rows)
    
    text_plot
    for (plot in final_plots) {
      print(plot)  # Add each plot in the list to the PDF
    }
    
    
    dev.off() 
   
      
  ## b) create excel data file ####
      
         
   # all icc results and dataframes to one xlsx:     
      out_xlsx <- sprintf("IORT_%s_results_%s.xlsx", safe_name, date_tag)
      
      # Helper to convert icc result to exportable table
      icc_to_table <- function(result) {
        if (result$skipped) {
          return(tibble(note = paste("Skipped:", result$label, "- only", result$n, "rows")))
        }
        vals <- unlist(result[!names(result) %in% c("data", "skipped", "label", "n")])
        tibble(metric = names(vals), value = as.character(vals))
      }
      
      sheets <- list(
        ICC_Summary         = IORT.table,
        IORT.all.dur        = icc_to_table(all.dur.result),
        IORT.all.frq        = icc_to_table(all.frq.result),
        IORT.nonsocial.dur  = icc_to_table(non_social.dur.result),
        IORT.nonsocial.frq  = icc_to_table(non_social.frq.result),
        IORT.social.dur     = icc_to_table(social.dur.result),
        IORT.social.frq     = icc_to_table(social.frq.result),
        Duration_Total      = diff.dur.by_value,
        Frequency_Total     = diff.frq.by_value,
        Bout_Total          = diff.bout.by_value,
        Duration_per_Video  = diff.dur.by_value_and_scoring,
        Frequency_per_Video = diff.frq.by_value_and_scoring,
        Bout_per_Video      = diff.bout.by_value_and_scoring,
        All_Data            = df
      )
      
      write_xlsx(sheets, path = out_xlsx)
      
    
     
    sessionInfo() 
    # R version 4.3.1 (2023-06-16 ucrt)
    # Platform: x86_64-w64-mingw32/x64 (64-bit)
    # Running under: Windows 11 x64 (build 26200)
    # 
    # Matrix products: default
    # 
    # 
    # locale:
    #   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
    # [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
    # [5] LC_TIME=English_United Kingdom.utf8    
    # 
    # time zone: Europe/Vienna
    # tzcode source: internal
    # 
    # attached base packages:
    #   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
    # 
    # other attached packages:
    #   [1] ggtext_0.1.2    scales_1.4.0    tibble_3.2.1    purrr_1.0.4     patchwork_1.2.0
    # [6] writexl_1.5.0   stringr_1.5.1   gridExtra_2.3   ggplot2_4.0.0   irr_0.84.1     
    # [11] lpSolve_5.6.20  tidyr_1.3.1     readr_2.1.5     dplyr_1.1.4    
    # 
    # loaded via a namespace (and not attached):
    #   [1] utf8_1.2.3         generics_0.1.3     xml2_1.3.8         stringi_1.8.3     
    # [5] hms_1.1.3          magrittr_2.0.3     RColorBrewer_1.1-3 fansi_1.0.4       
    # [9] abind_1.4-5        cli_3.6.1          rlang_1.1.1        crayon_1.5.2      
    # [13] performance_0.15.2 commonmark_1.9.1   bit64_4.0.5        withr_3.0.0       
    # [17] tools_4.3.1        parallel_4.3.1     tzdb_0.4.0         vctrs_0.6.5       
    # [21] R6_2.5.1           lifecycle_1.0.4    car_3.1-2          bit_4.0.5         
    # [25] vroom_1.6.5        insight_1.4.2      pkgconfig_2.0.3    pillar_1.9.0      
    # [29] gtable_0.3.6       glue_1.6.2         Rcpp_1.0.11        xfun_0.42         
    # [33] tidyselect_1.2.1   rstudioapi_0.15.0  farver_2.1.1       labeling_0.4.3    
    # [37] carData_3.0-5      compiler_4.3.1     S7_0.2.0           markdown_1.12     
    # [41] gridtext_0.1.6
    
    
# summary of findings    
    IORT.plot # show the most important results as plot
    IORT.table # show the most important results as table
    table(df$coder_source, df$Coder) # safety check that reference and trainee coders are correct (and compare number of coded events)
  