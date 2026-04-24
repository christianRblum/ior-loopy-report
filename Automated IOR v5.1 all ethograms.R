# Title: Automated Inter Observer Reliability  and Training Script for Loopy Scorings including summary report on differences per
# Video and Behaviour.
# Author: Christian Blum
# last updated: 2026-04-24


# Note: package load order matters, i haven't yet had the time to implement namespace operators (::)
# restart R if you encounter problems and then rerun this script to ensure correct load order


# This is a script to calculate Inter Observer Reliability Test Scores using ICCs and explore differences between coders.
#
# Before running: download all the CSV files from Loopy for all the videos of all the coders you want to compare 
# and place them in the folders "reference" (=CoderA) and "trainee" (=CoderB)
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
#   - report only "all data" and "all frequencies" results in your publications
#
# After that, the script does some additional data exploration, showing how much each behaviour differed between coders 
# in duration and frequency, for the complete dataset and per video.
#
# It then sums up all the findings in a PDF (for all plots) and an Excel file (for all data), wich are exported to
# the project main-directory.
#
# Manual edits are not required
#
# This script was written by Christian Blum for the labs of Thomas Bugnyar and Barbara Klump. 
# Intended to compare 2 coders at a time




# 0. LOAD PACKAGES ####
  # IMPORTANT:   
  # in case a previous script was loaded the packages might be loaded in the wrong order: run this
    # detach("package:plyr", unload=TRUE) # will return error if package wasn't already loaded, that's fine
    # detach("package:dplyr", unload=TRUE) # will return error if package wasn't already loaded, that's fine
  # alternatively re-start R / RStudio
  
  # load packages
    library(plyr) 
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
  
    
# 1. IMPORT AND MERGE ALL .CSV FILES FROM ONE FOLDER ####

    ## a) load data ####
   
    reference_files = list.files("CSV Input/reference", pattern = "\\.csv$", full.names = TRUE)
    
    reference_data = reference_files %>%
      lapply(read_csv) %>%
      bind_rows()

    
    trainee_files = list.files("CSV Input/trainee", pattern = "\\.csv$", full.names = TRUE)
    
    trainee_data = trainee_files %>%
      lapply(read_csv) %>%
      bind_rows()

 
    ## b) check data ####
    
    # glimpse data
    glimpse(reference_data)
    glimpse(trainee_data)
    
    
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
    
    sort(unique(df$Scoring))
    
    
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
    
    
    # for focal protocols we do not distinguish between the different vocalisation types,
    # i still am for training purposes though, so this code is commented out:
    # df$Value <- gsub('Loud', 'Vocalisation', df$Value)
    # df$Value <- gsub('Soft', 'Vocalisation', df$Value)
    # df$Value <- gsub('Haa call (food)', 'Vocalisation', df$Value)
    # unique(df$Value)
    
  # adjustment for group protocols: 
    # in focal protocols everybody chooses the same subject (the focal), therefore coding is standardised
    # in group protocols coding A>B and B<A are both valid, therefore standardisation is not guaranteed
    # to make sure the script doesn't interpret one identical event as two different events, I:
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
    # I tried doing it directly in the original formula above, but for some reason couldn't get it to work
    # I also tried it by calling on the column names directly, but that also didn't work.
    # So now I am saving each column as "x" and saving the alphabetized output as "y" to then save that as the new column
    # It isn't very elegant, but for some weird reason the only way I got this to work. 
    # Somebody more knowledgeable and willing to put in the time could maybe clean this code at some point
  
    x=df$initiation_emitter # save the column i want to alphabetize as x
    y = unname(sapply(x, function(x) {
      paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse='_OR_')} )) # alphabetize, save as y
    df$initiation_emitter2=y # save y as the new column 
    
    x=df$initiation_receiver # same for the other categories
    y = unname(sapply(x, function(x) {
      paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse='_OR_')} ))
    df$initiation_receiver2=y
    # for some reason this column has blanks instead of NA. can't figure out why. this fixes it:
    df$initiation_receiver2[df$initiation_receiver2 == ""] <- NA 
    
    x=df$termination_emitter
    y = unname(sapply(x, function(x) {
      paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse='_OR_')} ))
    df$termination_emitter2=y
    
    x=df$termination_receiver
    y = unname(sapply(x, function(x) {
      paste(sort(trimws(strsplit(x[1], ',')[[1]])), collapse='_OR_')} ))
    df$termination_receiver2=y
    
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
  # only non-social
    pivot.all_non_social <- pivot.all %>% 
      filter(Social=="non_social")
    head(pivot.all_non_social)
  # only social
    pivot.all_social <- pivot.all %>% 
      filter(Social=="social")
    head(pivot.all_social)
  # social data, with partner but without directionality
    pivot.partner_without_directionality <- df %>% 
      filter(Social=="social") %>%
      select(Coder, Partner, Value, Duration)%>% 
      group_by(Coder, Partner, Value) %>% 
      summarise(TotalDuration = sum(Duration), TotalFrequency = length(Duration))
    head(pivot.partner_without_directionality)
  # social data, without partner and without directionality
    pivot.no_partner_no_directionality <- df %>% 
      filter(Social=="social") %>%
      select(Coder, Value, Duration)%>% 
      group_by(Coder, Value) %>% 
      summarise(TotalDuration = sum(Duration), TotalFrequency = length(Duration))
    head(pivot.partner_without_directionality)
   
  # without modifiers at all
    pivot.all.no.modifiers <- df %>% 
      select(Coder, Scoring, initiation_dyad, Value, Duration, termination_dyad, Social) %>% 
      group_by(Coder, Scoring, initiation_dyad, Value, termination_dyad, Social) %>% 
      summarise(
        TotalDuration = sum(Duration, na.rm = TRUE),  # Handle NA values
        TotalFrequency = n()                         # Count rows
      )
    
  # without meter values for neighbours, this will only be used for FRQ 
    pivot.all.no.neighbour.meters <- pivot.all %>% 
      mutate(
        Modifiers = if_else(
          Value == "Neighbour",
          str_remove(Modifiers, ".*\\+"),  # remove everything before and including the "+"
          Modifiers  # keep the original modifier if value is not "Neighbour"
        )
      )
    
    
    
    
    
  # spread TotalDuration and TotalFrequency between Coders for separate datasets  
    IORT.data.all=as.data.frame(pivot.all %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.non_social=as.data.frame(pivot.all_non_social %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.social=as.data.frame(pivot.all_social %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.partner_without_directionality=as.data.frame(pivot.partner_without_directionality %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.no_partner_no_directionality=as.data.frame(pivot.no_partner_no_directionality %>% 
      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    
    IORT.data.all.no.modifiers=
      as.data.frame(pivot.all.no.modifiers %>% 
                      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    IORT.data.all.no.neighbour.meters=
      as.data.frame(pivot.all.no.neighbour.meters %>% 
                      pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
    
    
    
    
    
# 4. RUN IORTs FOR ALL DATASETS USING ICCs #####
    
   ## a) set names ####
    names(IORT.data.all) 
    
    # get dynamic placeholders from df
    placeholder_reference <- df %>%
      filter(coder_source == "reference") %>%
      distinct(Coder) %>%
      pull()
    
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
    
    
    
   ## b) Run ICC ####
    # calculate ICCs for Total Durations and Total Frequencies, the only two test results required for reporting.
    # I calculate scores for Durations and Frequencies separately, due to the nature of the data 
    # Test parameters are selected according to https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/
    
    
    # All Sociality, with Termination, with Modifiers:  
      # TotalDurations
        # select the required columns, replace NA with 0, remove rows where both coders have 0
        all.dur <- IORT.data.all %>%
          mutate(across(all_of(dur_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(dur_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        all.dur.result <- icc(all.dur, model = "twoway", type = "agreement")
        
  
      # TotalFrequencies
        # Loopy Frequencies can't be 0 for both coders, so no need to remove it, however: i don't know what this script might
        # get used for in the future, so i leave the code in. it won't harm 2 coder loopy data, but if e.g. solomon coder
        # data gets transformed to loopy data and then fed into this script it might be a different story, who knows.
        all.frq <- IORT.data.all %>%
          mutate(across(all_of(frq_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(frq_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        all.frq.result <- icc(all.frq, model = "twoway", type = "agreement") 
        
   
            ## c) alternative frequency test Krippendorff Alpha ####
            # ICC should be fine, but sometimes people are sceptical and prefer Krippendorff Alpha
            # TotalFrequencies using krippendorff alpha
            all.frq.kripp = kripp.alpha(t(all.frq), method = "interval")
            all.frq.kripp
            
            # # optional: get CIs for kripp.alpha via bootstrapping 
            # set.seed(123)
            # B <- 2000
            # boot_alpha <- replicate(B, {
            #   idx <- sample(seq_len(nrow(all.frq)), replace = TRUE)
            #   irr::kripp.alpha(t(all.frq[idx, ]), method = "interval")$value
            # })
            # quantile(boot_alpha, c(0.025, 0.975))
            
            # compare approaches:
            all.frq.result # default icc used for the pdf report
            all.frq.kripp # krippendorff alpha, not used for the pdf report
            c(alpha_interval = all.frq.kripp$value, ICC_A1 = all.frq.result$value) # should be pretty close
            
            # for the type of loopy data we work with, these different approaches should give very similar results,
            # especially towards the end of the training once coders are pretty good.
            # at the beginning results might differ between the tests, because the handle mismatches a bit differently
    
        
    ## d) additional IORs for sub-categories ####
        # these are not to be reported as IOR scores for publications, but help identify areas requiring further training
    # Only Non-Social Behaviours
      # TotalDurations
       non_social.dur <- IORT.data.non_social %>%
          mutate(across(all_of(dur_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(dur_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        non_social.dur.result = icc (non_social.dur, model="twoway", type= "agreement")
        non_social.dur.result
        # directional durations (continuous behaviours, e.g. eating, preening) 
      # TotalFrequencies
        non_social.frq <- IORT.data.non_social %>%
          mutate(across(all_of(frq_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(frq_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        non_social.frq.result = icc (non_social.frq, model="twoway", type= "agreement")
        non_social.frq.result
        # non-directional frequencies (instant events, e.g. jump, begging)  
    # Only Social Behaviours
        # TotalDurations
        social.dur <- IORT.data.social %>%
          mutate(across(all_of(dur_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(dur_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        social.dur.result = icc (social.dur, model="twoway", type= "agreement")
        social.dur.result
        # directional durations (continuous behaviours, e.g. eating, preening) 
      # TotalFrequencies
        social.frq <- IORT.data.social %>%
          mutate(across(all_of(frq_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(frq_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        social.frq.result=icc (social.frq, model="twoway", type= "agreement")
        social.frq.result
        # non-directional frequencies (instant events, e.g. jump, begging) 
    # Without Directionality
      # TotalDurations
        partner_without_directionality.dur <- IORT.data.partner_without_directionality %>%
          mutate(across(all_of(dur_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(dur_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        partner_without_directionality.dur.result = icc (partner_without_directionality.dur, model="twoway", 
                                                         type= "agreement")
        partner_without_directionality.dur.result
        # directional durations (continuous behaviours, e.g. eating, preening) 
       # TotalFrequencies
        partner_without_directionality.frq <- IORT.data.partner_without_directionality %>%
          mutate(across(all_of(frq_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(frq_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        partner_without_directionality.frq.result = icc (partner_without_directionality.frq, model="twoway",
                                                         type= "agreement")
        partner_without_directionality.frq.result 
    # Without Termination
      # TotalDurations
        no_partner_no_directionality.dur <- IORT.data.no_partner_no_directionality %>%
          mutate(across(all_of(dur_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(dur_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        no_partner_no_directionality.dur.result = icc (no_partner_no_directionality.dur, model="twoway", type= "agreement")
        no_partner_no_directionality.dur.result
        # directional durations (continuous behaviours, e.g. eating, preening) 
        # TotalFrequencies
        no_partner_no_directionality.frq <- IORT.data.no_partner_no_directionality %>%
          mutate(across(all_of(frq_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(frq_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        no_partner_no_directionality.frq.result = icc (no_partner_no_directionality.frq, model="twoway", type= "agreement")
        no_partner_no_directionality.frq.result
        # non-directional frequencies (instant events, e.g. jump, begging)  
    # Without Modifiers
        # TotalDurations
        all.dur.no.modifiers <- IORT.data.all.no.modifiers %>%
          mutate(across(all_of(dur_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(dur_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        all.dur.no.modifiers.result = icc (all.dur.no.modifiers, model="twoway", type= "agreement")
        all.dur.no.modifiers.result   
        # TotalFrequencies
        all.frq.no.modifiers <- IORT.data.all.no.modifiers %>%
          mutate(across(all_of(frq_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(frq_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        all.frq.no.modifiers.result = icc (all.frq.no.modifiers, model="twoway", type= "agreement")
        all.frq.no.modifiers.result
     # Without neighbour meters  
        # TotalFrequencies
        all.frq.no.neighbour.meters <- IORT.data.all.no.neighbour.meters %>%
          mutate(across(all_of(frq_cols), ~ tidyr::replace_na(., 0))) %>%
          select(all_of(frq_cols)) %>%
          filter(if_any(everything(), ~ . != 0))
        
        all.frq.no.neighbour.meters.result = icc (all.frq.no.neighbour.meters, model="twoway", type= "agreement")
        all.frq.no.neighbour.meters.result
        
        
# 5. ALL ICC RESULTS ####
    icc.plot <- data.frame(matrix(ncol = 2, nrow = 13))
    colnames(icc.plot) <- c("category", "result")
    icc.plot$category=c("all data (dur)", "all data (frq)", 
                        "non-social (dur)", "non-social (frq)",
                        "only social (dur)", "only social (frq)",
                        "only partner, no directionality (dur)", "only partner, no directionality (frq)",
                        "no partner, no directionality (dur)", "no partner, no directionality (frq)", 
                        "all data (dur), no modifiers", "all data (frq), no modifiers",
                        "all data (frq), no neighbour meter value")
    icc.plot$result=as.numeric(c(all.dur.result$value, all.frq.result$value, 
                                 non_social.dur.result$value, non_social.frq.result$value,
                                 social.dur.result$value, social.frq.result$value,
                                 partner_without_directionality.dur.result$value, 
                                 partner_without_directionality.frq.result$value,
                                 no_partner_no_directionality.dur.result$value, 
                                 no_partner_no_directionality.frq.result$value,
                                 all.dur.no.modifiers.result$value,
                                 all.frq.no.modifiers.result$value,
                                 all.frq.no.neighbour.meters.result$value
                                 ))
 
    icc.plot$score=icc.plot$result
    icc.plot=icc.plot %>% mutate(score = ifelse(score >= 0.9, "excellent", score),
                        score = ifelse(score < 0.9, "below excellent", score))
    
   # AS PLOT
    # create basic plot
    icc.plot$category <- factor(icc.plot$category, levels=c("all data (dur)", "non-social (dur)", 
                                                            "only social (dur)", "only partner, no directionality (dur)", 
                                                            "no partner, no directionality (dur)",
                                                            "all data (frq)", "non-social (frq)",
                                                            "only social (frq)", "only partner, no directionality (frq)",
                                                            "no partner, no directionality (frq)", 
                                                            "all data (dur), no modifiers", "all data (frq), no modifiers",
                                                            "all data (frq), no neighbour meter value"))
    
    icc.plot$score <- factor(icc.plot$score, levels = c("excellent", "below excellent"))
    
    p=ggplot(icc.plot, aes(x=category, y=result, shape=score))+
      geom_point()+  
      ylim(0, 1)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_shape_manual(values=c(16, 15))
    # define limits for acceptable ICC IORT scores
    lim1 <- data.frame(xmin=-Inf, xmax=+Inf, ymin=0, ymax=0.5)
    lim2 <- data.frame(xmin=-Inf, xmax=+Inf, ymin=0.5, ymax=0.75)
    lim3 <- data.frame(xmin=-Inf, xmax=+Inf, ymin=0.75, ymax=0.9)
    lim4 <- data.frame(xmin=-Inf, xmax=+Inf, ymin=0.9, ymax=1)
    # highlight limits in plot
    IORT.plot = p + geom_rect(data=lim1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  fill="brown3",
                  alpha=0.6,
                  inherit.aes = FALSE)+
      geom_rect(data=lim2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill="coral",
                alpha=0.6,
                inherit.aes = FALSE)+
      geom_rect(data=lim3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill="forestgreen",
                alpha=0.6,
                inherit.aes = FALSE)+
      geom_rect(data=lim4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill="chartreuse",
                alpha=0.6,
                inherit.aes = FALSE)+
      labs(title = paste("Results of the Inter Observer Reliability Test for ", placeholder_trainee, " \n calculated on ",
                         Sys.Date(), sep = " "),
           subtitle = "This entire PDF Report was created using the 'Automated IORT' R script of Christian Blum 
           (christian.blum@univie.ac.at).",
           x = "Categories",
           y = "IOR Score",
           caption = paste("Additional information on this PDF report: \n
           Only the categories 'all data (dur)' and 'all data (frq)' are to be used for publication, the rest only help identify areas in need of further training. \n 
           Colored backgrounds indicate limits:
           Values less than 0.5, between 0.5 and 0.75, between 0.75 and 0.9, and greater than 0.9 are indicative of 
           poor (red), moderate (orange), good (dark green), and excellent (bright green) reliability, respectively. 
           We only accept excellent scores, otherwise there needs to be more training. Scores were calculated using the library 'irr' and the function
           >icc (data, model='twoway', type= 'agreement') < \n
           Trainers: Please also evaluate scorings manually and in detail, do not only rely on ICC scores."))+
      theme(
        plot.caption = element_text(hjust = 0, size=7)
      )
    IORT.plot
    
  # AS TABLE
    IORT.table = icc.plot  
    IORT.table
      
      # checking the different sub-categories' IORT-scores can help identify where the scoring differs most
      # for further information I did some more data exploration below

         
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
  
  
  # now we spread TotalDuration and TotalFrequency between Coders for separate datasets
    pivot.exploration.all=as.data.frame(exploration.data %>% 
                                pivot_wider(names_from = Coder, values_from = c(TotalDuration, TotalFrequency)))
  # now let's calculate the differences between the coders' scored durations and frequencies
  # first we replace all entries of "NA" with "0"
    pivot.exploration.all[is.na(pivot.exploration.all)] <- 0
  
  # before we calculate the differences, we want to make sure not to clutter up things with useless info:
  # all behaviours have frequencies, but only some behaviours have durations
  # when investigating duration differences, we don't need to see zeros listed for instant events
  # so let's create another dataframe for only continuous behaviours.
    pivot.exploration.continuous <- pivot.exploration.all %>% 
    filter(Type=="continuous") 
    unique(pivot.exploration.continuous$Value) # only shows behaviours with durations, good.
    
  # to simplify coding from here on out, lets rename the datasets:
    diff.dur = pivot.exploration.continuous # no instant event behaviours (they don't have durations)
    diff.frq = pivot.exploration.all # all behaviours (continuous behaviours have frequencies too (=bouts))
    
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
    
    # sum differences by Value (=Behaviour)
    diff.dur.by_value=aggregate(TotalDuration_Differences ~ Value, data=diff.dur, FUN=sum)
    diff.frq.by_value=aggregate(TotalFrequency_Differences ~ Value, data=diff.frq, FUN=sum)
    # sum differences by Value (=Behaviour) and Scoring
    diff.dur.by_value_and_scoring=aggregate(TotalDuration_Differences ~ Scoring + Value, data=diff.dur, FUN=sum)
    diff.frq.by_value_and_scoring=aggregate(TotalFrequency_Differences ~ Scoring + Value, data=diff.frq, FUN=sum)
  
  # now we sort those dataframes by size of difference:
    diff.dur.by_value = diff.dur.by_value[order(
      diff.dur.by_value$TotalDuration_Differences, decreasing = TRUE),]
    
    diff.dur.by_value_and_scoring = diff.dur.by_value_and_scoring[order(
      diff.dur.by_value_and_scoring$TotalDuration_Differences,decreasing = TRUE),]
    
    diff.frq.by_value = diff.frq.by_value[order(
      diff.frq.by_value$TotalFrequency_Differences, decreasing = TRUE),]
    
    diff.frq.by_value_and_scoring = diff.frq.by_value_and_scoring[order(
      diff.frq.by_value_and_scoring$TotalFrequency_Differences,decreasing = TRUE),]
    
  # let's also plot them:
  
    # Duration Differences Overall
      DUR.total = ggplot(diff.dur.by_value, aes(x = reorder(Value, -TotalDuration_Differences), 
                                    y = TotalDuration_Differences)) +
                    geom_bar(stat = "identity")+
                    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
                          plot.caption = element_text(hjust = 0, size=5))+
                    ggtitle("Coding Differences") + ylab("Sum of Differences in coded Durations (s)") + xlab("Behaviours")+
                    labs(subtitle = "Duration Differences across all Videos",
                         caption = "Sum of all differences of coded durations, per behaviour, across all videos.
                         This plot doesn't inform on who coded more than the other, because absolute values had to be used. 
                         Otherwise differences might cancel each other out (20s more in one and 20s less in another video/bout
                         would add up to 0s differences).")
    
    # Frequency Differenes Overall
      FRQ.total = ggplot(diff.frq.by_value, aes(x = reorder(Value, -TotalFrequency_Differences), 
                                    y = TotalFrequency_Differences)) +
                    geom_bar(stat = "identity")+
                    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
                          plot.caption = element_text(hjust = 0, size=5))+
                    ggtitle("Coding Differences") + ylab("Sum of Differences in coded Frequencies (Events)") + xlab("Behaviours")+
        labs(subtitle = "Frequency Differences across all Videos",
             caption = "Sum of all differences of coded frequencies, per behaviour, across all videos.
                         This plot doesn't inform on who coded more than the other, because absolute values had to be used. 
                         Otherwise differences might cancel each other out (20s more in one and 20s less in another video/bout
                         would add up to 0s differences).")
   
    # Duration Differences per Video
      DUR.per_video = ggplot(diff.dur.by_value_and_scoring, aes(x = reorder(Value, -TotalDuration_Differences), 
                                                y = TotalDuration_Differences)) +
                        geom_bar(stat = "identity")+
                        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
                              strip.text = element_text(size=5),
                              plot.caption = element_text(hjust = 0, size=5))+
                        facet_wrap(~ Scoring, scales= "free")+
                        ggtitle("Coding Differences") + ylab("Sum of Differences in coded Durations (s)") + xlab("Behaviours")+
        labs(subtitle = "Duration Differences per Video",
             caption = "Sum of all differences of coded durations, per behaviour, across all videos.
                         This plot doesn't inform on who coded more than the other, because absolute values had to be used. 
                         Otherwise differences might cancel each other out (20s more in one and 20s less in another video/bout
                         would add up to 0s differences). Be aware of different y-axis limits.")
    
    # Frequency Differences per Video - this one seems very cluttered, but it's here if you want it
      FRQ.per_video = ggplot(diff.frq.by_value_and_scoring, aes(x = reorder(Value, -TotalFrequency_Differences), 
                                                y = TotalFrequency_Differences)) +
                        geom_bar(stat = "identity")+
                        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
                              strip.text = element_text(size=5),
                              plot.caption = element_text(hjust = 0, size=5))+
                        facet_wrap(~ Scoring, scales = "free")+
                        scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) + # Ensure integer breaks
                        ggtitle("Coding Differences") + ylab("Sum of Differences in coded Frequencies (Events)") + xlab("Behaviours")+
        labs(subtitle = "Frequency Differences per Video",
             caption = "Sum of all differences of coded frequencies, per behaviour, across all videos.
                         This plot doesn't inform on who coded more than the other, because absolute values had to be used. 
                         Otherwise differences might cancel each other out (20s more in one and 20s less in another video/bout
                         would add up to 0s differences). Be aware of different y-axis limits.")
      
      
# new plot idea:
      
     
     
      # Get unique Scoring categories
      scoring_categories <- unique(diff.dur.by_value_and_scoring$Scoring)
      
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
          
          # Create the duration plot for the current Scoring category
          dur_plot <- ggplot(dur_data, aes(x = reorder(Value, -TotalDuration_Differences), y = TotalDuration_Differences)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
                  strip.text = element_text(size = 5),
                  plot.caption = element_text(hjust = 0, size = 5),
                  plot.title = element_text(size = 8),
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
                  plot.title = element_text(size = 8),
                  axis.title.y = element_text(size = 8)) +
            ggtitle(scoring) +
            ylab("FRQ Difference (count)") +
            xlab(NULL)
          
          # Combine the two plots side by side
          paired_plots[[scoring]] <- dur_plot | frq_plot
        }
        
        # Combine all paired plots for the current group into a single stacked layout
        final_plots[[i]] <- wrap_plots(paired_plots, ncol = 1)
      }
      
      # Display all the plots (one at a time)
      for (plot in final_plots) {
        print(plot)
      }
      
  
      
      
      
  # # this is no longer needed, we have updated the reference codings to include focal_duration
  # # special addition for crow FOC ethogram:
  # # difference for focal_duration is typcially much larger than for the other behaviours, so we'll plot that separately.
  # # first redo the plots without focal_duration:
  # 
  #     # Remove "focal_duration" from the data
  #     diff.dur.by_value_no_focal <- diff.dur.by_value %>% filter(Value != "focal_duration")
  #     diff.dur.by_value_and_scoring_no_focal <- diff.dur.by_value_and_scoring %>% filter(Value != "focal_duration")
  # 
  #     # Duration Differences Overall
  #     DUR.total_no_focal = ggplot(diff.dur.by_value_no_focal, aes(x = reorder(Value, -TotalDuration_Differences),
  #                                               y = TotalDuration_Differences)) +
  #       geom_bar(stat = "identity")+
  #       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
  #             plot.caption = element_text(hjust = 0, size=5))+
  #       ggtitle("Coding Differences without focal_duration") + ylab("Sum of Differences in coded Durations (s)") + xlab("Behaviours")+
  #       labs(subtitle = "Duration Differences across all Videos",
  #            caption = "Same as above, but for better readability the behaviour focal_duration was excluded.")
  # 
  #     # Duration Differences per Video
  #     DUR.per_video_no_focal = ggplot(diff.dur.by_value_and_scoring_no_focal, aes(x = reorder(Value, -TotalDuration_Differences),
  #                                                               y = TotalDuration_Differences)) +
  #       geom_bar(stat = "identity")+
  #       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
  #             strip.text = element_text(size=5),
  #             plot.caption = element_text(hjust = 0, size=5))+
  #       facet_wrap(~ Scoring, scales= "free")+
  #       ggtitle("Coding Differences without focal_duration") + ylab("Sum of Differences in coded Durations (s)") + xlab("Behaviours")+
  #       labs(subtitle = "Duration Differences per Video",
  #            caption = "Same as above, but for better readability the behaviour focal_duration was excluded.")
  # 
  #    # then plot the differences for focal_duration separately:
  # 
  # 
  #     diff.dur.by_value_only_focal <- diff.dur.by_value %>% filter(Value == "focal_duration")
  #     diff.dur.by_value_and_scoring_only_focal <- diff.dur.by_value_and_scoring %>% filter(Value == "focal_duration")
  # 
  #     # Duration Differences Overall
  #     DUR.total_only_focal = ggplot(diff.dur.by_value_only_focal, aes(x = reorder(Value, -TotalDuration_Differences),
  #                                                                 y = TotalDuration_Differences)) +
  #       geom_bar(stat = "identity")+
  #       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
  #             plot.caption = element_text(hjust = 0, size=5))+
  #       ggtitle("Coding Differences") + ylab("Sum of Differences in coded Durations (s)") + xlab("Behaviours")+
  #       labs(subtitle = "Duration Differences across all Videos",
  #            caption = "Same as above, but for better readability, only the behaviour focal_duration was included.")
  # 
  # 
  #     # Duration Differences per Video
  #     DUR.per_video_only_focal = ggplot(diff.dur.by_value_and_scoring_only_focal, aes(x = reorder(Value, -TotalDuration_Differences),
  #                                                                                 y = TotalDuration_Differences)) +
  #       geom_bar(stat = "identity")+
  #       theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
  #             strip.text = element_text(size=5),
  #             plot.caption = element_text(hjust = 0, size=5))+
  #       facet_wrap(~ Scoring, scales= "free")+
  #       ggtitle("Coding Differences") + ylab("Sum of Differences in coded Durations (s)") + xlab("Behaviours")+
  #       labs(subtitle = "Duration Differences per Video",
  #            caption = "Same as above, but for better readability, only the behaviour focal_duration was included.")
      
      
      
      
      
      
      
  # these are the seprate categories that inform you on what behaviours differed the most:
    diff.dur.by_value # differences in durations overall
    diff.frq.by_value # differences in frequencies overall
    diff.dur.by_value_and_scoring # differences in durations per scoring (= focal video)
    diff.frq.by_value_and_scoring # differences in frequencies per scoring (=focal)
    
    # diff.dur.by_value_no_focal # differences in durations overall
    # diff.dur.by_value_and_scoring_no_focal # differences in durations per scoring (= focal video)
    
    # diff.dur.by_value_only_focal
    # diff.dur.by_value_and_scoring_only_focal

    
    
    
# 7. EXPORT ALL RESULTS ####
    grid.table(IORT.table)
    
    text_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "I also plot the same information in a different layout here.\n
               This might make it easier for the training, when going from Scoring to Scoring on Loopy. \n
               We now no longer have to scroll up and down between the duration and frequency plots.",
               size = 3, fontface = "plain", hjust = 0.5, vjust = 0.5) +
      theme_void()+  # Remove all axes and background
      theme(plot.margin = margin(10, 10, 10, 10))  # Add some padding around the text
    
    
    text_plot
    
    
    
  ## a) create pdf file ####
    # all plots to one pdf:
    date_tag <- format(Sys.Date(), "%Y-%m-%d")
    safe_name <- gsub("[^A-Za-z0-9._-]", "_", placeholder_trainee)
    pdf(paste0("IORT_", safe_name, "_results_", date_tag, ".pdf"))
        IORT.plot
        plot.new()
        grid.table(IORT.table)
        DUR.total
        FRQ.total
        DUR.per_video
        FRQ.per_video
        
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
   
      
  ## b) create excel file ####
      
         
   # all icc results and dataframes to one xlsx:     
    
     all.dur.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(all.dur.result.table) <- c("rowname", "value")
     all.dur.result.table$rowname=rownames(as.data.frame(unlist(all.dur.result)))
     all.dur.result.table$value=unlist(all.dur.result)
     
     all.frq.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(all.frq.result.table) <- c("rowname", "value")
     all.frq.result.table$rowname=rownames(as.data.frame(unlist(all.frq.result)))
     all.frq.result.table$value=unlist(all.frq.result)
     
     non_social.dur.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(non_social.dur.result.table) <- c("rowname", "value")
     non_social.dur.result.table$rowname=rownames(as.data.frame(unlist(non_social.dur.result)))
     non_social.dur.result.table$value=unlist(non_social.dur.result)
     
     non_social.frq.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(non_social.frq.result.table) <- c("rowname", "value")
     non_social.frq.result.table$rowname=rownames(as.data.frame(unlist(non_social.frq.result)))
     non_social.frq.result.table$value=unlist(non_social.frq.result)
     
     social.dur.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(social.dur.result.table) <- c("rowname", "value")
     social.dur.result.table$rowname=rownames(as.data.frame(unlist(social.dur.result)))
     social.dur.result.table$value=unlist(social.dur.result)
     
     social.frq.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(social.frq.result.table) <- c("rowname", "value")
     social.frq.result.table$rowname=rownames(as.data.frame(unlist(social.frq.result)))
     social.frq.result.table$value=unlist(social.frq.result)
     
     partner_without_directionality.dur.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(partner_without_directionality.dur.result.table) <- c("rowname", "value")
     partner_without_directionality.dur.result.table$rowname=rownames(as.data.frame(unlist(partner_without_directionality.dur.result)))
     partner_without_directionality.dur.result.table$value=unlist(partner_without_directionality.dur.result)
     
     partner_without_directionality.frq.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(partner_without_directionality.frq.result.table) <- c("rowname", "value")
     partner_without_directionality.frq.result.table$rowname=rownames(as.data.frame(unlist(partner_without_directionality.frq.result)))
     partner_without_directionality.frq.result.table$value=unlist(partner_without_directionality.frq.result)
     
     no_partner_no_directionality.dur.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(no_partner_no_directionality.dur.result.table) <- c("rowname", "value")
     no_partner_no_directionality.dur.result.table$rowname=rownames(as.data.frame(unlist(no_partner_no_directionality.dur.result)))
     no_partner_no_directionality.dur.result.table$value=unlist(partner_without_directionality.dur.result)
     
     no_partner_no_directionality.frq.result.table <- data.frame(matrix(ncol = 2, nrow = 15))
     colnames(no_partner_no_directionality.frq.result.table) <- c("rowname", "value")
     no_partner_no_directionality.frq.result.table$rowname=rownames(as.data.frame(unlist(no_partner_no_directionality.frq.result)))
     no_partner_no_directionality.frq.result.table$value=unlist(no_partner_no_directionality.frq.result)
     
     all.data = df # duplicate df for export
     names(all.data) # check columns present, so we can remove unwanted ones
     all.data = subset(all.data, select = -c(initiation_emitter, initiation_receiver, termination_emitter, termination_receiver, 
                                             initiation_emitter2, initiation_receiver2, termination_emitter2, termination_receiver2))
     
     
    
     out_xlsx <- sprintf("IORT_%s_results_%s.xlsx", safe_name, date_tag)
     
     sheets <- list(
       IORT_Summary = IORT.table,
       IORT.all.dur = all.dur.result.table,
       IORT.all.frq = all.frq.result.table,
       IORT.non_social.dur = non_social.dur.result.table,
       IORT.non_social.frq = non_social.frq.result.table,
       IORT.only_social.dur = social.dur.result.table,
       IORT.only_social.frq = social.frq.result.table,
       IORT.partner_without_directionality.dur = partner_without_directionality.dur.result.table,
       IORT.partner_without_directionality.frq = partner_without_directionality.frq.result.table,
       
       # FIXED: unique names for the “no partner, no directionality” sheets
       IORT.no_partner_no_directionality.dur = no_partner_no_directionality.dur.result.table,
       IORT.no_partner_no_directionality.frq = no_partner_no_directionality.frq.result.table,
       
       Duration_Total = diff.dur.by_value,
       Frequency_Total = diff.frq.by_value,
       Duration_per_Video = diff.dur.by_value_and_scoring,
       Frequency_per_Video = diff.frq.by_value_and_scoring,
       All_Data = all.data
     )
     
     write_xlsx(sheets, path = out_xlsx)
     
     
     
     
      
     
     
    sessionInfo() 
    # R version 4.3.1 (2023-06-16 ucrt)
    # Platform: x86_64-w64-mingw32/x64 (64-bit)
    # Running under: Windows 11 x64 (build 22631)
    # 
    # Matrix products: default
    # 
    # 
    # locale:
    # [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
    # [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
    # [5] LC_TIME=English_United Kingdom.utf8    
    # 
    # time zone: Europe/Vienna
    # tzcode source: internal
    # 
    # attached base packages:
    # [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
    # 
    # other attached packages:
    # [1] writexl_1.5.0  gridExtra_2.3  ggplot2_3.5.0  irr_0.84.1     lpSolve_5.6.20 tidyr_1.3.1    readr_2.1.5   
    # [8] dplyr_1.1.2    plyr_1.8.9    
    # 
    # loaded via a namespace (and not attached):
    #   [1] utf8_1.2.3        generics_0.1.3    lattice_0.21-8    lme4_1.1-35.1     hms_1.1.3         magrittr_2.0.3   
    # [7] Matrix_1.6-5      purrr_1.0.2       fansi_1.0.4       scales_1.3.0      cli_3.6.1         rlang_1.1.1      
    # [13] crayon_1.5.2      bit64_4.0.5       munsell_0.5.0     splines_4.3.1     withr_3.0.0       tools_4.3.1      
    # [19] parallel_4.3.1    tzdb_0.4.0        nloptr_2.0.3      minqa_1.2.6       colorspace_2.1-0  boot_1.3-28.1    
    # [25] vctrs_0.6.3       R6_2.5.1          lifecycle_1.0.4   bit_4.0.5         vroom_1.6.5       MASS_7.3-60      
    # [31] pkgconfig_2.0.3   pillar_1.9.0      gtable_0.3.4      glue_1.6.2        Rcpp_1.0.11       tibble_3.2.1     
    # [37] tidyselect_1.2.1  rstudioapi_0.15.0 farver_2.1.1      nlme_3.1-162      labeling_0.4.3    DHARMa_0.4.6     
    # [43] compiler_4.3.1   
  
    
    # summarize findings
    IORT.plot # print summary plot
    IORT.table # print summary table
    table(df$coder_source, df$Coder) # confirm reference and trainee were correctly placed in folders
  