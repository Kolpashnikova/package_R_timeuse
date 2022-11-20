#' Time Use Totals for Specific Activity Across US States in R
#'
#' @description
#' Creates a dataframe with 51 variables, corresponding to the US states and DC (two-letter lowercase codes).
#' Its values represent the mean for the activity of interest for a given state.
#'
#' @param df an ATUS-X dataframe constructed by ipumsr package.
#'
#' @param activity activity of interest. The name should appear in the .csv file mentioned in the path_to_activity_codes.
#'
#' @param path_to_activity_codes a path to an .csv file with the complete coding of activities. An example of the file can be found
#' here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv
#'
#' @param w the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."
#'
#' @references
#' Kolpashnikova, Kamila. (2022). Time Use Package for R. Toronto,ON: York University.
#'
#' @export
tu_maps <- function(df, activity, path_to_activity_codes = "data/ATUS_codes.csv", w = NULL){
  start.time <- Sys.time()

  data = df

  # checking if all activities summing up to 1440
  data <- data %>% group_by(CASEID) %>% mutate(sum_dur = sum(DURATION))

  if(names(table(data$sum_dur)) != c("1440")){
    stop("Your subsample contains sequences that do not add up to 1440 minutes, or 24 hours")
  }

  # upload activity codes file
  act_codes <- read.csv(path_to_activity_codes)

  # dictionary
  act <- unique(act_codes$Name)
  names(act) <- unique(act_codes$Alphabet)

  r_act <- unique(act_codes$Alphabet)
  names(r_act) <- unique(act_codes$Name)

  ## check if there are activities missing:
  if(length(setdiff(unique(data$ACTIVITY), unique(act_codes$ACTIVITY))) != 0){
    print(setdiff(unique(data$ACTIVITY), unique(act_codes$ACTIVITY)))
    stop("Your activity codes file does not contain all possible activity codes.")
  }

  ## attach the Codes and Alphabet
  total <- merge(data, act_codes,by="ACTIVITY", all.x = TRUE)

  total[total$Name == activity, "Alphabet"] <- 1
  total[!total$Name == activity, "Alphabet"] <- 0
  total$Alphabet <- as.numeric(total$Alphabet)


  #ordering the activity lines
  total <- total[order(total$CASEID, total$PERNUM, total$ACTLINE),]

  ## create sequences of activities per activity (result: long list of combined sequences)
  sequences = rep(total$Alphabet, total$DURATION)

  #sequences <- ifelse(is.element(sequences, activity), 1, 0)

  ## separate the long list into sequences (1440 min in each sequence)
  seq = matrix(sequences, nrow=length(sequences)/1440, ncol=1440, byrow=T)

  ## transform matrix to dataframe
  seq <- as.data.frame(seq, row.names = unique(total$CASEID))

  ## create id
  seq$CASEID <- unique(total$CASEID)

  w = "WT06"

  if(!is.null(w)){
    ## weights only + remove duplicates
    weights <- data %>% select(c(CASEID, all_of(w)))
    weights <- weights[!duplicated(weights), ]
    weights[[w]] <- weights[[w]]*length(weights[[w]])/sum(weights[[w]])

    ## merge sequences with weights
    seq <- merge(seq, weights, by="CASEID", all.x = TRUE)

    w = "WT06"
  }

  seq <- merge(seq, data[, c("CASEID", "STATEFIP")][!duplicated(data[, c("CASEID", "STATEFIP")]),],
               by="CASEID", all.x = TRUE)

  seq <- seq %>% mutate(state = STATEFIP,
                        state = case_when(STATEFIP == 1 ~ "al",
                                          STATEFIP == 2 ~ "ak",
                                          STATEFIP == 4 ~ "az",
                                          STATEFIP == 5 ~ "ar",
                                          STATEFIP == 6 ~ "ca",
                                          STATEFIP == 8 ~ "co",
                                          STATEFIP == 9 ~ "ct",
                                          STATEFIP == 10 ~ "de",
                                          STATEFIP == 11 ~ "dc",
                                          STATEFIP == 12 ~ "fl",
                                          STATEFIP == 13 ~ "ga",
                                          STATEFIP == 15 ~ "hi",
                                          STATEFIP == 16 ~ "id",
                                          STATEFIP == 17 ~ "il",
                                          STATEFIP == 18 ~ "in",
                                          STATEFIP == 19 ~ "ia",
                                          STATEFIP == 20 ~ "ks",
                                          STATEFIP == 21 ~ "ky",
                                          STATEFIP == 22 ~ "la",
                                          STATEFIP == 23 ~ "me",
                                          STATEFIP == 24 ~ "md",
                                          STATEFIP == 25 ~ "ma",
                                          STATEFIP == 26 ~ "mi",
                                          STATEFIP == 27 ~ "mn",
                                          STATEFIP == 28 ~ "ms",
                                          STATEFIP == 29 ~ "mo",
                                          STATEFIP == 30 ~ "mt",
                                          STATEFIP == 31 ~ "ne",
                                          STATEFIP == 32 ~ "nv",
                                          STATEFIP == 33 ~ "nh",
                                          STATEFIP == 34 ~ "nj",
                                          STATEFIP == 35 ~ "nm",
                                          STATEFIP == 36 ~ "ny",
                                          STATEFIP == 37 ~ "nc",
                                          STATEFIP == 38 ~ "nd",
                                          STATEFIP == 39 ~ "oh",
                                          STATEFIP == 40 ~ "ok",
                                          STATEFIP == 41 ~ "or",
                                          STATEFIP == 42 ~ "pa",
                                          STATEFIP == 44 ~ "ri",
                                          STATEFIP == 45 ~ "sc",
                                          STATEFIP == 46 ~ "sd",
                                          STATEFIP == 47 ~ "tn",
                                          STATEFIP == 48 ~ "tx",
                                          STATEFIP == 49 ~ "ut",
                                          STATEFIP == 50 ~ "vt",
                                          STATEFIP == 51 ~ "va",
                                          STATEFIP == 53 ~ "wa",
                                          STATEFIP == 54 ~ "wv",
                                          STATEFIP == 55 ~ "wi",
                                          STATEFIP == 56 ~ "wy"))


  columns <- names(seq)[(names(seq)!="CASEID") & (names(seq)!="STATEFIP") & (names(seq)!="state") & (names(seq)!="WT06")]

  seq$sums <- rowSums(seq[, columns])
  if(!is.null(w)){
    seq$weighted_sums <- seq$sums*seq[[w]]
  }else{
    seq$weighted_sums <- seq$sums
  }

  result <- seq %>% group_by(state) %>%
    summarise_at(vars(weighted_sums), list(act = mean))

  r <- round(result$act, 2)
  names(r) <- result$state
  r <- as.data.frame(t(as.matrix(r)))

  end.time <- Sys.time()

  t = end.time - start.time

  print(t)

  return(r)

}
