#' Time Use Transitions in R
#'
#' @description
#' Creates a list of two variables, "trate" and "activities." The "trate" variable
#' contains a n x n matrix (where n is the number of coded activities). Rows represent
#' the number of the activity from which the transition started, and columns represent
#' activities into which the transition ended. Transitions of an activity into itself is
#' always zero. The numbers in the matrix represent the percent of the total transitions
#' (sum of matrix). The "activities" variable contains a character list of activities in
#' the same order as they appear as the rows and columns in the trate matrix.
#'
#' @param df an ATUS-X dataframe constructed by ipumsr package.
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
tu_transitions <- function(df, path_to_activity_codes = "data/ATUS_codes.csv", w = NULL){
  start.time <- Sys.time()

  data = df

  # checking if all activities summing up to 1440
  data <- data %>% group_by(CASEID) %>% mutate(sum_dur = sum(DURATION))

  if(names(table(data$sum_dur)) != c("1440")){
    stop("Your subsample contains sequences that do not add up to 1440 minutes, or 24 hours")
  }

  # upload activity codes file
  act_codes <- read.csv(path_to_activity_codes)

  ## check if there are activities missing:
  if(length(setdiff(unique(data$ACTIVITY), unique(act_codes$ACTIVITY))) != 0){
    print(setdiff(unique(data$ACTIVITY), unique(act_codes$ACTIVITY)))
    stop("Your activity codes file does not contain all possible activity codes.")
  }

  ## attach the Codes and Alphabet
  total <- merge(data, act_codes,by="ACTIVITY", all.x = TRUE)

  total <- total[order(total$CASEID, total$PERNUM, total$ACTLINE),]

  ## create sequences of activities per activity (result: long list of combined sequences)
  sequences = rep(total$Alphabet, total$DURATION)

  ## separate the long list into sequences (1440 min in each sequence)
  seq = matrix(sequences, nrow=length(sequences)/1440, ncol=1440, byrow=T)

  ## transform matrix to dataframe
  seq <- as.data.frame(seq)

  ## create id
  seq$CASEID <- unique(total$CASEID)



  # dictionary
  act <- unique(act_codes$Name)
  names(act) <- unique(act_codes$Alphabet)

  trans <- data.frame(matrix(rep(0, length(act)^2),
                             nrow=length(act), ncol=length(act), byrow=T),
                      row.names = names(act))
  names(trans) <- names(act)

  activities<-c()
  for(i in 1:1440) {
    activities<-c(activities, paste("V", i, sep = ""))
  }

  if(!is.null(w)){
    ## weights only + remove duplicates
    weights <- data %>% select(c(CASEID, all_of(w)))
    weights <- weights[!duplicated(weights), ]
    weights[[w]] <- weights[[w]]*length(weights[[w]])/sum(weights[[w]])

    ## merge sequences with weights
    seq <- merge(seq, weights, by="CASEID", all.x = TRUE)

    w = "WT06"

    temp <- seqdef(seq,
                   var = activities,
                   id = seq$CASEID,
                   weights = seq$WT06)

    trate <- seqtrate(temp, weighted = TRUE, count = TRUE)

    mat <- matrix(trate, nrow = length(trate)^0.5)

    for(i in 1:length(trate)^0.5){
      mat[i, i] = 0
    }

    mat = (mat/sum(mat)*100)


  } else {

    temp <- seqdef(seq,
                   var = activities,
                   id = seq$CASEID)

    trate <- seqtrate(temp, count = TRUE)

    mat <- matrix(trate, nrow = length(trate)^0.5)

    for(i in 1:length(trate)^0.5){
      mat[i, i] = 0
    }

    mat = (mat/sum(mat)*100)

  }

  end.time <- Sys.time()

  t = end.time - start.time

  print(t)

  varList <- list("trate" = round(mat,2), "activities" = unique(act_codes$Name))

  return(varList)

}
