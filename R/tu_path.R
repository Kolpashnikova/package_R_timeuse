#' Time Use Path Files in R
#'
#' @description
#' Create 3 files. Filename1 is for the diaries in the long format for each minute.
#' Filename2 contains a JSON dictionary, where keys are activity names and values
#' are the numbers they correspond to. Filename3 contains weights for all the observations
#' in the same order as in Filename1.
#'
#' @param df an ATUS-X dataframe constructed by ipumsr package.
#'
#' @param path_to_activity_codes a path to an .csv file with the complete coding of activities. An example of the file can be found
#' here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv
#'
#' @param filename1 the path and name of the file where to save the long format diaries.
#'
#' @param filename2 the path and name of the file where to save the activities dictionary.
#'
#' @param filename3 the path and name of the file where to save weights.
#'
#' @param w the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."
#'
#' @references
#' Kolpashnikova, Kamila. (2022). Time Use Package for R. Toronto,ON: York University.
#'
#' @export
tu_path <- function(df, path_to_activity_codes = "data/ATUS_codes.csv",
                    filename1 = "ex_diaries.txt",
                    filename2 = "activities.txt",
                    filename3 = "weights.txt",
                    w = NULL){
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

  # dictionary
  act <- unique(act_codes$Name)
  names(act) <- unique(act_codes$Alphabet)

  numbered_act <- 1:length(unique(act_codes$Alphabet))
  names(numbered_act) <- unique(act_codes$Alphabet)

  # revalue
  for(i in 1:length(names(numbered_act))){
    seq[seq== names(numbered_act)[i]] <- i
  }

  act <- numbered_act
  names(act) <- unique(act_codes$Name)

  activities<-c()
  for(i in 1:1440) {
    activities<-c(activities, paste("V", i, sep = ""))
  }

  if(!is.null(w)){
    ## weights only + remove duplicates
    weights <- unique(data[[w]])
    weights <- weights*length(weights)/sum(weights)
    w = "WT06"

  } else {
    weights = NULL
  }

  seq_num <- matrix(as.integer(as.matrix(seq)), ncol = ncol(as.matrix(seq)))

  write(toJSON(seq_num), filename1)
  write(toJSON(as.data.frame(t(as.matrix(act)))), filename2)
  write(toJSON(weights), filename3)

  varList <- list("diaries_saved_to" = filename1, "activities" = filename2, "weights" = filename3)

  return(varList)

}
