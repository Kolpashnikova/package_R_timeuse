#' Time Use Totals in R
#'
#' @description
#' Creates a list of two variables, "totals" and "activities." The "totals" variable represents
#' a list of n (where n is the number of coded activities) of percent this activity takes up
#' in the total of all activities across all observations. The "activities" variable
#' represents a character list of coded activities in the same order as in they appear in the
#' totals.
#'
#' @param df an ATUS-X dataframe constructed by ipumsr package.
#'
#' @param path_to_activity_codes a path to an .csv file with the complete coding of activities. An example of the file can be found
#' here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv
#'
#' @references
#' Kolpashnikova, Kamila. (2022). Time Use Package for R. Toronto,ON: York University.
#'
#' @export
tu_totals <- function(df, path_to_activity_codes = "data/ATUS_codes.csv"){
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


  # dictionary
  act <- unique(act_codes$Name)
  names(act) <- unique(act_codes$Alphabet)

  mat = as.data.frame(table(sequences)*100/sum(table(sequences)))
  rownames(mat)<-mat$sequences
  rows = 1:length(mat[, 2])
  mat <- as.data.frame(t(mat[, 2]), row.names = 1)
  names(mat) <- as.character(rows)


  end.time <- Sys.time()

  t = end.time - start.time

  print(t)

  varList <- list("totals" = as.list(mat), "activities" = unique(act_codes$Name))

  return(varList)

}
