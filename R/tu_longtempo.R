#' Time Use Tempogram (Long-form) in R
#'
#' @description
#' Creates a dataframe with two variables: "key" and "values." The "key" variable contains character values of the activity names.
#' The "values" variables is a list for the number of observations for each timestamp.
#'
#' @param df an ATUS-X dataframe constructed by ipumsr package.
#'
#' @param path_to_activity_codes a path to an .csv file with the complete coding of activities. An example of the file can be found
#' here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv
#'
#' @param w the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."
#'
#' @param granularity a number representing what gap in between timestamps to make (in minutes). For example, 15 means that
#' observations will be at the 15-minutes interval: 4am, 4:15am, 4:30am etc. The default is "full," meaning that every minute is
#' included into the tempogram.
#'
#' @param method is the method used for defining the activity code for the granularized tempogram. "First" means to take the first
#' activity code and "last" means to take the last activity code to represent the time interval.
#'
#' @references
#' Kolpashnikova, Kamila. (2022). Time Use Package for R. Toronto,ON: York University.
#'
#' @export
tu_longtempo <- function(df, path_to_activity_codes = "data/ATUS_codes.csv", w = NULL, granularity = "full", method = "first"){
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
  seq <- as.data.frame(seq, row.names = unique(total$CASEID))

  ## create id
  seq$CASEID <- unique(total$CASEID)

  if(!is.null(w)){
    ## weights only + remove duplicates
    weights <- data %>% select(c(CASEID, all_of(w)))
    weights <- weights[!duplicated(weights), ]
    weights[[w]] <- weights[[w]]*length(weights[[w]])/sum(weights[[w]])

    ## merge sequences with weights
    seq <- merge(seq, weights, by="CASEID", all.x = TRUE)

    w = "WT06"
  }

  # dictionary
  act <- unique(act_codes$Name)
  names(act) <- unique(act_codes$Alphabet)

  tempo <- data.frame(key = unique(act_codes$Name),
                      values=rep(NA, length(unique(act_codes$Name))),
                      row.names = unique(act_codes$Alphabet))

  tempogram <- data.frame(matrix(ncol = 1440, nrow = length(names(act))),
                          row.names = unique(act_codes$Alphabet))

  colnames(tempogram) <- names(seq)[!names(seq) %in% c("CASEID", w)]


  pb = txtProgressBar(min = 0, max = 1440, initial = 0)
  if(is.null(w)){
    for(i in 1:1440){
      #temp <- seq %>% count((!!sym(paste("V", i, sep = ""))))
      # the following two lines helps avoid zeroes
      seq[[paste("V", i, sep = "")]] <- factor(seq[[paste("V", i, sep = "")]], levels = unique(act_codes$Alphabet))
      temp <- seq %>% count((!!sym(paste("V", i, sep = ""))), .drop = FALSE)
      rownames(temp) <- temp[[paste("V", i, sep = "")]]
      tempogram[[paste("V", i, sep = "")]] <- temp$n
      setTxtProgressBar(pb,i)
    }
  } else {
    for(i in 1:1440){
      #temp <- seq %>% count((!!sym(paste("V", i, sep = ""))), wt = !!sym(w))
      # the following two lines helps avoid zeroes
      seq[[paste("V", i, sep = "")]] <- factor(seq[[paste("V", i, sep = "")]], levels = unique(act_codes$Alphabet))
      temp <- seq %>% count((!!sym(paste("V", i, sep = ""))), wt = !!sym(w), .drop = FALSE)
      rownames(temp) <- temp[[paste("V", i, sep = "")]]
      tempogram[[paste("V", i, sep = "")]] <- temp$n
      setTxtProgressBar(pb,i)
    }
  }
  close(pb)

  if(granularity != "full" & is.numeric(granularity)){
    if(method == "first"){
      repetitions = seq(240, 1679, granularity)
      positions = seq(240, 1679, granularity) - 239
    } else if(method == "last"){
      repetitions = seq(240, 1679, granularity)
      positions = seq(240, 1679, granularity) - 239 + granularity - 1
      repetitions <- repetitions[positions < 1441]
      positions <- positions[positions < 1441]
      if(length(repetitions) != length(positions)){
        stop("Not equal length resulted in calculations.")
      }
    }
    if(1679 %in% repetitions){
      break
    } else {
      repetitions <- c(repetitions, 1679)
      positions <- c(positions, 1440)
    }
    for(i in 1:length(names(act))){
      tempo$values[i] <- list(unname(unlist(tempogram[i, positions])))
    }
  } else {
    for(i in 1:length(names(act))){
      tempo$values[i] <- list(unname(unlist(tempogram[i, ])))
    }
  }

  end.time <- Sys.time()

  t = end.time - start.time

  print(t)

  return(tempo)

}
