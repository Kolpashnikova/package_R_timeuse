% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tu_tempogram.R
\name{tu_tempogram}
\alias{tu_tempogram}
\title{Time Use Tempogram in R}
\usage{
tu_tempogram(
  df,
  path_to_activity_codes = "data/ATUS_codes.csv",
  w = NULL,
  granularity = "full",
  method = "first"
)
}
\arguments{
\item{df}{an ATUS-X dataframe constructed by ipumsr package.}

\item{path_to_activity_codes}{a path to an .csv file with the complete coding of activities. An example of the file can be found
here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv}

\item{w}{the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."}

\item{granularity}{a number representing what gap in between timestamps to make (in minutes). For example, 15 means that
observations will be at the 15-minutes interval: 4am, 4:15am, 4:30am etc. The default is "full," meaning that every minute is
included into the tempogram.}

\item{method}{is the method used for defining the activity code for the granularized tempogram. "First" means to take the first
activity code and "last" means to take the last activity code to represent the time interval.}
}
\description{
Creates a dataframe with two variables: "key" and "values." The "key" variable contains character values of the activity names.
The "values" variables is a list for each activity, representing a timestamp (starting at 4am = 240) and the number of observations
for this timestamp and activity.
}
\references{
Kolpashnikova, Kamila. (2022). Time Use Package for R. Toronto,ON: York University.
}
