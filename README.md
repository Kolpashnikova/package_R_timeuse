# Time Use Package for R.

Contains the following functions:
- tu_tempogram()
- tu_transitions()
- tu_totals()
- tu_maps()
- tu_path()

## Installation

```
devtools::install_github("Kolpashnikova/package_R_timeuse")

library(timeuse)
```

## tu_tempogram(): Time Use Tempogram in R

Creates a dataframe with two variables: "key" and "values." The "key" variable contains character values of the activity names. The "values" variables is a list for each activity, representing a timestamp (starting at 4am = 240) and the number of observations for this timestamp and activity.

### Usage
```
tu_tempogram(
  df,
  path_to_activity_codes = "data/ATUS_codes.csv",
  w = NULL,
  granularity = "full",
  method = "first"
)
```
### Arguments
*df*	
an ATUS-X dataframe constructed by ipumsr package.

*path_to_activity_codes*	
a path to an .csv file with the complete coding of activities. An example of the file can be found here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv

*w*	
the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."

*granularity*	
a number representing what gap in between timestamps to make (in minutes). For example, 15 means that observations will be at the 15-minutes interval: 4am, 4:15am, 4:30am etc. The default is "full," meaning that every minute is included into the tempogram.

*method*	
is the method used for defining the activity code for the granularized tempogram. "First" means to take the first activity code and "last" means to take the last activity code to represent the time interval.

## tu_transitions(): Time Use Transitions in R
Creates a list of two variables, "trate" and "activities." The "trate" variable contains a n x n matrix (where n is the number of coded activities). Rows represent the number of the activity from which the transition started, and columns represent activities into which the transition ended. Transitions of an activity into itself is always zero. The numbers in the matrix represent the percent of the total transitions (sum of matrix). The "activities" variable contains a character list of activities in the same order as they appear as the rows and columns in the trate matrix.

### Usage
```
tu_transitions(df, path_to_activity_codes = "data/ATUS_codes.csv", w = NULL)
```
### Arguments
*df*	
an ATUS-X dataframe constructed by ipumsr package.

*path_to_activity_codes*
a path to an .csv file with the complete coding of activities. An example of the file can be found here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv

*w*
the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."

## tu_totals(): Time Use Totals in R
Creates a list of two variables, "totals" and "activities." The "totals" variable represents a list of n (where n is the number of coded activities) of percent this activity takes up in the total of all activities across all observations. The "activities" variable represents a character list of coded activities in the same order as in they appear in the totals.

### Usage
```
tu_totals(df, path_to_activity_codes = "data/ATUS_codes.csv")
```
### Arguments
*df*	
an ATUS-X dataframe constructed by ipumsr package.

*path_to_activity_codes*
a path to an .csv file with the complete coding of activities. An example of the file can be found here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv

## tu_maps(): Time Use Totals for Specific Activity Across US States in R
Creates a dataframe with 51 variables, corresponding to the US states and DC (two-letter lowercase codes). Its values represent the mean for the activity of interest for a given state.

### Usage
```
tu_maps(df, activity, path_to_activity_codes = "data/ATUS_codes.csv", w = NULL)
```
### Arguments
*df*
an ATUS-X dataframe constructed by ipumsr package.

*activity*
activity of interest. The name should appear in the .csv file mentioned in the path_to_activity_codes.

*path_to_activity_codes*	
a path to an .csv file with the complete coding of activities. An example of the file can be found here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv

*w*
the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."

## tu_path(): Time Use Path Files in R
Create 3 files. Filename1 is for the diaries in the long format for each minute. Filename2 contains a JSON dictionary, where keys are activity names and values are the numbers they correspond to. Filename3 contains weights for all the observations in the same order as in Filename1.

### Usage
```
tu_path(
  df,
  path_to_activity_codes = "data/ATUS_codes.csv",
  filename1 = "ex_diaries.txt",
  filename2 = "activities.txt",
  filename3 = "weights.txt",
  w = NULL
)
```
### Arguments
*df*	
an ATUS-X dataframe constructed by ipumsr package.

*path_to_activity_codes*
a path to an .csv file with the complete coding of activities. An example of the file can be found here: https://github.com/Kolpashnikova/Kolpashnikova.github.io/blob/master/data/ATUS_codes.csv

*filename1*
the path and name of the file where to save the long format diaries.

*filename2*	
the path and name of the file where to save the activities dictionary.

*filename3*	
the path and name of the file where to save weights.

*w*
the name of the weights variables in the ATUS-X extract. The default is NULL. Usually, the weights variable is called "WT06."

## References
Kolpashnikova, Kamila. (2022). Time Use Package for R. Toronto,ON: York University.
