pollutantmean <- function(directory, pollutant, id=1:332) {
    # directory - directory location of CSVs
    # pollutant - 'sulfate' | 'nitrate'
    # id - vector of monitor id numbers to identify CSV files
    # output:  mean of specified pollutant across files listed in id

    # Load all results here
    store <- NULL

    for (i in id) {
        # Get file id, convert to base filename (001 - 332) by padding 0s:
        cfile <- formatC(i, width=3, format="d", flag="0")
        # Convert to string and append csv file suffix:
        cfile <- paste(as.character(cfile), "csv", sep=".")
        # Now prepend directory name
        cfile <- paste(directory, cfile, sep="/")

        # Read in file
        df <- read.csv(cfile)
    
        store <- c(store, df[[pollutant]])
    }

    # Return the mean of all the data
    mean(store, na.rm=TRUE)
}

