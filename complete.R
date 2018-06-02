complete <- function(directory, id=1:332) {
    # directory - directory location of files
    # id - vector of monitor id numbers mapping to files
    # output:  data frame, col1 = id, col2 = number of complete cases
    
    # complete cases results vector
    compcases <- NULL

    for (i in id) {
        # Get file id, convert to base filename (001 - 332) by padding 0s:
        cfile <- formatC(i, width=3, format="d", flag="0")
        # Convert to string and append csv file suffix:
        cfile <- paste(as.character(cfile), "csv", sep=".")
        # Now prepend directory name
        cfile <- paste(directory, cfile, sep="/")

        # Read in file
        df <- read.csv(cfile)
    
        # Number of valid records?
        good <- complete.cases(df)
        valid <- nrow(df[good, ])

        # Store results
        compcases <- c(compcases, valid)
    }

    # Return the new data frame
    data.frame(ID=id, NObs=compcases)
}

