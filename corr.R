corr <- function(directory, threshold=0) {
    # directory - directory location of files
    # threshold - minimum complete cases within file
    # output:  numeric vector

    # Results vector
    res <- vector()
    # Files in target directory
    files <- list.files(directory)

    for (file in files) {
        cfile <- paste(directory, file, sep='/')
        df <- read.csv(cfile)
    
        # Number of valid records?
        good <- complete.cases(df)
        valid <- nrow(df[good, ])

        # Meets threshold?
        if (valid > threshold) {
            cres <- cor(df[good, "sulfate"], df[good, "nitrate"])
            # Only keep result if it's not NA
            # Note:  Get NA where there are no complete rows
            if (!is.na(cres)) {
                res <- c(res, cres)
            }
        }
    }

    # Return the results vector
    res

}

