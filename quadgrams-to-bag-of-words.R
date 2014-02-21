    #by Ben Marwick; posted originally at http://digitalhumanities.org/answers/topic/topic-modeling-mallet-with-jstor-data-for-research
    # set working directory, ie. location of JSTOR DfR CSV
    # files on the computer
    setwd("C:\\mallet-2.0.7\\jamt\\quadgrams\\")
     
    # create a list of all the CSV files
    myFiles <- list.files(pattern="*.csv|CSV")
     
    # read in all the CSV files to an R data object
    myData <-  lapply(myFiles, read.csv)
     
    # assign file names to each dataframe in the list
    names(myData) <- myFiles
     
    # Here's the step where we turn the JSTOR DfR 'wordcount' into
    # the 'bag of words' that's typically needed for topic modelling
    # The R process is 'untable-ing' each CSV file into a
    # list of data frames, one data frame per file.
    # Depending on whether you've downloaded bigrams, trigrams, etc, you will need to change the second line below accordingly.	

    myUntabledData <- sapply(1:length(myData),
      function(x) {rep(myData[[x]]$QUADGRAMS, times = myData[[x]]$WEIGHT)})
     
    # And here's the step where we create individual txt files
    # for each data frame (formerly a CSV file) that should be suitable for
    # input into MALLET.
    names(myUntabledData) <- myFiles
    sapply(myFiles,
      function (x) write.table(myUntabledData[x], file=paste(x, "txt", sep="."),
                              quote = FALSE, row.names = FALSE, eol = " " ))
     
    # Look in the working directory to find the txt files
