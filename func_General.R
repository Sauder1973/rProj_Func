# GENERAL FUNCTIONS --------------------------------------------------------------------------------------------------------------------------------------------------
# Contains Just the General Functions Used Potentially Global
#
# W Sauder
#
# Added a comment for GIT Commit

varFreq <- function(x){
     wrkng_ProdGroupFreq <- as.data.table(table(x))
     wrkng_ProdGroupFreq <- wrkng_ProdGroupFreq[rev(order(wrkng_ProdGroupFreq$N)),]
     colnames(wrkng_ProdGroupFreq) <- c("Feature", "Freq")
     totalVariables <- sum(wrkng_ProdGroupFreq$Freq)
     wrkng_ProdGroupFreq$Total_Percentage <- round(wrkng_ProdGroupFreq$Freq / totalVariables * 100, 2)
     return(wrkng_ProdGroupFreq)
}



# getTimeStamp  Code
getTimeStamp <- function(x) {
     t1        <- Sys.time()
     timeStamp <- as.character(t1)
     timeStamp <- gsub(" ","_",timeStamp)
     timeStamp <- gsub(":","_",timeStamp)
     return (timeStamp)
}

#Taken From https://github.com/gastonstat/R_Functions/blob/master/functions/clean.text.R
clean.text <- function(x, lowercase=TRUE, numbers=TRUE, punctuation=TRUE, spaces=TRUE)
{
     # x: character string
     
     # lower case
     if (lowercase)
          x = tolower(x)
     # remove numbers
     if (numbers)
          x = gsub("[[:digit:]]", "", x)
     # remove punctuation symbols
     if (punctuation)
          x = gsub("[[:punct:]]", "", x)
     # remove extra white spaces
     if (spaces) {
          x = gsub("[ \t]{2,}", " ", x)
          x = gsub("^\\s+|\\s+$", "", x)
     }
     # return
     x
}


substrRight <- function(x, n){
     substr(x, nchar(x)-n+1, nchar(x))
}

# Convert the Dates Function
myDateToNumeric <- function(x){
     dates <- strptime(x, "%Y-%m-%d") 
     dates <- as.character.POSIXt(dates)
     dates <- as.Date(dates)
     dates <- as.numeric(dates)
     
     return(dates)
}


# Code to Query SFDC
querySFDC <- function(x, filename) {
     sqlQueryLoaded <- readLines(paste(sqlQueryDir, "/", fileName, sep = ""), encoding = "UTF-8")
     sqlQueryLoaded <- iconv(sqlQueryLoaded, to = "ASCII//TRANSLIT")
     
     queryString2   <- gsub("[\r\n\t]", "", paste(sqlQueryLoaded, collapse = " "))
     # queryString2   <- gsub("<U+FEFF>", "", queryString2)
     # queryString2   <- gsub("i>?", "", queryString2)
     # queryString2   <- gsub("/?", "", queryString2)
     # queryString2 <- iconv(queryString2, to = "UTF-8")
     # queryString2   <- gsub("\0", "", queryString2)
     
     dbhandle       <- odbcDriverConnect(db_connectionString)
     sf_AllData     <- as.data.table(sqlQuery(dbhandle, queryString2, stringsAsFactors = FALSE))
     return (sf_AllData)
}

