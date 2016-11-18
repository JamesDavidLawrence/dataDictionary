\name{extractDataDictionary}
\alias{extractDataDictionary}
\alias{applyDataDictionary}
\alias{addDataDictionary}
\alias{+,dataDictionary,dataDictionary-method}
\alias{extractDataDictionary,character-method}
\alias{extractDataDictionary,data.frame-method}
\title{
Functions to extract data types and factor levels from a data frame or file and apply them.
}
\description{
extractDataDictionary extracts data types and factor levels and produces a "Data Dictionary" object. This can then be viewed, edited if desired, and applied to another file using the applyDataDictionary function. This enables users to automatically produce files with consistent file formatting and in particular consistent factor levels, which is vital for processing using routines that make use of sparse matrices such as xgboost. 

extractDataDictionary can also be used to just return data types.

Data Dictionaries can also be passed as an argument to the RSAI function downSample; this acts to apply a data dictionary to a file while downsampling it. (This will usually be the data dictionary of the original file.) 
}
\usage{
extractDataDictionary(obj, stringsAsFactors, colClasses = NULL, chunkSize =
                 10000L, listFlag = FALSE, rowCount = FALSE,
                 justClasses = FALSE, maxFactor = 255, NALevels =
                 list(), checkNames = FALSE, ...)
applyDataDictionary(x,data,warnUnusedColumns=TRUE)
addDataDictionary(e1,e2)
e1 + e2
\S4method{extractDataDictionary}{character}(obj, stringsAsFactors, colClasses = NULL, chunkSize =
                 10000L, listFlag = FALSE, rowCount = FALSE,
                 justClasses = FALSE, maxFactor = 255, NALevels =
                 list(), checkNames = FALSE, ...)
\S4method{extractDataDictionary}{data.frame}(obj, stringsAsFactors, colClasses = NULL, chunkSize =
                 10000L, listFlag = FALSE, rowCount = FALSE,
                 justClasses = FALSE, maxFactor = 255, NALevels =
                 list(), checkNames = FALSE, ...)
\S4method{"+"}{dataDictionary,dataDictionary}(e1,e2)
}
\arguments{
  \item{obj}{
The file or \code{.csv} filename you wish to produce a data dictionary from.  This can be a \code{\link{data.frame}} or a file in the current working directory, but not a \code{\link{connection}} (see note below).
}
  \item{stringsAsFactors}{
 A logical. If \code{TRUE}, then by default, columns containing \code{character} data (or what \code{\link{read.csv}} interprets as character data when reading them in based on the start of the file) are read in as factors. If false, they are read in as characters.
}
  \item{colClasses}{
A named list of column classes. extractDataDictionary will apply these (and this overrides stringsAsFactors). For example, this might be set to prevent policy number being read in as a factor, or to coerce a (mostly numeric) field to factor. See usage notes below.
}
  \item{chunkSize}{
Where 'obj' is a filename, this is the number of columns of the file which are read in at a time. Larger chunkSize is likely to result in a slightly longer runtime, however the risk of coercion of columns is reduced.  
}
  \item{listFlag}{
Where 'obj' is a filename, option to return a list of the coerced column names in addition to the data dictionary.  
}
  \item{rowCount}{
Where 'obj' is a filename, option to return the number of rows in the dataset in addition to the data dictionary.  This is useful for subsequent application in the downSample function.
}
  \item{justClasses}{
Where 'obj' is a filename, option to return just the classes that the file would be read in as rather than including all of the factor levels.  Setting this option to TRUE also prints out any column coercion.

Using this option makes the function run more quickly and can be useful for preliminary investigations. See usage notes below.
}
  \item{maxFactor}{
Maximum number of levels a read in factor can have. If reading in a factor causes its levels to exceed maxFactor, extractDataDictionary stores the column as being class "character".

This results in a smaller (and more sensible) dictionary file, however note that lowering maxFactor doesn't materially affect runtime. it's better to specify a column which you know is a character field as "character" in colClasses.
}
\item{NALevels}{
A named list of character strings which are to be treated as NAs. If extractDataDictionary interprets a column as a factor, it will exclude these from the derived list of factor levels and having done so, and if it is sensible to do so, can interpret the column as numeric. 
NALevels do not need to solely be specified for columns containing factor data; You can also use it to coerce strings like "unknown" or "99999" to NA by using applyDataDictionary. 
}
\item{checkNames}{
if TRUE, extractDataDictionary's list of column names will be coerced to syntactically valid variable names as per check.names in read.csv.  See usage notes below.
}
\item{\dots}{
Other arguments to \code{\link{read.csv}}. See usage notes below.
}
 \item{x}{
In \code{extractDataDictionary}, the data dictionary to apply.
}
 \item{data}{
The dataset to which it's applied. Column classes are reset, items in NALevels are set to NA, and factor levels are set to the levels in the data dictionary.
}
 \item{warnUnusedColumns}{
if TRUE, warns if columns exist in the data dictionary but not in the data.
}
\item{e1}{
A dataDictionary object.
}
\item{e2}{
A dataDictionary object.
}
}

\details{
Data dictionaries can also be added together (essentially, creating a union of their fields and factor levels).  
}

\value{
Returns an object of (S4) class \code{"dataDictionary"}, and (optionally) a list of fields coerced to factor during the data processing and/or the number of columns in the original file.
}

\author{
Tom Bratcher & James Lawrence
}
\section{Notes}{
1. While \code{extractDataDictionary} is designed to run on large files where it is not desirable (or possible) to read in all of the data into memory at once, it will not infallibly produce the data dictionary you would want in some circumstances unless \code{colClasses} are specified.  (This is why a list of coerced columns is produced.)

Potential problems arise where the data in a column are almost all numeric but also contains rare character observations. If not specified, extractDataDictonary may read the initial chunk of the file in and interpret the column as numeric (as would read.csv). If it subsequently coerces this column as a factor, any factor levels which do not occur in the data after the coercion occurs will be missed.  
In practice this is not a huge problem as long as a consistent data dictionary is ultimately used, however you can avoid it by running the function with \code{justClasses = TRUE}, and then rerunning the function with the class of all coerced fields specified. This can also highlight fields which are unexpectedly being coerced to factor.  

What you _can't_ do is specify a column to a class within \code{colClasses} where \code{data} would not be correctly read in to that class from the datafile (eg a column containing 1,2,3,4 and "unknown" can't be specified as integer.) 

2. \code{extractDataDictionary} will always return a factor as unordered (ie "factor") by default - if you want an ordered factor this should be specified in colClasses. 

3. \code{extractDataDictionary} makes internal use of read.csv via connections.  Do not use it on a file you have already established an open connection to.  

4. \code{extractDataDictionary} will not impact the original data file. However it will read in column names without spaces and other unusual characteristics (as per check.names = TRUE in \code{\link{read.csv}}).  This means that, even if you want to set checkNames to false (and thus ultimately leave the column names as they are), any column names you specify in \code{colClasses} or \code{NALevels} should be similarly formatted. Also, do not attempt to pass \code{check.names} as a function argument here. 

5. DO NOT use extractDataDictionary on a dataset with missing column names (eg those produced by default by \code{\link{write.csv}}).
}

\examples{
df1 <- data.frame(x1=rnorm(5),x2=1:5,x3=letters[1:5],x4=LETTERS[2* 1:5])
df3 <- df2 <- df1
df1$x3 <- as.character(df1$x3)
extractDataDictionary(df2,FALSE) + extractDataDictionary(df1,FALSE)
extractDataDictionary(df1,TRUE) + extractDataDictionary(df2,TRUE)

df3$x4 <- as.factor (LETTERS [1:5])
df3a <- applyDataDictionary(extractDataDictionary(df1,TRUE),df3)
df3a
dd <- extractDataDictionary(df1,TRUE) + extractDataDictionary(df3,TRUE)
dd
df3b <- applyDataDictionary(dd,df3)
df3b
}