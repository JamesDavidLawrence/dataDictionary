\name{dataDictionary-class}
\Rdversion{1.1}
\docType{class}
\alias{dataDictionary-class}
\alias{dataDictionary}
\alias{print,dataDictionary-method}
\alias{show,dataDictionary-method}

\title{Class \code{"dataDictionary"}}
\description{
Class is used for storing data classes and levels of the columns of a data file.  Class objects are created by the RSAI function \code{extractDataDictionary} and applied by \code{applyDataDictionary}.
}
\section{Objects from the Class}{
Objects can in principle be created from scratch but this is not recommmended. The normal way to create them is via \code{\link{extractDataDictionary}.}
}
\section{Slots}{
  \describe{
    \item{\code{colNames}:}{Object of class "character" --- The column names of the dataset in question.}
    \item{\code{colClasses}:}{Object of class "character" --- The column classes of each column. Must be the same length as code{@colNames}, and each element must be one of \code{"int","int64","logical","complex","numeric","character","factor","ordered"}.}
    \item{\code{levels}:}{Object of class "list" --- A named list of factor levels. The named elements must correspond with the elements of \code{@colNames} for which \code{@colClasses} is either "factor" or "ordered".}
    \item{\code{NALevels}:}{Object of class "list" --- A named list of values to be coerced to NA. The names must be elements of \code{@colNames} and the values must be comparable with the corresponding class in \code{@colClasses} (but need not be exactly the same class, for instance it is fine to have a \code{NALevels} entry of -999 (numeric) when the column class is "integer". }
  }
}
\section{Methods}{
  \describe{
    \item{+}{\code{signature(e1 = "dataDictionary", e2 = "dataDictionary")}: Adds data dictionaries in an intuitive way (see the example in \code{extractDataDictionary}.) }
    \item{print}{\code{signature(x = "dataDictionary", maxLevels = 10)}: prints the dataDictionary object. if a factor has more levels than maxLevels then print will just return the number of levels rather than a list of them. }
    \item{show}{\code{signature(object = "dataDictionary")}: ... 
  }
}}

\author{
Tom Bratcher & James Lawrence
}

\note{
dataDictionary is an S4 class of object - so to examine or edit it in detail you need to access its slots. The R syntax for this uses "@" - for example if DD is your dataDictionary, its levels are DD@levels.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{extractDataDictionary}} and its methods, which creates objects of this class.
}
\examples{
showClass("dataDictionary")
}
\keyword{classes}
\keyword{extractDataDictionary}
\keyword{applyDataDictionary}
\usage{
dataDictionary(...)
}
\arguments{
  \item{\dots}{
    name-value pairs to assign into the slots of the resultant \code{dataDictionary} object.
  }
}
