setClass("dataDictionary",representation(
  colNames="character",
  colClasses="character",
  levels="list", ## named,names=colNames where colClasses="factor"
  NALevels = "list"
),validity=function(object){ l<- sapply(object@levels, length)
if(!all(names(object@levels)[which(l>1)] %in% object@colNames[object@colClasses=="factor"|object@colClasses =="ordered" ])){
  return("names of object@levels do not match those elements with class 'factor' or 'ordered'")}
if(!all(object@colNames[object@colClasses=="factor"| object@colClasses =="ordered"] %in% names(object@levels))){
  return("names of object@levels don't match those elements with class 'factor' or 'ordered'")}
#if(any(!(object@colClasses %in% c("logical","integer","int64","numeric","complex","character","factor","ordered")))) return("unsupported column classes")
}) -> dataDictionary

setAs("ANY","character",function(from) as.character(from))
setAs("ANY","numeric",function(from) as.numeric(from))
setAs("ANY","integer",function(from) as.integer(from))
setAs("ANY","factor",function(from) as.factor(from))
setAs("ANY","logical",function(from) as.logical(from))
setAs("ANY","ordered",function(from) as.ordered(from))


maxClass <- function (foo,bar) {
  
  x<- length (foo)  
  y<- length (bar)
  if (!(x==y)) stop ("length mismatch")
  z<- 0
  numberWang <- c ("logical","integer","int64","numeric","complex","character","factor","ordered")
  
  
  for (i in 1:x ) {
    
    num1 <- match (as.character (foo[i]), numberWang)  
    num2 <- match (as.character (bar[i]), numberWang) 
    
    if (is.na(num1) || is.na (num2)) stop ("invalid class")
    foo[i] <- numberWang[max(num1,num2)]
    
    if (num1<num2) cat("coerced ",names(foo)[i], " to class ",as.character(numberWang[num2]),"\n")
  }
  
  return (foo)
}



mergeDic <- function (data1,data2,
                      classList =as.character(),warnUnusedColumns=FALSE,
                      convMessage=TRUE,coercedCols = list()){
  
  numberWang <- c ("logical","integer","int64","numeric","complex")  
  
  for(i in seq_along(colnames(data1))){
    nm <- colnames(data1)[i]
    if(is.null(data2[[nm]])){
      if(warnUnusedColumns) warning(gettextf("column %s not present",nm))
      next
    }
    cl1 <-class(data1[[nm]])
    cl1 <-cl1 [length(cl1)]
    cl2 <- class(data2[[nm]])
    cl2 <-cl2 [length(cl2)]
    
    cl1to <- cl1
    if (nm %in% names (classList)) cl1to <- classList[[nm]]
    
    
    if( cl2 %in% c("factor","ordered")) {
      toFactorFlag <- TRUE
      
      if (!(cl1to %in% c("factor","ordered"))) toFactorFlag <-FALSE
      
      
      if (!(cl1 %in% c("factor","ordered")) && convMessage && toFactorFlag )   {print (gettextf("converting column %s from %s to factor",nm,cl1))
        attr(data1,"Coerced") <- "Yes"
        coercedCols <-c (coercedCols, nm)}
      
      if (cl1to == "factor"&& cl1 != "factor")  {data1[[nm]] <- factor(data1[[nm]])}
      if (cl1to == "ordered"&& cl1 != "ordered")  {data1[[nm]] <- ordered(data1[[nm]])} 
      if (toFactorFlag || convMessage) { data1[[nm]] <- factor(as.character(data1[[nm]]),
                                                               levels=sort(unique(c(levels(data1[[nm]]),levels(data2[[nm]])))),
                                                               sort(unique(c(levels(data1[[nm]]),levels(data2[[nm]])))))} 
      
      
      if (!(nm %in% names (classList)) && !(convMessage) && cl1 != "factor" ) {
        warning (c(nm, " not converted to factor from ",data2))
        data1[[nm]] <- as.character(data1 [[nm]])}
      
      next
    }
    
    if(cl1 %in% c("factor","ordered")) {
      d2f <- factor(data2[[nm]])
      data1[[nm]] <- factor(as.character(data1[[nm]]),
                            levels=sort(unique(c(levels(data1[[nm]]),levels(d2f)))),sort(unique(c(levels(data1[[nm]]),levels(d2f)))))
      if (nm %in% names (classList)) class (data1[[nm]]) <- classList[[nm]]
      next
    }
    
    ch1 <- (cl1 == "character")
    ch2 <- (cl2 == "character")
    if(ch1 + ch2 ==1 &&  ! (nm %in% names (classList)) ) {
      if (convMessage)   
      {print (gettextf("converted column %s to factor",nm))
        coercedCols <-c (coercedCols, nm)}   
      if (convMessage || cl1to == "factor") {
        
        data1[[nm]] <- factor(data1[[nm]])      
        d2f <- factor(data2[[nm]])
        data1[[nm]] <- factor(as.character(data1[[nm]]),
                              levels=sort(unique(c(levels(data1[[nm]]),levels(d2f)))),sort(unique(c(levels(data1[[nm]]),levels(d2f)))))}
      
      else { data1[[nm]] = as.character( data1 [[nm]])  
      warning (c("Column ",nm,
                 " coerced to character due to type mismatch. If this is not desired, you can add the column name into colClasses"))
      coercedCols <-c (coercedCols, nm)
      next}
      
      num1 <- match (cl1, numberWang)  
      num2 <- match (cl2, numberWang)    
      if (!(is.na(num1)) && !(is.na (num2))) 
      {class (data1[[nm]]) <- numberWang[max(num1,num2)]
      if (num1<num2) cat ("coerced ",names(foo)[i], " to class ",as.character(numberWang[num2]))
      }
    }
  }
  return (list (data1, coercedCols))
  
}


extractDataDictionary <- function (obj, stringsAsFactors, colClasses =NULL,
                                   chunkSize =10000L,listFlag=FALSE,rowCount = FALSE,
                                   justClasses = FALSE,maxFactor=255, # the argument of makeup artists
                                   NALevels = list(), checkNames= FALSE,...) {UseMethod("extractDataDictionary")}


setGeneric("extractDataDictionary", signature = "obj")

extractDataDictionary.default <- function(obj,...) {
  print ("No default method defined for this function. Type '?extractDataDictionary' for details")}

extractDataDictionary.data.frame <- function(obj,stringsAsFactors=FALSE, colClasses = NULL, maxFactor = 255, NALevels = list(), ...){
  
  
  validClasses <- c ("logical", "int64","integer","numeric","complex","character","factor", "ordered")
  
  for (i in names(colClasses)) {
    if (!(colClasses [[i]] %in% validClasses)) stop ("unsupported class name in colClasses") 
  } 
  if (length(colClasses) >0  && length(names(colClasses))==0) warning ("no names attached to colClasses")
  
  colNames <- colnames(obj)
  colClasses2 <- character(length(colNames))
  levels <- list()
  NALevels2 <-list()
  for(i in seq_along(colNames)){
    
    nm <- colNames[i]
    NALevels2[[nm]]<-NULL
    if (!is.null(t1 <- NALevels[[nm]])) t1 -> NALevels2 [[nm]]
    if (i %in% names(colClasses)) cClass <- colClasses[[i]]
    # sAF used to turn into factors
    
    if(is(obj[[i]],"int64")) colClasses2[i] <- "int64"
    else if(is.logical(obj[[i]])) colClasses2[i] <- "logical"
    else if(is.integer(obj[[i]])) colClasses2[i] <- "integer"
    else if(is.complex(obj[[i]])) colClasses2[i] <- "complex"
    else if(is.numeric(obj[[i]])) colClasses2[i] <- "numeric"
    
    else if(is.factor(obj[[i]]) || stringsAsFactors ==TRUE) colClasses2[i] <- "factor" 
    
    else colClasses2[i] <- "character"
    
    if (nm %in% names(colClasses)) colClasses2[i] <- colClasses[[nm]]
    if(colClasses2[i] %in% c( "factor", "ordered")){
      if(is.factor(obj[[i]])) levels[[nm]] <- levels(obj[[i]]) # is.factor includes ordered objects
      else levels[[nm]] <- sort(unique(obj[[i]]))
      
      levels [[nm]] <- setdiff(levels[[nm]], NALevels[[nm]])   
      tempClass <- "character"
      if (nm %in% names(colClasses)) tempClass <- colClasses[[nm]] 
      if ( Hmisc::all.is.numeric(levels [[colNames[i]]], what = "test", extras = NALevels[[nm]]) && ! (tempClass %in% c( "factor", "ordered")) ) {  
        numFlag <- 1
        for (j in levels [[colNames[i]]]) if (substring (j,1,1)=="0" && !(j=="0") && !(substring(j,1,2)=="0.") ) numFlag <-0
        if (numFlag) {
          colClasses2[i]<- "numeric"
          levels [[colNames[i]]] <- as.character("")
        }
      } 
      
    }
    else levels [[colNames[i]]] <- as.character("")
    
    
    if (length (levels [[colNames[i]]] ) >maxFactor) {
      colClasses2[i] <- "character"
      levels [[colNames[i]]] <- as.character("")
    }
    
  }
  dataDictionary(colNames=colNames,colClasses=colClasses2,levels=levels,NALevels=NALevels2)
}

extractDataDictionary.character <- function (obj,stringsAsFactors=FALSE, colClasses = NULL,
                                             chunkSize = 10000L,listFlag =FALSE,rowCount = FALSE,justClasses = FALSE,maxFactor =255,NALevels = NULL,checkNames,...){
  validClasses <- c ("logical","int64","integer","numeric","complex","character","factor","ordered")
  
  for (i in names(colClasses)) {
    if (!(colClasses [[i]] %in% validClasses)) stop ("unsupported class name in colClasses") }
  
  
  chunk <- read.csv(obj,stringsAsFactors= stringsAsFactors,nrows=chunkSize, header = TRUE,check.names=TRUE,...)
  tempClasses <- sapply (chunk, class)
  
  obj2<-obj
  obj<- file(obj,"rt")
  on.exit(close(obj))
  coercedCols <- list ()
  dataDic <-0
  loop <-0
  wng <- options (warn = -1)
  on.exit(options(wng),add=TRUE)
  chunk <- read.csv(obj,stringsAsFactors= FALSE,nrow=chunkSize,
                    header = TRUE, colClasses = rep ("character",length(tempClasses)),check.names=TRUE,...)
  colnames(chunk)<-names (tempClasses)   
  
  options (wng)
  for(nm in names (colClasses)) { if (nm %in% colnames(chunk))   {
    
    
    if (colClasses[[nm]] == "factor") chunk[[nm]] <- factor (chunk[[nm]])  
    else class (chunk[[nm]]) <- colClasses[[nm]]
  }
    else warning (c("column ", nm, " specified in colClasses but not present in data"))}
  
  for ( nm in colnames(chunk)) { 
    
    if (!( nm %in% names (colClasses))) 
      
    {if (tempClasses [[nm]] =="factor") chunk [[nm]] = factor (chunk[[nm]]) else class (chunk [[nm]]) <- tempClasses [[nm]] }    
    
  }
  nlines <- 0
  classList <- lapply (chunk,class)
  
  while (!inherits(chunk,"try-error")) {
    colnames(chunk)<-names (tempClasses)
    if (loop ==0) chunkone <- chunk 
    else {
      colnames (chunk) <- colnames (chunkone)# names but _not_ classes!
      if (!(justClasses)) {
        chunkone <- mergeDic (chunkone,chunk,classList = colClasses, FALSE,TRUE,
                              coercedCols = coercedCols)
        coercedCols <- chunkone [[2]]
        chunkone <- as.data.frame ( chunkone [[1]])
        
      }
      
      else classList <- maxClass (classList,lapply (chunk,class))
    }
    loop <- 1+ loop  
    nlines <- nlines + nrow(chunk)
    
    chunk <- try(read.csv(obj,stringsAsFactors=stringsAsFactors,nrow=chunkSize,header = FALSE,check.names=TRUE,...),silent =TRUE)
    
    
    
    
    
    cat (c ("processed chunk",loop,"\n"))
  }
  close(obj)
  rm(obj)
  on.exit(NULL)
  
  if (loop >0 & !(justClasses)) dataDic <- extractDataDictionary(chunkone,stringsAsFactors=stringsAsFactors, colClasses= colClasses,maxFactor = maxFactor, NALevels = NALevels,...)  
  
  # and finally....
  if (!(checkNames) & loop >0 & ! (justClasses)  )
  {
    chunk <- read.csv(obj2,stringsAsFactors= FALSE ,nrow= 1,
                      header = TRUE, colClasses = rep ("character",length(tempClasses)),check.names=FALSE,...) 
    naIndex = match (names(dataDic@NALevels), dataDic@colNames )
    naIndex <- naIndex [!is.na (naIndex)]
    dataDic@colNames<- colnames(chunk) # because they had better always exist
    
    names (dataDic@levels) <- colnames(chunk) # because they always exist
    for (i in seq_along( naIndex)) names (dataDic@NALevels)[i]<- colnames(chunk)[naIndex[i]]   }# ugly, but tested
  
  
  if (rowCount) classList <- list (classList,nlines)
  if (justClasses) return (classList)
  if (!is.null(attributes(chunkone)[["Coerced"]])) warning ("One or more fields were initially read in as numbers but subsequently coerced to factors. These factor levels may be incomplete. You can avoid this problem by specifying the desired class explicitly in colClasses")
  if (rowCount) dataDic <- list (dataDic,nlines)
  if (listFlag) return(list(dataDic,coercedCols)) else return (dataDic)
}

setMethod("extractDataDictionary",signature = signature (obj = "data.frame"), extractDataDictionary.data.frame)
setMethod("extractDataDictionary",signature = signature (obj = "character"), extractDataDictionary.character)


addDataDictionary <- function(e1,e2){
  z <- e1
  for(i in seq_along(e2@colNames)){
    if(e2@colNames[i] %in% z@colNames){
      i1 <- match(e2@colNames[i],e1@colNames)
      if(e2@colClasses[i] != e1@colClasses[i]) warning(gettextf("column type mismatch in column %s; LHS has %s, RHS has %s",e2@colNames[i],e1@colClasses[i1],e2@colClasses[i]))
      if(e2@colClasses[i] == "factor" && e1@colClasses[i1] == "factor" && !identical(e1@levels[[e1@colNames[i1]]],e2@levels[[e2@colNames[i]]])){
        warning(gettextf("factor level mismatch in column %s, expanding and/or reordering dictionary to include all levels in both sides",e1@colNames[i1]))
        z@levels[[e2@colNames[i]]] <- sort(unique(c(e1@levels[[e1@colNames[i1]]],e2@levels[[e2@colNames[i]]])))
      }
      if(!is.null(e1@NALevels[[e1@colNames[i1]]]) || !is.null(e2@NALevels[[e2@colNames[i]]])) z@NALevels[[e2@colNames[i]]] <- sort(unique(c(e1@NALevels[[e1@colNames[i1]]],e2@NALevels[[e2@colNames[i]]])))
    }
    else {
      z@colNames <- c(z@colNames,e2@colNames[i])
      z@colClasses <- c(z@colClasses,e2@colClasses[i])
      if(!is.null(e2@levels[[e2@colNames[i]]])) {
        z@levels[[e2@colNames[i]]] <- e2@levels[[e2@colNames[i]]]
      }
      if(!is.null(e2@NALevels[[e2@colNames[i]]])){
        z@NALevels[[e2@colNames[i]]] <- e2@NALevels[[e2@colNames[i]]]
      }
    }
  }
  z
}
setMethod("+",signature(e1="dataDictionary",e2="dataDictionary"),addDataDictionary)

applyDataDictionary <- function(x,data,warnUnusedColumns=TRUE){
  for(i in seq_along(x@colNames)){
    
    nm <- x@colNames[i]
    cl <- x@colClasses[i]
    if(is.null(data[[nm]])){
      
      
      if(warnUnusedColumns) warning(gettextf("column %s not present",nm))
      next
    }
    if(!(cl %in% c("factor","ordered"))) {data[[nm]] <- as(data[[nm]],cl)}
    else if (cl=="factor")  {data[[nm]] <- factor(as.character(data[[nm]],levels=x@levels[[nm]]))}
    else data[[nm]] <- ordered(as.character(data[[nm]],levels=x@levels[[nm]]))
    
    data[[nm]] [as.character (data[[nm]]) %in% x@NALevels[[nm]]] <- NA  
  }
  data
}

print.dataDictionary <- function(x,maxLevels=10,...){
  cat("column Name     | column Class| levels \n")
  for(i in seq_along(x@colNames)){
    nm <- x@colNames[i]
    cl <- x@colClasses[i]
    cat(strtrim(paste0(nm,"             "),16)," ",strtrim(paste0(cl,"              "),13))
    
    if(!is.null(t1 <- x@NALevels[[nm]])){cat("(NA =",t1,")")}
    
    if(cl=="factor" || cl == "ordered"){
      if((fl <- length(x@levels[[nm]])) > maxLevels) cat(fl,"levels\n") else cat(x@levels[[nm]],"\n")
    } else cat("\n")
  }
  if(!length(i)) cat("(no columns)\n")
}
show.dataDictionary <- function(object){
  print.dataDictionary(object)
}

setMethod("print","dataDictionary",print.dataDictionary)
setMethod("show",signature(object="dataDictionary"),show.dataDictionary)
