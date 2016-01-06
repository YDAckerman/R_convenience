############################################################
## generally useful helper functions
############################################################

hf <- new.env()

## a quick and dirty multi-grepl function:
## wow, can easily do this with regex...?
hf$mgrepl <- function(patterns, x, ignore.case = FALSE,
                      strict = 0, fuzzy = FALSE, perl = FALSE){
    require(plyr)
    f <- if (fuzzy){ agrepl } else { grepl }
    bool_df <- ldply(patterns, function(pattern){
        f(pattern, x, ignore.case = ignore.case, perl = perl)
    })
    colSums(bool_df) > strict
}

# a quicker and dirtier multi-grep function:
hf$mgrep <- function(patterns, x, ignore.case = FALSE,
                     strict = 0, fuzzy = FALSE, value = FALSE, perl = FALSE){

    i <- hf$mgrepl(patterns, x, ignore.case = ignore.case,
              strict = strict, fuzzy = fuzzy, perl = perl)

    if(value){
        x[i]
    } else {
        seq_along(x)[i]        
    }
}

## trail leading and trailing whitespace:
hf$trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## find all the words two strings share (useful for finding
## the right row in the excel file using insect name):
hf$stringIntersect <- function(s1, s2){
    require(plyr)
    s <- strsplit(gsub("[^[:alnum:]]", " ", c(s1,s2)), " +")
    s <- llply(s, function(si){
        si[which(si != "")]
    })
    intersect(tolower(s[[1]]), tolower(s[[2]]))
}

## see if string2 contains all of string 1 (excluding non alphanum)
hf$stringContains <- function(s1, s2){
    s <- strsplit(gsub("[^[:alnum:]]", " ", c(s1,s2)), " ")
    all(tolower(s[[1]]) %in% tolower(s[[2]]))
}

hf$removeParens <- function(s){
    gsub("(?=\\().*?(?<=\\))", "", s, perl=T)
}

hf$aIn <- function(s1, s2, ic = TRUE, md = 0.1){

    if (all(is.na(s1) & all(is.na(s2)))){ return(TRUE) }
    
    s1 <- na.omit(s1)
    s2 <- na.omit(s2)
    sapply(s1, function(x){
        any(agrepl(x, s2, ignore.case = ic, max.distance = md))
    })
}

hf$RandDF <- function(rows, cols){
    stopifnot(cols <= 26)
    data <- sample(1:(rows * cols), rows * cols, replace = TRUE)
    tmp <- data.frame(matrix(data, nrow = rows, ncol = cols))
    colnames(tmp) <- letters[1:cols]
    tmp
}

## segment a vector into bins
hf$SegmentVec <- function(vec, num.segs, ordered = TRUE){

    ##require(plyr)
    if(ordered){
        vec <- sort(vec, na.last = FALSE)
    }

    segs <- sort(rep(1:num.segs,length = length(vec)))
    segments <- llply(1:num.segs,function(i){ vec[which(segs == i)] })
    return(segments)
}

## insert new values into specified indices in a
## vector
hf$insert.vals <- function(vector, values, indices){
    if(length(values) != length(indices)){
        values <- rep(values, length(indices))
    }
    indices <- sort(indices)
    vector <- c(
        vector[1:(indices[1] - 1)],
        values[1],
        vector[indices[1]:length(vector)]
    )
    if(length(indices) != 1){
        indices <- indices[2:length(indices)]
        values <- values[2:length(values)]
        vector <- insert.vals(vector, values, indices)
    }
    vector
}

hf$convertMonthNamesToNumeric <- function(mo_name){
   
    i <- 0
    for (month in months){
        i <- i + 1
        if(grepl(month, mo_name, ignore.case = TRUE)){
            return(i)
        }
    }
    return(NA)
}

hf$mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

hf$fixDates <- function(date, format = "%d/%m/%Y"){
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    as.Date(gsub("/0", "/200",
                 hf$mgsub(c(months, "-", " ", "208", "some subset of:", "dates not specified"),
                          c(1:12, "/", "/", "2008", "", ""),
                          hf$trim(hf$removeParens(date)))
                 ),
            format = format)
}

hf$symdiff <- function(A, B){
    c(setdiff(A, B), setdiff(B, A))
}

############################################################
############################################################
