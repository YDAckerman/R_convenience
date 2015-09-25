############################################################
## generally useful helper functions
############################################################

hf <- new.env()

## a quick and dirty multi-grepl function:
## wow, can easily do this with regex...?
hf$mgrepl <- function(patterns, x, ignore.case = FALSE,
                      strict = 0, fuzzy = FALSE){
    require(plyr)
    f <- if (fuzzy){ agrepl } else { grepl }
    bool_df <- ldply(patterns, function(pattern){
        f(pattern, x, ignore.case = ignore.case)
    })
    colSums(bool_df) > strict
}

# a quicker and dirtier multi-grep function:
hf$mgrep <- function(patterns, x, ignore.case = FALSE,
                     strict = 0, fuzzy = FALSE){

    i <- hf$mgrepl(patterns, x, ignore.case = ignore.case,
              strict = strict, fuzzy = fuzzy)
    
    seq_along(x)[i]
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


############################################################
############################################################
