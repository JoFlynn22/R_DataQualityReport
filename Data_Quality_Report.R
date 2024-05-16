
library(caret)
library(dplyr)
library(mice)
library(knitr)
library(tidyr)
library(ggplot2)
library(Hmisc)

train = read.csv("train.csv")

###############################################################################
#                         Numeric Data                                        #

# selecting only the numeric data
custNum <- tibble(dplyr::select(train, where(is.numeric)))

# getting the non numeric data
preCustFactor <- tibble(dplyr::select(train, Negate(is.numeric)))
custFactor <- as_tibble(unclass(preCustFactor), stringsAsFactors = TRUE, na.strings = c("", " ", "NA"))

# taking a look at the data
glimpse(custNum)
glimpse(custFactor)

# getting the quantile functions
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}

Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

# getting the numeric summary
myNumericSummary <- function(x){
  c(length(x),  n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

# passing everything through the function
numericSummary <- custNum %>% dplyr::summarise(across(
  .cols=everything(), .fns=myNumericSummary))


glimpse(numericSummary)

# combining everything
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)

glimpse(numericSummary)

# final summary
numericSummaryFinal <- numericSummary %>%
  pivot_longer("sessionId":"revenue", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = (100*as.numeric(missing))/n,
         unique_pct = (100*unique)/n) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())

options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()

################################################################################
#                               Non-Numeric Data                               #


# making two functions to identify common nodes and their frequencies
getmodes <- function(v,type=1) {      # Identifies which node is common or not
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1))      #1st mode
  }
  else if (type==2) {     
    return (names(which.max(tbl[-m1])))  #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl)))      # least common node
  }
  else {
    stop("Invalid type selected")
  }
}

getmodesCnt <- function(v,type=1) {     # frequencies of nodes
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl))        #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1]))     #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl))        #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

# frequency ratio function
freqRatio <- function(v){
  getmodesCnt(v, type = 1)/getmodesCnt(v, type = 2)
}

# making another function to obtain numerous parameters for the housing factors
#myFactorSummary <- function(v) {
#  c(length(v), sum(is.na(v)), n_distinct(v), freqRatio(v), getmodes(v, type = 1), 
#    getmodesCnt(v, type = 1), getmodes(v, type = 2), getmodesCnt(v, type = 2),
#    getmodes(v, type = -1), getmodesCnt(v, type = -1))
#}

myFactorSummary <- function(v) {
  c(length(v), sum(is.na(v)), n_distinct(v))
}

# piping the data through the functions
custFactorReplace <- custFactor
custFactorReplace[custFactorReplace == "" | custFactorReplace == " "] <- NA
factorSummary <- custFactorReplace %>% dplyr::summarise(across(.cols = everything(),
                                                               .fns = myFactorSummary))

glimpse(factorSummary)

# adding another column to the df and adding it to myFactorSummary
#tempFactorSummary <- cbind(stat = c("n", "missing", "unique", "freqRatio", "1st mode", 
#                                    "1st mode freq", "2nd mode", "2nd mode freq",
#                                    "least common", "least common freq"), factorSummary)

tempFactorSummary <- cbind(stat = c("n", "missing", "unique"), factorSummary)

glimpse(tempFactorSummary)


factorSummaryFinal <- tempFactorSummary %>%
  pivot_longer("date":"adwordsClickInfo.isVideoAd", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

factorSummaryFinal <- factorSummaryFinal %>% mutate(across(c(missing, n, unique), 
                                                           .fns = as.numeric))
factorSummaryFinal <- factorSummaryFinal %>% mutate(missing_pct = (100*missing/n), 
                                                    unique_pct = (100*unique)/n) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())

options(digits=3)
options(scipen=99)
factorSummaryFinal %>% kable()
