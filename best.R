#########################################################################
### hospital.R
### 2015/08/24 
############################################################################
bestInitialize <- function() {
  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)
#   setwd("C:/Users/p622403/Documents/Work/R")
  setwd("C:/Users/p622403/Documents/Work/GitRepos/SomeRandD")
  #   require("XML")
  #   install.packages("XML")  
}
### http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
### hmmmm 
leukemiaPlot <- function() {
  require(foreign)
  require(ggplot2)
  require(MASS)
  
  data(leuk, package="MASS")
  leuk.mod <- glm(time~ag-1+log10(wbc),family=Gamma(log),data=leuk)
  leuk.diag <- glm.diag(leuk.mod)
  glm.diag.plots(leuk.mod,leuk.diag)
}
############################################################################
###   readOutcomeData
############################################################################
readOutcomeData <- function() {
  ### All columns as character
  dfOutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  return(dfOutcome)
}
############################################################################
###   inspectOutcomeData
############################################################################
inspectOutcomeData <- function() {
  ### All columns as character
  dfOutcome <- readOutcomeData()
  # print(head(dfOutcome))
  print(nrow(dfOutcome))
  print(ncol(dfOutcome))
  # View(dfOutcome)
  # print(names(dfOutcome))
  dfOutcomeColNames <- as.data.frame(names(dfOutcome))
  View(dfOutcomeColNames)
  ### Cast column 11 to be numeric
  dfOutcome[, 11] <- as.numeric(dfOutcome[, 11])
  ### Column 11 colname is "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ### Plot a Histogram from column 11
  hist(dfOutcome[, 11])
}
############################################################################
###   getUniqueStates
############################################################################
getUniqueStates  <- function() {
  dfOutcome <- readOutcomeData()
  vStates <- as.vector(dfOutcome$State)
  vStatesU <- unique(vStates)
  #   print("*** vStatesU ***")
  #   print(length(vStatesU))
  return(vStatesU)
}
########################################################################################
### best 
########################################################################################
best <- function(state, outcome) {
  bestInitialize()
  ###########################################################
  ## Read outcome data
  ###########################################################
  dfOutcome <- readOutcomeData()
  
  ###########################################################
  ## Check that state and outcome are valid
  ###########################################################
  if (!state %in% getUniqueStates()) {
    # warning('that state stinks, whaddya thinkin invalid state')
    stop('invalid state')
  }
  vValidOutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% vValidOutcome) {
    # warning('that state stinks, whaddya thinkin invalid state')
    stop('invalid outcome')
  }
  strFullColumnName <- fullColumnName(outcome)
  print("*** strFullColumnName ***")
  print(strFullColumnName)
  ########################################################################################
  ########################################################################################
  ########################################################################################
  df3 <- create3colDataFrame(dfOutcome, outcome)
  dfState <- filterByState(df3, state)
  print("*** dfState ***")
  print(state)
  print(class(dfState))
  print(nrow(dfState))
  print(colnames(dfState))
  print(head(dfState))
  
  
  dfOrder <- sortByOutcome(dfState, strFullColumnName)
  print("*** dfOrder ***")
  print(class(dfOrder))
  print(nrow(dfOrder))
  print(colnames(dfOrder))
  print(head(dfOrder))
  ########################################################################################
  ## Return hospital name in that state with lowest 30-day death
  ### 30 day mortality rates from HEART ATTACK = Column 11
  ### Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  ########################################################################################
#   vHeartAttack <- dfOutcome[,11]
#   print(length(vHeartAttack))
#   vHeartAttack <- na.omit(vHeartAttack)
#   print(length(vHeartAttack))
  
  ########################################################################################
  ### 30 day mortality rates from HEART FAILURE    = Column 17
  ### Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failur
  ########################################################################################
#   vHeartFailure <- dfOutcome[,17]
#   print(length(vHeartFailure))
#   vHeartFailure <- na.omit(vHeartFailure)
#   print(length(vHeartFailure))
  
  
  ########################################################################################
  ### 30 day mortality rates from HEART FAILURE    = Column 23
  ### Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  ########################################################################################
#   dfOutcome[, 23] <- as.numeric(dfOutcome[, 23])
#   vPneumonia <- dfOutcome[,23]
#   print("*** vPneumonia ***")
#   print(length(vPneumonia))
#   print(class(vPneumonia))
  
#   vPneumoniaNA <- na.omit(vPneumonia)
#   print("*** vPneumoniaNA ***")
#   print(class(vPneumoniaNA))
#   print(length(vPneumoniaNA))
  
#   vPneumoniaNASort <- sort(vPneumonia, decreasing=FALSE)
#   print(length(vPneumoniaNASort))

#   vPSRemoveNA <- sort(vPneumoniaNA, decreasing=FALSE, na.last = NA)
#   print("*** vPSRemoveNA ***")
#   print(class(vPSRemoveNA))
#   print(length(vPSRemoveNA))
#   print(vPSRemoveNA)
  ## rate
}
########################################################################################
### create3colDataFrame
########################################################################################
create3colDataFramed <- function() {
  bestInitialize()
  dfOutcome <- readOutcomeData()
  strFullColumnName <- fullColumnName(outcome)
  df3 <- select(dfOutcome, State, Hospital.Name, strFullColumnName)
  return(df3)
}
create3colDataFrame <- function(dfOutcome, outcome) {
  strFullColumnName <- fullColumnName(outcome)
  df3 <- dfOutcome[,c("State", "Hospital.Name", strFullColumnName)]
  ### This subset by column number also works, I would rather do the explicit column names
  # df3 <- dfOutcome[,c(7,2,11)]
  return(df3)
}

########################################################################################
### filterByState
########################################################################################
filterByState <- function(df3, state) {
  dfState <- df3[df3$State == state,]
  return(dfState)
}
fullColumnName <- function(strColAbbr) {
  strFullColNa <- if (strColAbbr == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else{
    if (strColAbbr == "pneumonia") {
      "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else{   {
      if (strColAbbr == "heart failure") {
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failur"
      } else {
        warning("invalid outcome")
      }
    }
    }
  }
  return(strFullColNa)
}
### http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns
sortByOutcome <- function(dfState, outcome) {
    dfOrder <- dfState[order(dfState[[outcome]], decreasing=TRUE, na.last=TRUE), ] 
  return(dfOrder)
}
########################################################################################
### splitByState
########################################################################################
splitByState <- function() {
  bestInitialize()
  dfOutcome <- readOutcomeData()
  strOutcome <- "heart attack"
  df3 <- create3colDataFrame(dfOutcome, strOutcome)
  # factorStates <- as.factor(getUniqueStates())
  vcStates <- df3$State
  print("***  vcStates ***")
  print(class(vcStates))
  print(length(vcStates))
  
  lstStateGroups <- split(df3, vcStates)
  print("***  lstStateGroups ***")
  print(class(lstStateGroups))
#   print(str(lstStates))
  print(length(lstStateGroups))
print(names(lstStateGroups))
  #   print(str(lstStates[30]))

  
  #######################################################################
  ### Subset by Value
  ### the Dataframe for ONLY North Dakata
  ### This works 2015/08/24
  #######################################################################
#   lstNDOnly <- df3[df3$State == 'ND',]
#   # ndList <- subset(lstStates, State == "ND")
#   print("***  lstNDOnly ***")
#   #   print(lstNDOnly)
#   print(class(lstNDOnly))
#   print(length(lstNDOnly))
#   print(str(lstNDOnly))
}
########################################################################################
### testBest 
########################################################################################
testBest <- function() {
  # best ("xx", "heart attack") 
  # best ("TX", "xxxxxx") 
  # debug(sortByOutcome)
  best("TX", "pneumonia")
  
#   dfOutcome <- readOutcomeData()
#   df3 <- create3colDataFrame(dfOutcome)
#   dfTX <- filterByState(df3, "ND")
#   print("***  dfTX ***")
#   print(class(dfTX))
#   print(length(dfTX))
#   print(str(dfTX))
}
