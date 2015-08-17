########################################################################################################################################################
### C:\Users\p622403\Documents\Work\R\BBB
### harvestMapAddresses.R
### August 14 2015 
########################################################################################################################################################
############################################################################
### http://zevross.com/blog/2015/05/19/scrape-website-data-with-the-new-r-package-rvest/
### devtools::install_github("rstudio/leaflet")
########################################################################################################################################################
############################################################################
### getHarvestPackages
### Install the necessary packages
############################################################################
getHarvestPackages <- function() {
  if (!require(rvest)) install.packages('rvest')
  library(rvest)
  if (!require(leaflet)) install.packages('leaflet')
  library(leaflet)
  # library(ggmap)
  library(RColorBrewer)
  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)
  if (!require(stringr)) install.packages('stringr')
  library(stringr)
  if (!require(RCurl)) install.packages('RCurl')
  library(RCurl)
  if (!require(XLConnect)) install.packages('XLConnect')
  library(XLConnect)
}
############################################################################
### saveEntireWebsiteToLocalFile
############################################################################
saveEntireWebsiteToLocalFile <- function(strURL) {
  library(RCurl)
  library(XML)
  # strURL <- "http://www.bbb.org/tulsa/accredited-business-directory/painting-contractors#"
  # strURL <- "http://www.bbb.org/tulsa/business-reviews/construction-and-remodeling-services/3-d-construction-in-coweta-ok-38016087"
  # strURL <- "http://www.bbb.org/boston/business-reviews/roofing-contractors/a-roofing-llc-in-williston-vt-121527"
  # strURL <- "http://www.bbb.org/boston/business-reviews/painting-contractors/lifetyme-exteriors-llc-in-needham-hgts-ma-95426"
  ### No Customer Review Table :   strURL <- "http://www.bbb.org/boston/business-reviews/insurance-companies/brooklawn-insurance-agency-inc-in-new-bedford-ma-62884/customer-reviews/"
  ### List of URLs with some sponsored
  strURL <- "http://www.bbb.org/boston/accredited-business-directory/find/asphalt?s=asphalt&cbbb=1"
  # strURL <- "http://www.bbb.org/nebraska/business-reviews/fire-and-water-damage-restoration/paul-davis-restoration-in-columbus-ne-300033282"
  
  webpage <- getURL(strURL)
  # print(webpage)
  # Process escape characters
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  # blnLapTop <- TRUE
  saveToALocalFile(webpage, blnTimeStampI=TRUE, blnAtWork=TRUE, strBaseFileNa="ListWithSponsored")
}
############################################################################
### parseThisWebsite
############################################################################
parseThisWebsite <- function(strURL) {
  url <- html(strURL)
  return(url)
}
############################################################################
### parseTheEntireWebsite
############################################################################
parseTheEntireWebsite <- function() {
  # URL for the Visit Ithaca website, wineries page
  # url <- html("http://www.visitithaca.com/attractions/wineries.html")
  # url <- html("http://www.visitithaca.com/attractions/cayuga-wine-trail-661")
  url <- html("http://www.bbb.org/new-jersey/business-reviews/contractors-general/carlson-bros-inc-in-fair-lawn-nj-17002237")
  print(class(url))
  return(url)
}

############################################################################
### extractListOfURLs
### http://stackoverflow.com/questions/27297484/r-using-rvest-package-instead-of-xml-package-to-get-links-from-url
############################################################################
extractListOfURLs  <- function(url) {
  selector_name<-".track-click"
  fnames <- url %>% html_nodes("a") %>% html_attr("href")
  #   fnames <- html_nodes(url, selector_name) %>% html_nodes("a") %>%
  #       html_text()
  
  vURLClean3 <- gsub("\r\n", "", fnames, fixed = TRUE)
  # vURLClean <-  gsub(" ", "", vURLClean3, fixed = TRUE)
  vURLClean <- vURLClean3
  
  #   print("*** vURLClean *** ")
  #   print(class(vURLClean))
  # print(vURLClean)
  return(vURLClean)
}
############################################################################
### extractTDFromFirst2Tables
############################################################################
extractTDFromFirst2Tables  <- function(url) {
  ### These 2 lines are for DEBUG, stand alone processing
  # strSingleURL <- "http://www.bbb.org/greater-maryland/business-reviews/fire-and-water-damage-restoration/water-mold-and-fire-baltimore-in-baltimore-md-90261344"
  ### 10 rows, 2 positive reviews :
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/roofing-contractors/a-roofing-llc-in-williston-vt-121527"
  ### Easy One : strSingleURL <- "http://www.bbb.org/dallas/business-reviews/fire-and-water-damage-restoration/puroclean-restoration-specialists-in-frisco-tx-90247270"
  ### No Customer Review Table :   strSingleURL <- "http://www.bbb.org/boston/business-reviews/insurance-companies/brooklawn-insurance-agency-inc-in-new-bedford-ma-62884/customer-reviews/"
  #   url  <- parseThisWebsite(strSingleURL)
  #   print("*** url done *** ")
  # print(html_text(url))
  
  selector_complaint_table <- ".complaint-table td"
  # selector_complaint_table <- ".complaint-table .even,.odd td"
  #   fnames <- html_nodes(url, selector_name) %>%
  #     html_text()
  vTDs <- html_nodes(url, selector_complaint_table) %>%  html_text()
  #   print("*** vTDs ***")
  #   print(length(vTDs))
  #   print(class(vTDs))
  #   print(vTDs)
  
  if (length(vTDs)>0) {
    df2 <- cleanExtractedHTMLText(vTDs)
  }
  else {
    ### ten zero, data frame
    dfZeroes <- data.frame(matrix("0", nrow = 1, ncol = 10), stringsAsFactors=FALSE)
    selector_ATH <- ".active-table-header a"
    vReviews <- html_nodes(url, selector_ATH) %>%  html_text()
    #     print("*** vReviews ***")
    #     print(vReviews)
    selector_NoLink <- ".nolink"
    vReviewsNoLink <- html_nodes(url, selector_NoLink) %>%  html_text()
    #     print("*** vReviewsNoLink ***")
    #     print(vReviewsNoLink)
    vReviewsNoLinkNumber <- gsub("[^0-9]", "", c(vReviews, vReviewsNoLink))
    #     print("*** vReviewsNoLinkNumber ***")
    #     print(vReviewsNoLinkNumber)
    if (length(vReviewsNoLinkNumber)==3) {
      dfZeroes[8] <- vReviewsNoLinkNumber[1]
      dfZeroes[9] <- vReviewsNoLinkNumber[2]
      dfZeroes[10] <- vReviewsNoLinkNumber[3]
      df2 <- dfZeroes
      #       print("*** df2 ***")
      #       print(df2)
    }
    else {
      ### Return 0 length data frame, this causes useListExtractDetails to log an Error in  the XMLLogFile
      df2 <- data.frame(Labels = character(0), stringsAsFactors = FALSE)
    }
  }
  return(df2)
}
cleanExtractedHTMLText  <- function(vTDs) {
  library(stringr)
  print("*** cleanExtractedHTMLText started ***")
  # vTDs <- url %>% html_nodes("table") %>% `[`(1:2) %>% html_nodes("td") %>% html_text()
  # vTDs <- url %>% html_nodes(".complaint-table") %>% html_nodes("td") %>% html_text()
  # print("*** gsub ***")
  vTDClean3 <- gsub("\r\n", "", vTDs, fixed = TRUE)
  #     print("*** vTDClean3 ***")
  #     print(length(vTDClean3))
  # vTDClean <-  gsub(" ", "", vTDClean3, fixed = TRUE)
  vTDClean <-  str_trim(vTDClean3)
  #       print("*** vTDClean ***")
  #       print(length(vTDClean))
  #   for (i in 1:length(vTDClean)){
  #     cat("\n",i, ")",vTDClean[i])
  #   }
  ###http://stackoverflow.com/questions/24440258/selecting-multiple-odd-or-even-columns-rows-for-dataframe-in-r
  # print("*** odds and evens *** ")
  odd_indexes<-seq(1,length(vTDClean)+1,2)
  even_indexes<-seq(2,length(vTDClean)+2,2)
  vLabels <- vTDClean[odd_indexes]
  vValues <- vTDClean[even_indexes]
  vValues <-  gsub("0CustomerReviews", "0", vValues, fixed = TRUE)
  #   if (length(vValues)>8){
  #     vValues[7:9] <-  gsub("", "0", vValues[7:9])
  #   }
  intMaxElement <- min(length(vValues), 9)
  vValues[1:intMaxElement] <- sapply(vValues[1:intMaxElement],  replaceNullWithZero)
  #   vTDClean <- str_replace_all(vTDs, fixed(" "), "")
  
  ### setup a 2 column data frame
  # print("*** df *** ")
  colClasses = c("character", "character")
  vColNames  = c("Labels", "Vals")
  df4 <- data.frame(Labels = character(length(vLabels)), stringsAsFactors = FALSE)
  #   print("*** df4 *** ")
  #   print(nrow(df4))
  #   print(length(vLabels))
  if (length(vLabels)>0) {
    for(i in 1:length(vLabels)){
      df4$Labels[i] <- vLabels[i]
    }
    df3 <- mutate(df4, Vals=vValues)
    names(df3) <- vColNames
    df2 <- df3[complete.cases(df3),]
  } else {
    df2 <- df4
  }
  #   print("*** df2 *** ")
  #   print(nrow(df2))
  return(df2)
}
replaceNullWithZero <- function(x) {
  strValue <- x
  if (is.na(x)) {
    strValue <- "0"
  }
  else{if (x=="") {
    strValue <- "0"
  }
  }
  return(strValue)
}

############################################################################
### extractBBBRating
############################################################################
extractBBBRating  <- function(url) {
  ### These 2 lines are for DEBUG, stand alone processing
  ### A minus example
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/painting-contractors/lifetyme-exteriors-llc-in-needham-hgts-ma-95426"
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/building-contractors/fowler-s-roofing-in-chelsea-me-93008"
  ### A example
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/handyman-services/aurora-handyman-service-llc-in-n-providence-ri-141336"
  ### APlus example
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/mortgage-brokers/homestate-mortgage-inc-in-hopkinton-ma-122931"
  ### No Rating Found  example
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/building-contractors/pro-lines-in-plainville-ma-124501/customer-reviews/"
  # url  <- parseThisWebsite(strSingleURL)
  
  #   
  strRating <- "N/R" 
  ratingNode <- url %>%  html_nodes(xpath="//div[@id='accedited-rating']/img") 
  #   print("*** ratingNode *** ")
  #   print(length(ratingNode))
  #   print(ratingNode)
  if (length(ratingNode)>0) {
    strRatingFullFileNa <-   html_attr(ratingNode, "src")
    #     print("*** strRatingFullFileNa *** ")
    #     print(strRatingFullFileNa)
    vParts <- strsplit(strRatingFullFileNa, "/")[[1]]
    strRatingFileNa <- vParts[length(vParts)]
    vRating <- strsplit(strRatingFileNa, "\\.")[[1]]
    #     print("*** vRating *** ")
    #     print(vRating)
    strRating <- vRating[1]
    strRating <- gsub("cbbb-accred-","",strRating)  
  }
  #   print("*** strRating *** ")
  #   print(strRating)
  return(strRating)
}
############################################################################
### extractLicenseNumber
############################################################################
extractLicenseNumber   <- function(url) {
  ### These 2 lines are for DEBUG, stand alone processing
  ### A minus example
  #      strSingleURL <- "http://www.bbb.org/new-jersey/business-reviews/contractors-general/carlson-bros-inc-in-fair-lawn-nj-17002237"
  #   # strSingleURL <- "http://www.bbb.org/boston/business-reviews/painting-contractors/cq-painting-inc-in-s-weymouth-ma-103741"
  #   url  <- parseThisWebsite(strSingleURL)
  
  #   
  strLicenseNumber <- "liscenseNumber" 
  # LicenseNumberNode <- url %>%  html_nodes(xpath="//p[contains(b,'NJ Division of Consumer Affairs')]/span") 
  # LicenseNumberNode <- url %>%  html_nodes(xpath="//p[contains(b,'Massachusetts Division of Professional Licensure')]/span") 
  LicenseNumberNode <- url %>%  html_nodes(xpath="//p/span[contains(.,'The number is ')]") 
  #       print("*** LicenseNumberNode *** ")
  #       print(length(LicenseNumberNode))
  #       print(LicenseNumberNode)
  if (length(LicenseNumberNode)>0) {
    strLicenseNumber <-   html_text(LicenseNumberNode)
    strLicenseNumber <- gsub("The number is ","",strLicenseNumber)  
  }
  #     print("*** strLicenseNumber *** ")
  #     print(strLicenseNumber)
  return(strLicenseNumber)
}
############################################################################
### extractPrincipals
############################################################################
extractPrincipalsWorkSometimes   <- function(url) {
  ### These 2 lines are for DEBUG, stand alone processing
  strSingleURL <- "http://www.bbb.org/boston/business-reviews/roofing-contractors/a-roofing-llc-in-williston-vt-121527"
  # strSingleURL <- "http://www.bbb.org/new-jersey/business-reviews/contractors-general/carlson-bros-inc-in-fair-lawn-nj-17002237"
  
  url  <- parseThisWebsite(strSingleURL)
  # print(html_text(url))
  print("*** URL DONE ***")
  
  #   
  vPrincipal <- c("Principal")
  nodePrincipal <- url %>%  html_nodes(xpath="//span[@class='employees']") %>% html_nodes(xpath="//span[@itemprop='name']") 
  # %>%  html_nodes(xpath="//span[@class='employees']") 
  print("*** nodePrincipal *** ")
  print(html_text(nodePrincipal))
  print(nodePrincipal)
  print(length(nodePrincipal))
  print(class(nodePrincipal))
  print(is.null(nodePrincipal))
  print(attributes(html_text(nodePrincipal)))
  
  if (length(nodePrincipal)==0) {
    print("*** Business Management  *** ")
    # nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Management']/following-sibling::*[1]") 
    # nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Management']/following-sibling::*[name()='span']") 
    
    nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Contact Information']/following-sibling::*[1][name()='div']/span") 
    print(nodePrincipal)
    print(length(nodePrincipal))
  }  
  if (length(nodePrincipal)>0) {
    vPrincipal <-   html_text(nodePrincipal)
    # strNoEmplyees <- gsub("The number is ","",strNoEmplyees)  
    print("*** vPrincipal  *** ")
    print(class(vPrincipal))
    print(length(vPrincipal))
  }
  print(vPrincipal)
  
  nodeJobTitle <- url %>%  html_nodes(xpath="//span[@class='employees']") %>% html_nodes(xpath="//span[@itemprop='jobTitle']") 
  if (length(nodeJobTitle)>0) {
    vJobTitle  <-   html_text(nodeJobTitle)
    print("*** vJobTitle  *** ")
    print(class(vJobTitle))
    print(length(vJobTitle))
    print(vJobTitle)
  }
  
  return(vPrincipal)
}
############################################################################
### extractAltBusnNames
############################################################################
extractAltBusnNamesNotQuiteWorking   <- function(url) {
  library(stringr)
  ### These 2 lines are for DEBUG, stand alone processing
  strSingleURL <- "http://www.bbb.org/boston/business-reviews/roofing-contractors/a-roofing-llc-in-williston-vt-121527"
  # strSingleURL <- "http://www.bbb.org/wyoming-and-northern-colorado/business-reviews/contractors-general/rivertree-custom-builders-inc-in-steamboat-springs-co-46074616"
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/insurance-companies/brooklawn-insurance-agency-inc-in-new-bedford-ma-62884/customer-reviews/"
  # strSingleURL <- "http://www.bbb.org/south-dakota/business-reviews/solar-energy-equipment-and-systems-manufacturing-and-distribution/genpro-energy-solutions"
  url  <- parseThisWebsite(strSingleURL)
  # print(html_text(url))
  # print("*** URL DONE ***")
  
  #   
  vAltBusnNames <- c("AltBusnNames")
  vNames <- url %>%  html_nodes(xpath="//h5[.='Alternate Business Names']/following-sibling::*[1]") %>% html_text()
  if (length(vNames)>0) {
    vAltBusnNames <- gsub("\r\n", "", vNames, fixed = TRUE)
    vAltBusnNames <-  str_trim(vAltBusnNames)
    # nodeNext <- html_nodes(nodeAltBusnNames, xpath="//h5[.='Alternate Business Names']")
    print("*** vAltBusnNames  *** ")
    print(class(vAltBusnNames))
    print(length(vAltBusnNames))
    
  }
  print(vAltBusnNames)
}
############################################################################
### extractBusnCategories
### New Jersey and Boston, are still working from my ip 2015/08/10
############################################################################
extractBusnCategories   <- function(url) {
  library(stringr)
  ### These 2 lines are for DEBUG, stand alone processing
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/roofing-contractors/a-roofing-llc-in-williston-vt-121527"
  # strSingleURL <- html("http://www.bbb.org/new-jersey/business-reviews/contractors-general/carlson-bros-inc-in-fair-lawn-nj-17002237")
  #   strSingleURL <- "http://www.bbb.org/boston/business-reviews/insurance-companies/brooklawn-insurance-agency-inc-in-new-bedford-ma-62884/customer-reviews/"
  # url  <- parseThisWebsite(strSingleURL)
  # print(html_text(url))
  # print("*** URL DONE ***")
  vBusnCategories <- c("BusnCategories")
  vCategories <- url %>%  html_nodes(xpath="//h5[.='Business Category']/following-sibling::*[1]/span") %>% html_text()
  if (length(vCategories)==0) {
    print("*** CSText  *** ")
    # nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Management']/following-sibling::*[1]") 
    # nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Management']/following-sibling::*[name()='span']") 
    nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Category']/following-sibling::*[1][name()='p']") 
    strCategories <- html_text(nodePrincipal)
    print("*** strCategories  *** ")
    print(strCategories)
    if (length(strCategories)>0) { 
      vCategories <- strsplit(strCategories, ",")[[1]]
    }
    #     print(length(vCategories))
    #     print(vCategories)
  }
  if (length(vCategories)>0) {
    vBusnCategories <- gsub("\r\n", "", vCategories, fixed = TRUE)
    vBusnCategories <-  str_trim(vBusnCategories)
    # nodeNext <- html_nodes(nodeAltBusnNames, xpath="//h5[.='Alternate Business Names']")
    #     print("*** vBusnCategories  *** ")
    #     print(class(vBusnCategories))
    #     print(length(vBusnCategories))
  }
  strCategories <- paste(vBusnCategories, collapse = '|')
  # print(strCategories)
  return(strCategories)
}
############################################################################
### extractContent - Generic
############################################################################
extractContent  <- function(url, strItemprop) {
  vXPath <- c("//meta[@itemprop='", strItemprop, "']")
  strXPath <- paste(vXPath, collapse="")
  ContentNode <- url %>%  html_nodes(xpath=strXPath) 
  strContentText <-   html_attr(ContentNode, "content")
  return(strContentText)
}
############################################################################
### extractTheAddresses
############################################################################
extractTheAddresses  <- function(url) {
  # Pull out the addresses of the wineries and breweries
  # selector_address<-".results_summary_item_details td:nth-child(3) strong:first-child"
  selector_complaint_total <- ".complaint-table .even,.odd td:first-child"
  
  vTotals <- html_nodes(url, selector_complaint_total) %>%
    html_text()
  
  print("*** vTotals *** ")
  print(vTotals)
  return(vTotals)
}
############################################################################
### harvestTheNamesAndAddresses
############################################################################
harvestTheNamesAndAddresses  <- function(url) {
  
  dfComplaints <- as.data.frame(extractTDFromFirst2Tables(url))
  #   print("*** dfComplaints *** ")
  #   print(nrow(dfComplaints))
  #   print(dfComplaints)
  # vComplaintLabels <- extractTheNames(url)
  #   vTDs <- extractTDFromFirst2Tables(url)
  #   print("xxxxxxxx*** vTDs ***xxxxxxx ")
  #   print(paste("class vTDs:", class(vTDs)))
  #   print(paste("length vTDs:", length(vTDs)))
  # vBetterTDs <- str_trim(vTDs)
  # print(vTDs)
  dfInfo <- dfComplaints
  
  strname <- extractContent(url, "name")
  vInfo <- c(strname)
  #   strAddressStreet <- extractStreetAddresses(url)
  #   vInfo <- c(vInfo, strAddressStreet)
  #   strAddressLocality <- extractAddressLocality(url)
  #   vInfo <- c(vInfo, strAddressLocality)
  #   strAddressRegion <- extractAddressRegion(url)
  #   vInfo <- c(vInfo, strAddressRegion)
  
  strVariable <- "streetAddress"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "addressLocality"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "addressRegion"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "postalCode"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "telephone"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strtelephone  <- extractContent(url, "telephone"); vInfo <- c(vInfo, strtelephone)  
  strVariable <- "latitude"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "longitude"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "last-modified"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "foundingDate"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "url"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "map"
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  strVariable <- "naics"
  dfInfo <- extractContentAddToDF(url, dfInfo, strVariable)
  # 
  
  # vInfo <- c(vInfo, vTDs)
  
  #   vTotals <- extractTheAddresses(url)
  #   print(class(vTotals))
  
  print("*** harvestTheNamesAndAddresses - Complete ***")
  return(dfInfo)
}
############################################################################
### Extract ATTRIBUTE info, in this case the @content
############################################################################
extractContentAddToDF <- function(url, dfInfo, strVariable) {
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractContent(url, strVariable))
  return(dfInfo)
}
extractContentAttrAddToDF <- function(url, strVariable, dfInfo) {
  strValue <- strVariable
  strAttrValue <- extractContentAttr(url, strVariable)
  # dfInfo[nrow(dfInfo) +1,] <- c(strVariable,)
  if (length(strAttrValue)>0) {
    strValue <- strAttrValue
  }
  else {
    strElementValue <- extractElement(url, strVariable)
    if (length(strElementValue)>0) {
      #       print("*** strNewValue ***")
      #       print(strValue)
      #       print(length(strValue))
      strValue <- strElementValue
    }
  }
  dfInfo[nrow(dfInfo) +1,] <- strValue
  return(dfInfo)
}
extractContentAttr  <- function(url, strItemprop) {
  vXPath <- c("//meta[@itemprop='", strItemprop, "']")
  strXPath <- paste(vXPath, collapse="")
  ContentNode <- url %>%  html_nodes(xpath=strXPath) 
  strContentText <-   html_attr(ContentNode, "content")
  #   print("***strContentText***")
  #   print(strContentText)
  #   print(length(strContentText))
  if (strItemprop=='naics'){
    if (length(strContentText)==0) {
      strContentText <- strItemprop
    }
    else {if (strContentText=="-2147483648"){
      strContentText <- strItemprop
    }
    }
  }
  
  if (strItemprop=='longitude'){
    if (length(strContentText)==0) {
      strContentText <- strItemprop
    }
    else {if (strContentText=="-79228162514264337593543950335"){
      strContentText <- strItemprop
    }
    }
  }
  return(strContentText)
}
############################################################################
### extractElementAddToDF
############################################################################
extractElementAddToDF <- function(url, strVariable, dfInfo) {
  dfInfo[nrow(dfInfo) +1,] <- c(strVariable,extractElement(url, strVariable))
  return(dfInfo)
}
extractElement  <- function(url, strItemprop) {
  vXPath <- c("//span[@itemprop='", strItemprop, "']")
  strXPath <- paste(vXPath, collapse="")
  # print(paste("strXPath: ",strXPath))
  elementText <- url %>%  html_nodes(xpath=strXPath) %>% html_text()
  # print(paste("elementText: ",elementText))
  return(elementText)
}

############################################################################
### createListOfBBBReviewURLs
### Start with vector, filter as data.frame, return a vector
############################################################################
createListOfBBBReviewURLs  <- function(strURL, xmltreeLog) {
  print("*** createListOfBBBReviewURLs - Started ***")
  xmltreeLog$addTag("createListOfBBBReviewURLs", close=FALSE)
  xmltreeLog$addTag("strListURL", strURL) 
  ### Retrieve the HTML into an XML Dom
  ### New Jesey
  # print(strURL)
  xmlDOMWS <- parseThisWebsite(strURL)
  vURL4s <- extractListOfURLs(xmlDOMWS)
  # print(paste("Length dfURL4s: ", length(vURL4s)))
  xmltreeLog$addTag("AllURLs", as.character(length(vURL4s))) 
  
  dfURL4s <- as.data.frame(vURL4s,stringsAsFactors=FALSE)
  vColNames <- c("bbbReviewURL")
  names(dfURL4s) <- vColNames
  # print(paste("Length dfURL4s: ", nrow(dfURL4s)))
  
  # print(head(dfURL4s))
  # print(dfURL4s[30:42,])
  
  dfURLs3 <- filter(dfURL4s, grepl("business-reviews", bbbReviewURL))
  # print(paste("nrow(dfURLs3): ", nrow(dfURLs3)))
  xmltreeLog$addTag("BusnReviewURLs", as.character(nrow(dfURLs3))) 
  
  ##########################################################################################
  ### Prepend to relative URL to create an Absolute URL if necessary 
  ##########################################################################################
  if (nrow(dfURLs3)>0) {
    dfURLs2 <- filter(dfURLs3, !grepl("/quote/", bbbReviewURL))
    dfURLs2 <- filter(dfURLs2, !grepl("/customer-reviews/", bbbReviewURL))
    for (w in 1:nrow(dfURLs2)){
      #       print(dfURLs2$bbbReviewURL[w])
      #       print(length(grep("http://www.bbb.org",dfURLs2$bbbReviewURL[w])))
      #       print(length(grep("http://www.bbb.org",dfURLs2$bbbReviewURL[w]))==0)
      if (length(grep("http://www.bbb.org",dfURLs2$bbbReviewURL[w]))==0) {
        dfURLs2$bbbReviewURL[w] <- paste("http://www.bbb.org",dfURLs2$bbbReviewURL[w], sep="")  
      }
      #       print("dfURLs2$bbbReviewURL[w]")
      #       print(dfURLs2$bbbReviewURL[w])
      # dfURLs2$bbbReviewURL <-  paste("http://www.bbb.org",dfURLs2$bbbReviewURL, sep="")
    }
    # print(paste("nrow(dfURLs2): ", nrow(dfURLs2)))
    xmltreeLog$addTag("NoQuoteURLs", as.character(nrow(dfURLs2))) 
    
    dfURLs1 <-unique(dfURLs2)
    # print(paste("nrow(dfURLs1): ", nrow(dfURLs1)))
    xmltreeLog$addTag("UniqueURLs", as.character(nrow(dfURLs1))) 
    #   intLthdfURLs <- nrow(dfURLs)
    #   print(paste("intLthdfURL2s: ",intLthdfURL2s))
  }
  else{
    dfURLs1 <- data.frame(Lables=character(), Vals=character(), stringsAsFactors=FALSE) 
    xmltreeLog$addTag("error",  close=FALSE)
    xmltreeLog$addTag("message", "Zero BBBreview URLs found in this list") 
    xmltreeLog$addTag("strURL", strURL) 
  }
  ##############################################################
  ### Save List as Excel File 
  ##############################################################
  # SaveDataFrameAsExcel(dfURLs2, createLocalExcelFileNa("harvestListOfMAPainters")) 
  xmltreeLog$closeTag()
  vListOfURLS <- dfURLs1$bbbReviewURL
  print("*** createListOfBBBReviewURLs - Complete ***")
  return(vListOfURLS)
}

############################################################################
### useListExtractDetails
############################################################################
useListExtractDetails  <- function(xmltreeLog, strListURL, strRegion, strCategory, strPage, vMasterURL) {
  print("*** useListExtractDetails - Started ***")
  
  vListOfURLs <- createListOfBBBReviewURLs(strListURL, xmltreeLog)
  ### Eliminate any that have already been done
  vListOfURLs  <- vListOfURLs[!vListOfURLs %in% vMasterURL]
  vDuplicateURLs  <- vListOfURLs[vListOfURLs %in% vMasterURL]
  for(strDupeURL in vDuplicateURLs) {
    print("*** DUPLICATE found ***")
    print(strDupeURL)
    xmltreeLog$addTag("error",  close=FALSE)
    xmltreeLog$addTag("message", "Duplicate URL found") 
    xmltreeLog$addTag("strDupeURL", strDupeURL) 
    xmltreeLog$closeTag()
  }
  vMasterURL <- c(vMasterURL, vListOfURLs)
  
    # dfURLs <- createListOfBBBReviewURLs(strListURL, xmltreeLog)
  ### Only One  FUDGE for Debug 2015/08/12
  # vPages <- c("http://www.bbb.org/boston/business-reviews/roofing-contractors/a-roofing-llc-in-williston-vt-121527")
  # vPages <- c("http://www.bbb.org/new-jersey/business-reviews/contractors-general/carlson-bros-inc-in-fair-lawn-nj-17002237")
  # SPonsored BBBreview, very little info :   vListOfURLs <- c("http://www.bbb.org/boston/business-reviews/stone-setting/spencer-associates-in-beverly-ma-116362/customer-reviews/")
  # vListOfURLs<- "http://www.bbb.org/boston/business-reviews/stone-setting/spencer-associates-in-beverly-ma-116362"
  #     vPages <- c("http://www.bbb.org/boston/business-reviews/stone-setting/spencer-associates-in-beverly-ma-116362/customer-reviews/")
  #     dfURLs <- as.data.frame(vPages, stringsAsFactors=FALSE)
  # print(vListOfURLs)
  ##############################################################
  ### Save List dataframe  as csv file
  ##############################################################
  #   if (nrow(dfURLs)>0) {
  #     vBaseName <- c(gsub("-","",strRegion), "_",  gsub("-","",strCategory), "_", gsub("=","",strPage))
  #     strBaseName <- paste(vBaseName, collapse = "")
  #     strFullFileNa <- createLocalCSVFileNa(strBaseName)
  #     print("*** List File Name ***")
  #     print(strFullFileNa)
  #     #   xmltreeLog$addTag("strFullFileNa", strFullFileNa) 
  #     SaveDataFrameAsCSV(dfURLs, strFullFileNa, FALSE) 
  #   }
  
  
  ### define empty Data.frame
  vVariables <- c("legalName","streetAddress","addressLocality","addressRegion","postalCode","telephone","faxNumber","url","latitude","longitude","foundingDate","naics")
  colClasses = c(rep("character",length(vVariables)))
  #   vColNames  = c("Labels", "Vals")
  dfInfoPage <- data.frame(colClasses, stringsAsFactors = FALSE)
  # intTopRow <- min(length(vListOfURLs), 5)
  intTopRow <- min(length(vListOfURLs), 270)
  #   print("*** intTopRow ***")
  #   print(intTopRow)
  if (intTopRow>0) {
    # for(i in 1:nrow(dfURLs)){
    for(i in 1:intTopRow){
      print(paste("extracting Details number: ", i))
      # strSingleURL <- dfURLs[i,1]
      strSingleURL <- vListOfURLs[i]
      # print(paste("Single URL:", strSingleURL))
      url  <- parseThisWebsite(strSingleURL)
      # print("Website Parsed")
      #   
      # dfInfo <- harvestTheNamesAndAddresses(url) 
      dfComplaints <- as.data.frame(extractTDFromFirst2Tables(url))
      #       print("uuuuu dfComplaints complete uuuuu")
      #       print(nrow(dfComplaints))
      if (nrow(dfComplaints)==0) {
        xmltreeLog$addTag("error",  close=FALSE)
        xmltreeLog$addTag("message", "Complaints table not found") 
        xmltreeLog$addTag("strSingleURL", strSingleURL) 
        xmltreeLog$closeTag()
        next
      }
      dfInfo <- dfComplaints
      dfInfo <- rbind(dfInfo, c("BBBRating",extractBBBRating(url)))
      # print(paste("nrow(dfInfo) : ", nrow(dfInfo)))
      if(nrow(dfInfo)==0) {
        dfInfo <- data.frame(Labels = character(0), Vals = character(0), stringsAsFactors = FALSE)
      }
      for(s in 1:length(vVariables)){
        ### Did not extract many details from 20150723
        ### http://www.bbb.org/greater-maryland/business-reviews/windows/chandler-remodeling-in-cockeysville-md-20013629
        # print(vVariables[s])
        dfInfo <- extractContentAttrAddToDF(url, vVariables[s], dfInfo)  
      }
      
      dfInfo <- rbind(dfInfo, c("LicenseNumber",extractLicenseNumber(url)))
      dfInfo <- rbind(dfInfo, c("BBBCategories",extractBusnCategories(url)))
      dfInfo <- rbind(dfInfo, c("BBBURL",strSingleURL))
      dfInfo <- rbind(dfInfo, c("BBBRegion", strRegion))
      dfInfo <- rbind(dfInfo, c("BBBCategory", strCategory))
      dfInfo <- rbind(dfInfo, c("BBBPage", strPage))
      dfInfo <- rbind(dfInfo, c("Item", toString(i)))
      # print("*** dfInfo *** ")
      # print(dfInfo) 
      
      ##############################################################
      ### Transpose Name-Value pair dataframe to 1 Long row dataframe   
      ##############################################################
      dfRow <- data.frame(t(dfInfo))
      dfRow2 <- dfRow[2,]
      
      ##################################################################
      ### Start New dataframe, or append new row to existing dataframe
      ##################################################################
      if(i==1) {
        dfInfoPage <- dfRow2
      }
      else {
        if ((nrow(dfRow2)>0) && (ncol(dfRow2)==ncol(dfInfoPage))) {
          dfInfoPage <- rbind(dfInfoPage, dfRow2[1,])
        }
        # print(str(dfInfoPage))
      }
    }
  }
  ##############################################################
  ### Save List dataframe  as Excel File with 2 rows
  ##############################################################
  #   vBaseName <- c(gsub("-","",strRegion), "_",  gsub("-","",strCategoryFileNa), "_", gsub("=","",strPage))
  #   strBaseName <- paste(vBaseName, collapse = "")
  # #   strFullFileNa <- createLocalExcelFileNa(strBaseName)
  # #   SaveDataFrameAsExcel(df4, strFullFileNa) 
  #     strFullFileNa <- createLocalCSVFileNa(strBaseName)
  #     print("*** List File Name ***")
  #     print(strFullFileNa)
  #   #   xmltreeLog$addTag("strFullFileNa", strFullFileNa) 
  #     SaveDataFrameAsCSV(df4, strFullFileNa) 
  
  print("*** useListExtractDetails - Complete ***")
#   print("*** dfInfoPage ***")
#   print(nrow(dfInfoPage))
#   print(class(dfInfoPage))
#   print(dfInfoPage)
  # return(dfInfoPage)
  # dfFiller <- as.data.frame(matrix(1:10, ncol = 2, nrow = 5), colClasses = vColClasses)
  # returnList <- list("dfInfoPage" = dfInfoPage, "dfFiller" = dfFiller)
  returnList <- list("dfInfoPage" = dfInfoPage, "vMasterURL" = vMasterURL)
  return(returnList)
}
############################################################################
### findPages
############################################################################
findPages   <- function(strListURL) {
  ### Initialize 
  vPages <- c("page=1")
  # strListURL <- "http://www.bbb.org/boston/accredited-business-Guide/painting-contractors/437/?page=1"
  #   strListURL <- "http://www.bbb.org/new-jersey/accredited-business-directory/painting-contractors/437/?page=1"
  url  <- parseThisWebsite(strListURL)
  #   print("*** url done *** ")
  
  # selector_complaint_label <- ".complaint-table .even,.odd td:first-child"
  selector_class_dirp <- ".dirpaging a"
  #   fnames <- html_nodes(url, selector_name) %>%
  #     html_text()
  vClassDirp <- html_nodes(url, selector_class_dirp) %>%
    html_text()
  #   print("*** vClassDirp *** ")
  #   print(class(vClassDirp))
  #   print(length(vClassDirp))
  #   print(vClassDirp)
  if (length(vClassDirp)) {
    vPages <-  sapply(vClassDirp, prependPage)
    #     print("*** vPages *** ")
    #     print(class(vPages))
    #     print(length(vPages))
    #     print(vPages)
  }
  #   print("*** drURLs ***")
  #   print(nrow(dfURLs))
  # vPages <- c("page=1", "page=2", "page=3")
  return(vPages)
}
prependPage <- function(vIn) {
  vNewPages <- paste("page=", vIn, sep="")
  return(vNewPages)
}
############################################################################
### processPages
############################################################################
processPages   <- function(xmltreeLog, strRegion, strCategory, vMasterURL) {
  print("*** processPages - Started ***")
  vListURL <- c("http://www.bbb.org/", strRegion, "/accredited-business-directory/", strCategory, "?page=1")
  strListURL <- paste(vListURL,  collapse="")
  # print(paste("strListURL", strListURL))
  ### Only One  FUDGE for Debug 2015/08/11
  # vPages <- c("page=1")
  vPages <- findPages(strListURL)
  
  for(p in 1:length(vPages)){
    # print(paste("p: ", p))
    strPage <-vPages[p]
    # print(paste("vPages[p]: ", strPage))
    xmltreeLog$addTag("page",  close=FALSE)
    xmltreeLog$addTag("pagnum", as.character(strPage)) 
    vListURL <- c("http://www.bbb.org/", strRegion, "/accredited-business-directory/", strCategory, "?", strPage)
    strListURL <- paste(vListURL,  collapse="")
    # print(paste("strListURL: ", strListURL))
    # xmltreeLog$addTag("strListURL", strListURL)
    # dfPage <- useListExtractDetails(xmltreeLog, strListURL, strRegion, strCategory, strPage, vHOLDMaster)
    # list[dfInfoPage, dfFiller]  <- useListExtractDetails(xmltreeLog, strListURL, strRegion, strCategory, strPage, vMasterURL)
    lTwo  <- useListExtractDetails(xmltreeLog, strListURL, strRegion, strCategory, strPage, vMasterURL)
#     print("*** lTwo ***")
#     print(class(lTwo))
#     print(str(lTwo))
    vMasterURL <- lTwo$vMasterURL
#     print("*** vMasterURL ***")
#     print(class(vMasterURL))
#     print(length(vMasterURL))
#     dfFiller <- lTwo$dfFiller
#     print("*** dfFiller ***")
#     print(class(dfFiller))
#     print(nrow(dfFiller))
#     print(dfFiller)
    dfPage <- lTwo$dfInfoPage
#     print("*** dfPage ***")
#     print(class(dfPage))
#     print(dfPage)
    
    if (p==1) {
      dfPages <- dfPage
    }
    else {
      if ((nrow(dfPage)>0) && (ncol(dfPage)==ncol(dfPages))) {
        dfPages <- rbind(dfPages, dfPage)
        # print(paste("nrow(dfPages):",nrow(dfPages)))
        xmltreeLog$addTag("NumberOfPages", as.character(nrow(dfPages))) 
      }
    }
    xmltreeLog$closeTag()
  }
  ###################################################################################
  ### Create A local Excel File Name, save the data.frame
  ###################################################################################
  vBaseName <- c(gsub("-","",strRegion), gsub("-","",strCategory))
  strBaseName <- paste(vBaseName, collapse = "")
  # strFullFileNa <- createLocalExcelFileNa(strBaseName)
  strFullFileNa <- createLocalCSVFileNa(strBaseName, blnTimeStampI=TRUE, blnLaptop=TRUE)
  # print(strFullFileNa)
  xmltreeLog$addTag("strFullFileNa", strFullFileNa) 
  # SaveDataFrameAsExcel(dfPages, strFullFileNa) 
  SaveDataFrameAsCSV(dfPages, strFullFileNa, FALSE)
  print("*** processPages - Complete ***")
  # return(dfPages)
  returnList <- list("dfPages" = dfPages, "vMasterURL" = vMasterURL)
  return(returnList)
}
############################################################################
### processCategories
############################################################################
processCategories   <- function(xmltreeLog, strRegion, vMasterURL) {
  print("*** processCategories - Started ***")
  ### , "Smoke Odor Counteracting Service" does not exist in Central Mass  
  ### vCategories <- c("mold-and-mildew-inspection", "mold-consulting-and-testing")
  vCategories <- vectorFromExcelFile()
  ### if you want to use only one category, then change the first worksheet in the file
  for(c in 1:length(vCategories)){
    xmltreeLog$addTag("Category", close=FALSE) 
    xmltreeLog$addTag("catnum", as.character(vCategories[c]))
    # dfInfo <- processPages(xmltreeLog, strRegion, vCategories[c], vMasterURL)  
    lTwo <- processPages(xmltreeLog, strRegion, vCategories[c], vMasterURL)  
    dfInfo <- lTwo$dfInfo
    vMasterURL <- lTwo$vMasterURL
    xmltreeLog$closeTag()
  }
  print("*** processCategories - Complete ***")
  returnList <- list("dfInfo" = dfInfo, "vMasterURL" = vMasterURL)
  return(returnList)
}
############################################################################
### processRegions
############################################################################
processRegions   <- function() {
  print("*** processRegions - Started ***")
  ########################################################################################
  ### Initialize
  ########################################################################################
  getHarvestPackages()
  blnLaptop = TRUE
  ### Global Variable
  # xmltreeLog <<- startXMLTree()
  assign("xmltreeLog", startXMLTree(), envir = .GlobalEnv)
  assign("vMasterURL",  vector(), envir = .GlobalEnv)
  # vMasterURL <<- vector()
  
  # vRegions <- c("boston","central-texas", "central-oklahoma", "central-western-massachusetts","chicago", "columbia","dallas", "eastern-oklahoma", "eastern-washington", "greater-maryland","myrtle-beach", "nebraska", "new-jersey","new-york-city", "norfolk","northeast-california", "pittsburgh","richmond", "south-east-florida", "upstate-new-york", "upstatesc","utah", "washington-dc-eastern-pa","western-virginia","wyoming-and-northern-colorado")
  vRegions <- c("new-jersey","boston")
  # "central-california-inland-empire"  ???
  for(r in 1:length(vRegions)){
    xmltreeLog$addTag("Region",  close=FALSE) 
    xmltreeLog$addTag("regnum", as.character(vRegions[r]))
    # dfInfo <- processCategories(xmltreeLog, vRegions[r], vMasterURL)  
    lTwo <- processCategories(xmltreeLog, vRegions[r], vMasterURL)  
    dfInfo <- lTwo$dfInfo
    vMasterURL <- lTwo$vMasterURL
    xmltreeLog$closeTag()
  }
  ##############################################################
  ### Save Master List of ALL URLs
  ##############################################################
  strCSVFullFileNa <- createLocalCSVFileNa("AllURLs", blnTimeStampI=TRUE, blnLaptop=TRUE)
  cat("\n", "*** vMasterURL final ***", length(vMasterURL),  sep=" ")
  write.csv(vMasterURL, strCSVFullFileNa, row.names=TRUE) 
  
  ##############################################################
  ### Close and save XML Log
  ##############################################################
  closeXMLTree(xmltreeLog)
  # strPath <- "C:/Users/p622403/Documents/Work/R/BBB/"
  saveXML(xmltreeLog, file=CreateLocalXMLFileNa(blnLaptop))
  print("*** processRegions - Complete ***")
}
############################################################################
### flowFiles()
############################################################################
flowFiles   <- function() {
  strBasePath <- "C:/Users/p622403/Documents/Work/R/BBB/"
  strTransferPath <- paste(strBasePath, "Transfer/", sep="")
  strDetailsPath <- paste(strBasePath, "Details/", sep="")
  strBaseFileBaseNa <- "bostonroofingcontractors_2015_08_11_120749"
  strBaseFileNa <- paste(strBaseFileBaseNa, ".csv", sep="")
  vBaseFullFileNa <- c(strDetailsPath, strBaseFileNa)
  strBaseFullFileNa <- paste(vBaseFullFileNa, collapse="")
  print("strBaseFullFileNa")
  print(strBaseFullFileNa)
  strFullTableName <- "prhbbb.bbtrv100_bbb_review"
  strSeqN <- 110 
  strFlowPrefix <- "BBB"
  # createSQLFileDrop(strPath, strBaseFileNa, strFullTableName, strSeqN) 
  write(createDropSQLFileContent(strFullTableName)  , createDropSQLFileNa(strTransferPath, strFlowPrefix,  strFullTableName, strSeqN, strBaseFileBaseNa))
  print("*** Drop File   *** ")
  print(createDropSQLFileContent(strFullTableName) )
  ###################################################################
  ### Create data.frame from this .csv file
  ###################################################################
  # dfIn <-  ReadTextFileIntoDataFrame(strBaseFullFileNa, 0 )
  dfIn <- read.csv(strBaseFullFileNa, header=FALSE, stringsAsFactors = FALSE, sep=",", na.strings = c("NA", "N/A"), blank.lines.skip=TRUE, skip = 0)
  print("*** flowFiles dfIn  *** ")
  print(nrow(dfIn))
  print(ncol(dfIn))
  ### Column Names For Upload into Aster
  vColNames1 <- c("IssueAdvSales", "IssueBilling","IssueDelivery","IssueWarranty","IssueProductService","ClosedComplaintTotal","ReviewPositive","ReviewNegative","ReviewNeutral","ReviewTotal")
  vColNames2 <- c("BBBRating", "LegalEnttyNa", "Street","City","State","Zip","Phone","Fax","Website","Longitude","Latitude")
  vColNames3 <- c("BusnStartDate", "naics","LicenseNumber")
  vColNames3A <- c("BBBCategories")
  vColNames4 <- c("BBBReviewURL", "BBBRegion","BBBCategory","BBBPage")
  vColNames5 <- c("item")
  vColNames <- c(vColNames1, vColNames2, vColNames3, vColNames3A, vColNames4, vColNames5)
  colnames(dfIn) <- tolower(vColNames)
  
  #######################################################################################################################
  ### Using data.frame create sql file for CREATE TABLE
  #######################################################################################################################
  strCreateDDLFullFileName <- createCreateSQLFileNa(strTransferPath, strBaseFileBaseNa, strFlowPrefix, strSeqN)
  print("xxx flowFiles dfIn xxxx")
  print(dfIn)
  strFirstColumnName <- DDLFromDataFrame(dfIn, strCreateDDLFullFileName, strFullTableName)  
  # write(strCreateDDLFullFileName, DDLFromDataFrame(dfIn, strBaseFullFileNa, strFullTableName)   )
  # write(paste("strFirstColumnName",strFirstColumnName) , strLogFileNa, append=TRUE)
  
  #######################################################################################################################
  ### create sql file for nClasterLoad
  #######################################################################################################################
  strCreatenClusterLoadFullFileName <- createnClusterLoadSQLFileNa(strTransferPath, strBaseFileBaseNa, strFlowPrefix, strSeqN)
  write(createnClusterLoadSQLFileContent(strFullTableName, strBaseFullFileNa)  , strCreatenClusterLoadFullFileName)
  
  write(createCountSQLFileContent(strFullTableName)  , createCountSQLFileNa(strTransferPath, strBaseFileNa, strFlowPrefix, strSeqN))
}
############################################################################
### createLoadFiles()
############################################################################
createLoadFiles   <- function() {
  strBasePathData <-  "//wtors003/isg-secure/ISGBIG~1/VENDOR~1/Data/BBB/"
  strBasePathSQL <- "//wtors003/isg-secure/ISGBIG~1/2013HA~1/POC-TE~1/SQL/BBB/"
  strFlowPrefix <- "BBB"
  strFullTableName <- "prhbbb.bbtrv100_bbb_review"
  #   strTransferPath <- paste(strBasePath, "Transfer/", sep="")
  #   strDetailsPath <- paste(strBasePath, "Details/", sep="")
  intSeqN <- 1005L
  vFileList <- listFullFilesInDirectory(strBasePathData, "*.csv")
  for (f in 1:length(vFileList)) {
    strBaseFileNa <- basename(as.character(vFileList[f]))
    strBaseFileBaseNa <- sub("^([^.]*).*", "\\1", strBaseFileNa)
    #     print(f)
    #     print(vFileList[f])
    #     print(strBaseFileNa)
    #     print(strBaseFileBaseNa)
    intSeqN <- intSeqN + 5 ; strSeqN <- formatC(intSeqN, width=4, flag="0")
    strCreatenClusterLoadFullFileName <- createnClusterLoadSQLFileNa(strBasePathSQL, strBaseFileBaseNa, strFlowPrefix, strSeqN)
    # print(strCreatenClusterLoadFullFileName)
    write(createnClusterLoadSQLFileContent(strFullTableName, vFileList[f]), strCreatenClusterLoadFullFileName) 
  }
}
############################################################################
### testVectorMatrixDataFrame()
### cbind example
############################################################################
testVectorMatrixDataFrame    <- function() {
  vNumbers <- c(1:5)
  print(class(vNumbers))
  vChar <- c("alpha", "beta","charlie", "delta", "epsilon")
  print(class(vChar))
  mtxDim <- matrix(c(vNumbers,vChar), nrow=5, ncol=2)
  print(mtxDim)
  
  # mtxVec <- matrix(nrow=5, ncol=2)
  mtxVec <- cbind(vNumbers,vChar)
  print(mtxVec)
  
  vColClasses <- c("numeric", "character")
  vColNames <- c("nums", "chars")
  
  # dfMixed <- as.data.frame(mtxVec, stringsAsFactors=FALSE, colClasses = vColClasses)
  #   print(str(dfMixed))
  #   lapply(dfMixed, class)
  
  # dfClasses <- data.frame(nrow=5, ncol=2, stringsAsFactors=FALSE, colClasses = vColClasses)
  # dfClasses <- data.frame(nrow=5, ncol=2, colClasses = vColClasses)
  dfClasses <- as.data.frame(matrix(0, ncol = 2, nrow = 5), colClasses = vColClasses)
  print(str(dfClasses))
  #    print(dfClasses)
  # dfClasses <- cbind(1:5,vChar)
  dfClasses[,1] <-vNumbers
  dfClasses[,2] <- vChar
  
  #    dfClasses[1,1] <- 1
  #    dfClasses[2,1] <- 2
  #    dfClasses[3,1] <- 3
  #    dfClasses[4,1] <- 4
  #    dfClasses[5,1] <- 5
  
  #    dfClasses[1,2] <- "alphaa"
  #    dfClasses[2,2] <- "alphab"
  #    dfClasses[3,2] <- "alphac"
  #    dfClasses[4,2] <- "alphad"
  #    dfClasses[5,2] <- "alphae"
  #   dfClasses <- data.frame(vNumbers, vChar )
  #   print("before")
  #   print(dfClasses)
  #   dfClasses <- cbind(vNumbers,vChar)
  colnames(dfClasses) <- tolower(vColNames)
  #   print("after")
  print(str(dfClasses))
  print(dfClasses)
  lapply(dfClasses, class)
}
############################################################################
### test2VectorElimination()
############################################################################
test2VectorElimination    <- function() {
  vMaster <- c("A", "B", "C", "D")
  vNewOne <- c("Q", "B", "E", "D","G")
  vNewOne <- vNewOne[!vNewOne %in% vMaster]
  print (vNewOne)
  for(strURL in vNewOne) {
    print(strURL)
  }
  strCSVFullFileNa <- createLocalCSVFileNa("VectorTest", blnTimeStampI=TRUE, blnLaptop=TRUE)
  write.table(vNewOne, strCSVFullFileNa, row.names=FALSE) 
}

