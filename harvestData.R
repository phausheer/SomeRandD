########################################################################################################################################################
### C:\Users\p622403\Documents\Work\R\BBB
### harvestMapAddresses.R
### July 2015 
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
  library(ggmap)
  library(RColorBrewer)
  library(dplyr)
  library(stringr)
  library(RCurl)
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
  # strURL <- "http://www.bbb.org/boston/business-reviews/insurance-companies/brooklawn-insurance-agency-inc-in-new-bedford-ma-62884/customer-reviews/"
  strURL <- "http://www.bbb.org/nebraska/business-reviews/fire-and-water-damage-restoration/paul-davis-restoration-in-columbus-ne-300033282"
  
  webpage <- getURL(strURL)
  # print(webpage)
  # Process escape characters
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  saveToALocalFile(webpage)
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
### saveToALocalFile
############################################################################
saveToALocalFile <- function(strContent) {
  strLogFileNa <- "C:/Users/p622403/Documents/Work/R/BBB/Webpage_"
  # strLogFileNa <- "C:/Users/Hausheer/Documents/Work/R/BBB/Webpage_"
  strLogFileNa <- paste(strLogFileNa,gsub(":","",Sys.time()),sep='')
  strLogFileNa <- gsub("-","_",strLogFileNa)
  strLogFileNa <- gsub(" ","_",strLogFileNa)
  strLogFileNa <- paste(strLogFileNa, ".htm", sep="")
  print(strLogFileNa)
  fileConn<-file(strLogFileNa)
  writeLines(strContent, fileConn)
  close(fileConn)
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
### extractTheNames
############################################################################
extractTheNames  <- function(url) {
  ### These 2 lines are for DEBUG, stand alone processing
  strSingleURL <- "http://www.bbb.org/boston/business-reviews/painting-contractors/lifetyme-exteriors-llc-in-needham-hgts-ma-95426"
  url  <- parseThisWebsite(strSingleURL)
  print("*** url done *** ")
  
  
  # Pull out the names of the wineries and breweries
  # selector_name<-".pageListingHeader"
  selector_complaint_label <- ".complaint-table .even,.odd td:first-child"
  selector_complaint_table <- ".complaint-table .even,.odd td"
#   fnames <- html_nodes(url, selector_name) %>%
#     html_text()
  vComplaintLabels <- html_nodes(url, selector_complaint_table) %>%
    html_text()
  print("*** vComplaintLabels *** ")
  print(class(vComplaintLabels))
  print(length(vComplaintLabels))
  print(vComplaintLabels)
  return(vComplaintLabels)
}
############################################################################
### extractTDFromFirst2Tables
############################################################################
extractTDFromFirst2Tables  <- function(url) {
  ### These 2 lines are for DEBUG, stand alone processing
#     strSingleURL <- "http://www.bbb.org/greater-maryland/business-reviews/fire-and-water-damage-restoration/water-mold-and-fire-baltimore-in-baltimore-md-90261344"
#     # Easy One : strSingleURL <- "http://www.bbb.org/dallas/business-reviews/fire-and-water-damage-restoration/puroclean-restoration-specialists-in-frisco-tx-90247270"
#     url  <- parseThisWebsite(strSingleURL)
#     print("*** url done *** ")
  
  selector_complaint_table <- ".complaint-table td"
  # selector_complaint_table <- ".complaint-table .even,.odd td"
  #   fnames <- html_nodes(url, selector_name) %>%
  #     html_text()
  vTDs <- html_nodes(url, selector_complaint_table) %>%  html_text()
#   print("*** vTDs ***")
#   print(length(vTDs))
  if (length(vTDs)>0) {
    df2 <- cleanExtractedHTMLText(vTDs)
  }
  else {
    ### Write to the XMLLogFile this error condition
    df2 <- data.frame(Labels = character(0), stringsAsFactors = FALSE)
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
  vTDClean <-  str_trim(vTDClean)
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
### extractStreetAddresses
############################################################################
extractStreetAddresses  <- function(url) {
#   vStreet <- url %>%  html_nodes(xpath="//meta[@itemprop='streetAddress']") %>%
#     html_attr("content")
  
  streetNode <- url %>%  html_nodes(xpath="//meta[@itemprop='streetAddress']") 
  print("*** streetNode *** ")
  print(class(streetNode))
  
  streetAddr <-   html_attr(streetNode, "content")
#   print("*** streetAddr *** ")
#   print(class(streetAddr))
#   print(length(streetAddr))
#   print(streetAddr)
#   print("*** vStreet *** ")
#   print(class(vStreet))
#   print(vStreet)
  
  return(streetAddr)
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
############################################################################
extractBusnCategories   <- function(url) {
  library(stringr)
  ### These 2 lines are for DEBUG, stand alone processing
  # strSingleURL <- "http://www.bbb.org/boston/business-reviews/roofing-contractors/a-roofing-llc-in-williston-vt-121527"
  strSingleURL <- "http://www.bbb.org/nebraska/business-reviews/fire-and-water-damage-restoration/paul-davis-restoration-in-columbus-ne-300033282"
  #   strSingleURL <- "http://www.bbb.org/boston/business-reviews/insurance-companies/brooklawn-insurance-agency-inc-in-new-bedford-ma-62884/customer-reviews/"
  url  <- parseThisWebsite(strSingleURL)
  # print(html_text(url))
  # print("*** URL DONE ***")
  
  #   
  vBusnCategories <- c("BusnCategories")
  vCategories <- url %>%  html_nodes(xpath="//h5[.='Business Category']/following-sibling::*[1]/span") %>% html_text()
  if (length(vCategories)==0) {
    print("*** CSText  *** ")
    # nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Management']/following-sibling::*[1]") 
    # nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Management']/following-sibling::*[name()='span']") 
    nodePrincipal <- url %>%  html_nodes(xpath="//h5[.='Business Category']/following-sibling::*[1][name()='p']") 
    strCategories <- html_text(nodePrincipal)
    print(strCategories)
    vCategories <- strsplit(strCategories, ",")[[1]]
    print(length(vCategories))
    print(vCategories)
  }
  if (length(vCategories)>0) {
    vBusnCategories <- gsub("\r\n", "", vCategories, fixed = TRUE)
    vBusnCategories <-  str_trim(vBusnCategories)
    # nodeNext <- html_nodes(nodeAltBusnNames, xpath="//h5[.='Alternate Business Names']")
    print("*** vAltBusnNames  *** ")
    print(class(vBusnCategories))
    print(length(vBusnCategories))
  }
  print(vBusnCategories)
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
### createLocalExcelFileNa
############################################################################
createLocalExcelFileNa <- function(strBaseFileNa) {
  strExcelFileNa <- "C:/Users/p622403/Documents/Work/R/BBB/Details/"
  strExcelFileNa <- paste(strExcelFileNa, strBaseFileNa, sep='')
  strExcelFileNa <- paste(strExcelFileNa, "_", sep='')
  strExcelFileNa <- paste(strExcelFileNa,gsub(":","",Sys.time()),sep='')
  strExcelFileNa <- gsub("-","_",strExcelFileNa)
  strExcelFileNa <- gsub(" ","_",strExcelFileNa)
  strExcelFileNa <- paste(strExcelFileNa, ".xls", sep="")
  return(strExcelFileNa)
  # print(strExcelFileNa)
}
createLocalCSVFileNa <- function(strBaseFileNa, blnTimeStampI) {
  strCSVFileNa <- "C:/Users/p622403/Documents/Work/R/BBB/Details/"
  strCSVFileNa <- paste(strCSVFileNa, strBaseFileNa, sep='')
  if (blnTimeStampI) {
    strCSVFileNa <- paste(strCSVFileNa, "_", sep='')
    strCSVFileNa <- paste(strCSVFileNa,gsub(":","",Sys.time()),sep='')
  }
  strCSVFileNa <- gsub("-","_",strCSVFileNa)
  strCSVFileNa <- gsub(" ","_",strCSVFileNa)
  strCSVFileNa <- paste(strCSVFileNa, ".csv", sep="")
  return(strCSVFileNa)
  # print(strExcelFileNa)
}
############################################################################
### harvestListOfNJPainters
############################################################################
harvestListOfNJPainters  <- function() {
  print("*** harvestListOfNJPainters - Started ***")
  ### Retrieve the HTML into an XML Dom
  ### New Jesey
  strURL <- "http://www.bbb.org/new-jersey/accredited-business-guide/painting-contractors/299/"
  xmlDOMWS <- parseThisWebsite(strURL)
  vURL4s <- extractListOfURLs(xmlDOMWS)
  print(paste("Length dfURL4s: ", length(vURL4s)))
  
  dfURL4s <- as.data.frame(vURL4s,stringsAsFactors=FALSE)
  vColNames <- c("bbbReviewURL")
  names(dfURL4s) <- vColNames
  print(paste("Length dfURL4s: ", nrow(dfURL4s)))
  # print(head(dfURL4s))
  # print(dfURL4s[30:42,])
  
  dfURLs3 <- filter(dfURL4s, grepl("business-reviews", bbbReviewURL))
  print(paste("Length dfURLs3: ", nrow(dfURLs3)))
  
  dfURLs2 <- filter(dfURLs3, !grepl("/quote/", bbbReviewURL))
  print(paste("Length dfURLs2: ", nrow(dfURLs2)))
  dfURLs2$bbbReviewURL <-  paste("http://www.bbb.org",dfURLs2$bbbReviewURL, sep="")
  
  dfURLs1 <-unique(dfURLs2)
  print(paste("Length dfURLs1: ", nrow(dfURLs1)))
  #   intLthdfURLs <- nrow(dfURLs)
  #   print(paste("intLthdfURL2s: ",intLthdfURL2s))
  
  ##############################################################
  ### Save List as Excel File 
  ##############################################################
  # SaveDataFrameAsExcel(dfURLs2, createLocalExcelFileNa("harvestListOfMAPainters")) 
  
  print("*** harvestListOfNJPainters - Complete ***")
  return(dfURLs1)
}
############################################################################
### harvestListOfSSCategory
############################################################################
harvestListOfSSCategory  <- function(strURL, xmltreeLog) {
  print("*** harvestListOfSSCategory - Started ***")
  xmltreeLog$addTag("harvestListOfSSCategory", close=FALSE)
  xmltreeLog$addTag("strListURL", strURL) 
  ### Retrieve the HTML into an XML Dom
  ### New Jesey
  print(strURL)
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
  }
  ##############################################################
  ### Save List as Excel File 
  ##############################################################
  # SaveDataFrameAsExcel(dfURLs2, createLocalExcelFileNa("harvestListOfMAPainters")) 
  xmltreeLog$closeTag() 
  print("*** harvestListOfSSCategory - Complete ***")
  return(dfURLs1)
}
############################################################################
### harvestListOfTulsaPainters
############################################################################
harvestListOfTulsaPainters  <- function() {
  print("*** harvestListOfTulsaPainters - Started ***")
  ### Retrieve the HTML into an XML Dom
  ### New Jesey
  # strURL <- "http://www.bbb.org/new-jersey/accredited-business-guide/painting-contractors/299/"
  ### Oklahoma. Tulsa
  strURL <- "http://www.bbb.org/tulsa/accredited-business-directory/painting-contractors#"
  xmlDOMWS <- parseThisWebsite(strURL)
  vURL4s <- extractListOfURLs(xmlDOMWS)
  print(paste("Length dfURL4s: ", length(vURL4s)))
  
  dfURL4s <- as.data.frame(vURL4s,stringsAsFactors=FALSE)
  vColNames <- c("bbbReviewURL")
  names(dfURL4s) <- vColNames
  print(paste("Length dfURL4s: ", nrow(dfURL4s)))
  # print(head(dfURL4s))
  # print(dfURL4s[100:112,])
  
  dfURLs3 <- filter(dfURL4s, grepl("business-reviews", bbbReviewURL))
  print(paste("Length dfURLs3: ", nrow(dfURLs3)))
#   dfURLs <- filter(dfURLs3, !grepl("/quote/", bbbReviewURL))
#   dfURLs$bbbReviewURL <-  paste("http://www.bbb.org",dfURLs2$bbbReviewURL, sep="")
#   intLthdfURLs <- nrow(dfURLs)
#   print(paste("intLthdfURL2s: ",intLthdfURL2s))

  ##############################################################
  ### Save List as Excel File 
  ##############################################################
  # SaveDataFrameAsExcel(dfURLs3, createLocalExcelFileNa()) 
  
  print("*** harvestListOfTulsaPainters - Complete ***")
  return(dfURLs3)
}
############################################################################
### harvestListOfMAPainters
############################################################################
harvestListOfMAPainters  <- function() {
  print("*** harvestListOfMAPainters - Started ***")
  ### MA, ME, VT, RI, page 1 of 4
  # strURL <- "http://www.bbb.org/boston/accredited-business-guide/painting-contractors/437/"
  ### MA, ME, VT, RI, page 3 of 4
  strURL <- "http://www.bbb.org/boston/accredited-business-guide/painting-contractors/437?page=3"
  xmlDOMWS <- parseThisWebsite(strURL)
  vURL4s <- extractListOfURLs(xmlDOMWS)
  print(paste("Length dfURL4s: ", length(vURL4s)))
  
  dfURL4s <- as.data.frame(vURL4s,stringsAsFactors=FALSE)
  vColNames <- c("bbbReviewURL")
  names(dfURL4s) <- vColNames
  print(paste("Length dfURL4s: ", nrow(dfURL4s)))
  # print(head(dfURL4s))
   # print(dfURL4s[30:42,])
  
  dfURLs3 <- filter(dfURL4s, grepl("business-reviews", bbbReviewURL))
  print(paste("Length dfURLs3: ", nrow(dfURLs3)))
  
  dfURLs2 <- filter(dfURLs3, !grepl("/quote/", bbbReviewURL))
  print(paste("Length dfURLs2: ", nrow(dfURLs2)))
  dfURLs2$bbbReviewURL <-  paste("http://www.bbb.org",dfURLs2$bbbReviewURL, sep="")
  
  dfURLs1 <-unique(dfURLs2)
  print(paste("Length dfURLs1: ", nrow(dfURLs1)))
  #   intLthdfURLs <- nrow(dfURLs)
  #   print(paste("intLthdfURL2s: ",intLthdfURL2s))
  
  ##############################################################
  ### Save List as Excel File 
  ##############################################################
  # SaveDataFrameAsExcel(dfURLs2, createLocalExcelFileNa("harvestListOfMAPainters")) 
  
  print("*** harvestListOfMAPainters - Complete ***")
  return(dfURLs1)
}
############################################################################
### harvestDetailsOfPainters
############################################################################
harvestDetailsOfPainters  <- function() {
  ### Initialize
  getHarvestPackages()
  
  # dfURLs <- harvestListOfPainters()
  dfURLs <- harvestListOfTulsaPainters()
  
  colClasses = c(rep("character",16))
  #   vColNames  = c("Labels", "Vals")
  df4 <- data.frame(colClasses, stringsAsFactors = FALSE)
  
  # strSingleURL <- dfURLs[3,1]
  for(i in 1:nrow(dfURLs)){
    strSingleURL <- dfURLs[i,1]
    # print(paste("Single URL:", strSingleURL))
    url  <- parseThisWebsite(strSingleURL)
    #   
    # dfInfo <- harvestTheNamesAndAddresses(url) 
    dfComplaints <- as.data.frame(extractTDFromFirst2Tables(url))
    dfInfo <- dfComplaints
    dfInfo[nrow(dfInfo) +1,] <- c("BBBURL",strSingleURL)
    # strVariable <- "streetAddress"
    vVariables <- c("name","streetAddress","addressLocality","addressRegion","postalCode","telephone","latitude","longitude","foundingDate")
    for(s in 1:length(vVariables)){
      dfInfo <- extractElementAddToDF(url, vVariables[s], dfInfo)  
    }
    # print("*** dfInfo *** ")
    # print(dfInfo) 
    #   
    dfRow <- data.frame(t(dfInfo))
    dfRow2 <- dfRow[2,]
    ##############################################################
    ### Save List as Excel File 
    ##############################################################
    SaveDataFrameAsExcel(dfRow2, createLocalExcelFileNa("SingleTulsaPainter"))
    #     print("*** dfRow2 *** ")
    #     print(str(dfRow2))
    if(i==1) {
      df4 <- dfRow2
      print(str(df4))
      # print(df4)
    }
    else {
      # df4[nrow(df4)+1,] <- dfRow2[1,]
      df4 <- rbind(df4, dfRow2[1,])
      # print(str(df4))
    }
  }
#   print("*** df4 ***")
#   print(str(df4))
  ##############################################################
  ### Save List as Excel File 
  ##############################################################
  SaveDataFrameAsExcel(df4, createLocalExcelFileNa("TulsaPainters")) 
  
  print("*** harvestDetailsOfPainters - Complete ***")
}
############################################################################
### harvestDetailsOfXXPainters
############################################################################
harvestDetailsOfXXPainters  <- function() {
  ### Initialize
  getHarvestPackages()
  xmltreeLog <- startXMLTree()
  
  # strURL <- "http://www.bbb.org/boston/accredited-business-guide/painting-contractors/437/"
  ### MA, ME, VT, RI, page 3 of 4
  # strURL <- "http://www.bbb.org/boston/accredited-business-guide/painting-contractors/437?page=3"
  strListURL <- "http://www.bbb.org/new-jersey/accredited-business-directory/siding-contractors"
  dfURLs <- harvestListOfSSCategory(strListURL, xmltreeLog)
  
  ### define empty Data.frame
  vVariables <- c("legalName","streetAddress","addressLocality","addressRegion","postalCode","telephone","faxNumber","url","latitude","longitude","foundingDate","naics")
  colClasses = c(rep("character",length(vVariables)))
  #   vColNames  = c("Labels", "Vals")
  df4 <- data.frame(colClasses, stringsAsFactors = FALSE)
  
  # strSingleURL <- dfURLs[3,1]
  for(i in 1:nrow(dfURLs)){
  # for(i in 1:13){
    print(paste("processing harvestDetailsOfXXPainters number: ", i))
    strSingleURL <- dfURLs[i,1]
    # print(paste("Single URL:", strSingleURL))
    url  <- parseThisWebsite(strSingleURL)
    #   
    # dfInfo <- harvestTheNamesAndAddresses(url) 
    dfComplaints <- as.data.frame(extractTDFromFirst2Tables(url))
    dfInfo <- dfComplaints
    dfInfo <- rbind(dfInfo, c("BBBRating",extractBBBRating(url)))
    # print(paste("nrow(dfInfo):", nrow(dfInfo)))
    if(nrow(dfInfo)==0) {
      dfInfo <- data.frame(Labels = character(0), Vals = character(0), stringsAsFactors = FALSE)
      # dfInfo <- rbind(dfInfo, c("BBBURL",strSingleURL))
      # print(str(dfInfo))
      # print(df4)
    }
    #     else {
    #       dfInfo[nrow(dfInfo) +1,] <- c("BBBURL",strSingleURL)
    #     }
    # strVariable <- "streetAddress"
    
    for(s in 1:length(vVariables)){
      dfInfo <- extractContentAttrAddToDF(url, vVariables[s], dfInfo)  
    }
    dfInfo <- rbind(dfInfo, c("LicenseNumber",extractLicenseNumber(url)))
    dfInfo <- rbind(dfInfo, c("BBBURL",strSingleURL))
    # print("*** dfInfo *** ")
    # print(dfInfo) 
    #   
    dfRow <- data.frame(t(dfInfo))
    dfRow2 <- dfRow[2,]
    ##############################################################
    ### Save List as Excel File 
    ##############################################################
    # SaveDataFrameAsExcel(dfRow2, createLocalExcelFileNa("SingleMAPainter"))
    #     print("*** dfRow2 *** ")
    #     print(str(dfRow2))
    if(i==1) {
      df4 <- dfRow2
      # print(str(df4))
      # print(df4)
    }
    else {
      # df4[nrow(df4)+1,] <- dfRow2[1,]
      #       print("*** rbind ***")
      #       print(nrow(dfRow2))
      #       print(ncol(dfRow2))
      #       print(dfRow2)
      if ((nrow(dfRow2)>0) && (ncol(dfRow2)==ncol(df4))) {
        #         print("*** df4 ***")
        #         print(nrow(df4))
        #         print(ncol(df4))
        df4 <- rbind(df4, dfRow2[1,])
      }
      # print(str(df4))
    }
  }
  #   print("*** df4 ***")
  #   print(str(df4))
  ##############################################################
  ### Save List as Excel File 
  ##############################################################
  SaveDataFrameAsExcel(df4, createLocalExcelFileNa("harvestDetailsOfNJsiding_P1of3")) 
  
  ##############################################################
  ### Close and save XML Log
  ##############################################################
  closeXMLTree(xmltreeLog)
  # strPath <- "C:/Users/p622403/Documents/Work/R/BBB/"
  saveXML(xmltreeLog, file=CreateLocalXMLFileNa())
  
  
  print("*** harvestDetailsOfMAPainters - Complete ***")
}
############################################################################
### useListExtractDetails
############################################################################
useListExtractDetails  <- function(xmltreeLog, strListURL, strRegion, strCategory, strPage) {
  print("*** useListExtractDetails - Started ***")
  
  dfURLs <- harvestListOfSSCategory(strListURL, xmltreeLog)
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
  df4 <- data.frame(colClasses, stringsAsFactors = FALSE)
  intTopRow <- min(nrow(dfURLs), 27)
  print("*** intTopRow ***")
  print(intTopRow)
  if (intTopRow>0) {
    # for(i in 1:nrow(dfURLs)){
    for(i in 1:intTopRow){
      print(paste("extracting Details number: ", i))
      strSingleURL <- dfURLs[i,1]
      print(paste("Single URL:", strSingleURL))
      url  <- parseThisWebsite(strSingleURL)
      #   
      # dfInfo <- harvestTheNamesAndAddresses(url) 
      dfComplaints <- as.data.frame(extractTDFromFirst2Tables(url))
      # print("dfComplaints complete")
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
      dfInfo <- rbind(dfInfo, c("BBBURL",strSingleURL))
      dfInfo <- rbind(dfInfo, c("BBBRegion", strRegion))
      dfInfo <- rbind(dfInfo, c("BBBCategory", strCategory))
      dfInfo <- rbind(dfInfo, c("BBBPage", strPage))
      # print("*** dfInfo *** ")
      # print(dfInfo) 
      
      ##############################################################
      ### Transpose Name-Value pair dataframe to 1 Long row dataframe   
      ##############################################################
      dfRow <- data.frame(t(dfInfo))
      dfRow2 <- dfRow[2,]
      
      ##############################################################
      ### Start New dataframe, or append new row to existing dataframe
      ##############################################################
      if(i==1) {
        df4 <- dfRow2
      }
      else {
        if ((nrow(dfRow2)>0) && (ncol(dfRow2)==ncol(df4))) {
          df4 <- rbind(df4, dfRow2[1,])
        }
        # print(str(df4))
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
  return(df4)
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
processPages   <- function(xmltreeLog, strRegion, strCategory) {
  print("*** processPages - Started ***")
  vListURL <- c("http://www.bbb.org/", strRegion, "/accredited-business-directory/", strCategory, "?page=1")
  strListURL <- paste(vListURL,  collapse="")
  # print(paste("strListURL", strListURL))
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
    
    dfPage <- useListExtractDetails(xmltreeLog, strListURL, strRegion, strCategory, strPage)
    if (p==1) {
      dfPages <- dfPage
    }
    else {
      if ((nrow(dfPage)>0) && (ncol(dfPage)==ncol(dfPages))) {
        dfPages <- rbind(dfPages, dfPage)
        print(paste("nrow(dfPages):",nrow(dfPages)))
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
  strFullFileNa <- createLocalCSVFileNa(strBaseName, FALSE)
  # print(strFullFileNa)
  xmltreeLog$addTag("strFullFileNa", strFullFileNa) 
  # SaveDataFrameAsExcel(dfPages, strFullFileNa) 
  SaveDataFrameAsCSV(dfPages, strFullFileNa, FALSE)
  
  print("*** processPages - Complete ***")
}
############################################################################
### processCategories
############################################################################
processCategories   <- function(xmltreeLog, strRegion) {
  print("*** processCategories - Started ***")
  ### , "Smoke Odor Counteracting Service" does not exist in Central Mass  
  # vCategories <- c("contractors-flooring", "fire-water-damage-restoration", "insurance-fire-flood-specialists","lawn-tree-care", "mold-and-mildew-inspection", "painting-contractors", "siding-contractors","smoke-odor-counteracting-service", "tree-service", "Water-Heaters-Repairing","windows")
  # vCategories <- c("mold-and-mildew-inspection", "mold-consulting-and-testing")
  vCategories <- vectorFromExcelFile()
  for(c in 1:length(vCategories)){
    xmltreeLog$addTag("Category", close=FALSE) 
    xmltreeLog$addTag("catnum", as.character(vCategories[c]))
    dfInfo <- processPages(xmltreeLog, strRegion, vCategories[c])  
    xmltreeLog$closeTag()
  }
  print("*** processCategories - Complete ***")
}
############################################################################
### processRegions
############################################################################
processRegions   <- function() {
  print("*** processRegions - Started ***")
  ### Initialize
  getHarvestPackages()
  xmltreeLog <<- startXMLTree()
  
  # vRegions <- c("boston","central-texas", "central-western-massachusetts","chicago", "columbia","dallas", "eastern-washington", "greater-maryland","myrtle-beach", "new-jersey","new-york-city", "norfolk","northeast-california", "pittsburgh","richmond", "south-east-florida", "upstate-new-york", "upstatesc","washington-dc-eastern-pa","western-virginia")
  vRegions <- c("eastern-oklahoma","central-oklahoma","nebraska","utah","wyoming-and-northern-colorado")
  # "central-california-inland-empire"  ???
  for(r in 1:length(vRegions)){
    xmltreeLog$addTag("Region",  close=FALSE) 
    xmltreeLog$addTag("regnum", as.character(vRegions[r]))
    dfInfo <- processCategories(xmltreeLog, vRegions[r])  
    xmltreeLog$closeTag()
  }
  
  ##############################################################
  ### Close and save XML Log
  ##############################################################
  closeXMLTree(xmltreeLog)
  # strPath <- "C:/Users/p622403/Documents/Work/R/BBB/"
  saveXML(xmltreeLog, file=CreateLocalXMLFileNa())
  print("*** processRegions - Complete ***")
}
############################################################################
### flowFiles()
############################################################################
flowFiles   <- function() {
  strBasePath <- "C:/Users/p622403/Documents/Work/R/BBB/"
  strTransferPath <- paste(strBasePath, "Transfer/", sep="")
  strDetailsPath <- paste(strBasePath, "Details/", sep="")
  strBaseFileBaseNa <- "boston_sidingcontractors_page7_2015_07_23_183228"
  strBaseFileNa <- paste(strBaseFileBaseNa, ".csv", sep="")
  vBaseFullFileNa <- c(strDetailsPath, strBaseFileNa)
  strBaseFullFileNa <- paste(vBaseFullFileNa, collapse="")
  strFullTableName <- "prhbbb.bbtrv100_bbb_review"
  strSeqN <- 100 
  strFlowPrefix <- "BBB"
  # createSQLFileDrop(strPath, strBaseFileNa, strFullTableName, strSeqN) 
  write(createDropSQLFileContent(strFullTableName)  , createDropSQLFileNa(strTransferPath, strFlowPrefix,  strFullTableName, strSeqN, strBaseFileBaseNa))
  print("*** Drop File   *** ")
  print(createDropSQLFileContent(strFullTableName) )
  ###################################################################
  ### Create data.frame from this .csv file
  ###################################################################
  # dfIn <-  ReadTextFileIntoDataFrame(strBaseFullFileNa, 0 )
  dfIn <- read.csv(strBaseFullFileNa, header=TRUE, stringsAsFactors = FALSE, sep=",", na.strings = c("NA", "N/A"), blank.lines.skip=TRUE, skip = 0)
  print("*** dfIn  *** ")
  print(nrow(dfIn))
  print(ncol(dfIn))
  ### Column Names For Upload into Aster
  vColNames1 <- c("IssueAdvSales", "IssueBilling","IssueDelivery","IssueWarranty","IssueProductService","ClosedComplaintTotal","ReviewPositive","ReviewNegative","ReviewNeutral","ReviewTotal")
  vColNames2 <- c("BBBRating", "LegalEnttyNa", "Street","City","State","Zip","Phone","Fax","Website","Longitude","Latitude")
  vColNames3 <- c("BusnStartDate", "naics","LicenseNumber")
  vColNames4 <- c("BBBReviewURL", "BBBRegion","BBBCategory","BBBPage")
  vColNames <- c(vColNames1, vColNames2, vColNames3, vColNames4)
  colnames(dfIn) <- tolower(vColNames)
  
  #######################################################################################################################
  ### Using data.frame create sql file for CREATE TABLE
  #######################################################################################################################
  strCreateDDLFullFileName <- createCreateSQLFileNa(strTransferPath, strBaseFileBaseNa, strFlowPrefix, strSeqN)
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
  intSeqN <- 900L
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
  
