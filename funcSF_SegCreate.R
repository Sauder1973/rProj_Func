
#Segment Creation from SFDC Account Data
# January 3, 2018
# W Sauder
# Ceridian Corporation

segmentAcctLookup <- function(sf_All_Accounts) {
     
     
     #Determine Segment
     
     sf_All_Accounts$Segment <- ifelse(is.na(sf_All_Accounts$Segment_f__c),sf_All_Accounts$Segment_f2__c, sf_All_Accounts$Segment_f__c)
     
     # table(sf_All_Accounts$Segment[sf_All_Accounts$ShippingCountry == "Canada" | sf_All_Accounts$ShippingCountry =="United States"], 
     #       sf_All_Accounts$ShippingCountry[sf_All_Accounts$ShippingCountry == "Canada" | sf_All_Accounts$ShippingCountry =="United States"])
     # 
     # table(sf_All_Accounts$RPO_Owner_Department__c[sf_All_Accounts$ShippingCountry == "Canada" | sf_All_Accounts$ShippingCountry =="United States"],
     #       sf_All_Accounts$Segment[sf_All_Accounts$ShippingCountry == "Canada" | sf_All_Accounts$ShippingCountry =="United States"], 
     #       sf_All_Accounts$ShippingCountry[sf_All_Accounts$ShippingCountry == "Canada" | sf_All_Accounts$ShippingCountry =="United States"] 
     # )
     
     
     #Flag Probable SMALL BUSINESS Accounts based on Segment and RPO_Owner_Department__c
     
     
     sf_All_Accounts$Is_SB <- sf_All_Accounts$RPO_Owner_Department__c == "Small Business" | sf_All_Accounts$Segment == "Small Business"
     
     # table(sf_All_Accounts$SmallBusiness)     
     
     sf_All_Accounts$SegmentSimple <- clean.text(sf_All_Accounts$Segment, lowercase = FALSE)
     
     sf_All_Accounts$DFText <- paste("DF_",sf_All_Accounts$SegmentSimple, sep = "")
     sf_All_Accounts$RegText <- sf_All_Accounts$SegmentSimple
     
     #Convert Has_Dayforce to logical (TRUE or FALSE)
     
     sf_All_Accounts$Has_Dayforce__c <- ifelse(sf_All_Accounts$Has_Dayforce__c == "true",TRUE,FALSE)
     # table(sf_All_Accounts$Has_Dayforce__c)
     
     # Change the simple segment to DF if DF Found
     sf_All_Accounts$DFSegmentSimple <- ifelse(sf_All_Accounts$Has_Dayforce__c, sf_All_Accounts$DFText,sf_All_Accounts$SegmentSimple)
     sf_All_Accounts$DFText <- NULL
     sf_All_Accounts$RegSegmentSimple <- ifelse(sf_All_Accounts$Has_Dayforce__c, sf_All_Accounts$RegText,sf_All_Accounts$SegmentSimple)
     sf_All_Accounts$RegText <- NULL
     
     sf_All_Accounts$Segment<- trimws(sf_All_Accounts$Segment, which = c("both"))
     sf_All_Accounts$DFSegmentSimple<- trimws(sf_All_Accounts$DFSegmentSimple, which = c("both"))
     sf_All_Accounts$RegSegmentSimple<- trimws(sf_All_Accounts$RegSegmentSimple, which = c("both"))
     
     accToSegLookUp <- sf_All_Accounts[,.(Id,Segment,DFSegmentSimple, RegSegmentSimple, Has_Dayforce__c, Is_SB)]
     rownames(accToSegLookUp) <- accToSegLookUp$Id
     setkey(accToSegLookUp,Id)
     
     return (accToSegLookUp)
}