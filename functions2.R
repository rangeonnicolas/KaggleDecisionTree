transformations.ETL <- function(dataframe,metadata,write.csv=TRUE,filename="test.csv"){

	# Dans cette fonction sont regroupées toutes le modifications faites à l'ensemble 
	# d'apprentissage "learn", afin qu'elles soient appliquables aussi à l'ensemble de test

			to.transform <- c("ADTIND", "ADTOCC", "SEOTR", "VETYN", "YEAR")
			for(i in to.transform)
				dataframe[i] <- factor(unlist(dataframe[i]))
		
			for(VAR in unlist(metadata["VAR_ID"])){
				if(!is.na(replace <- metadata[VAR,"STR_LACKOFINFO_1"]))
					dataframe[which(dataframe[VAR] == replace),VAR] <- NA
				if(!is.na(replace <- metadata[VAR,"STR_LACKOFINFO_2"]))
					dataframe[which(dataframe[VAR] == replace),VAR] <- NA	
			}

			edu.corresp <- setNames(0:16 , c(" Children"  ," Less than 1st grade"," 1st 2nd 3rd or 4th grade" ," 5th or 6th grade"                      ," 7th and 8th grade"                     ," 9th grade"," 10th grade"                            ," 11th grade"                            ," 12th grade no diploma"                 ," High school graduate"          ," Some college but no degree" ," Associates degree-occup /vocational"   ," Associates degree-academic program"," Bachelors degree(BA AB BS)" ," Masters degree(MA MS MEng MEd MSW MBA)"," Prof school degree (MD DDS DVM LLB JD)"," Doctorate degree(PhD EdD)" ))
			EDUC <- levels(dataframe$AHGA)[dataframe$AHGA]
			dataframe$EDUC <- edu.corresp[EDUC]

 	 		dataframe$GRIN_ST_REG <- factor(paste0(dataframe$GRINST,"--",dataframe$GRINREG))
 	
 		 	dataframe$HDD_REL_FMX <- factor(paste0(dataframe$HHDFMX,"--",dataframe$HHDREL))
 
 		 	dataframe$A_REORGN_RACE <- factor(paste0(dataframe$AREORGN,"--",dataframe$ARACE))
 	
 	 	 	dataframe$EMPLOY <- factor(paste0(dataframe$AWKSTAT,"--",dataframe$AUNTYPE))
 
			dataframe_cleaned <- dataframe[which(metadata$USED == "yes")]

			write.csv(dataframe_cleaned,file.path("data","CLEANED",filename))

			return(dataframe_cleaned)
}