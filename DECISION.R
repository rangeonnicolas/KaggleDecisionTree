
##############################
#                            #
#   CHARGEMENT DES DONNEES   #
#                            #
##############################

		library(rpart)
		if((! "learn_cleaned" %in% ls() )  |  (! "metadata" %in% ls() ))
			source("ETL.R")

		source("functions2.R")
	# Chargement et transformation de l'échantillon de test
		test         <- read.csv(file.path(getwd(),"data","census_income_test.csv"),header=FALSE, sep=",")
		names(test)  <- subset(metadata$VAR_ID,metadata$ORIGIN=="original")
		test_cleaned <- transformations.ETL(test,metadata)


#################################################################################
#                                                                               #
#   DETERMINATION DE L'ARBRE DE DECISION GRACE A L'ECHATILLON D'APPRENTISSAGE   #
#                                                                               #
#################################################################################

	# determination de l'arbre
		arbre <- rpart(PEARNVAL ~  AAGE + ACLSWKR + ADTIND + ADTOCC + AHSCOL        + AMARITL   +    AMJIND      +  AMJOCC       + ASEX          + AUNMEM    +    CAPGAIN     +  CAPLOSS      + DIVVAL        + FILESTAT  +    MIGMTR1     +  MIGMTR3      + MIGMTR4       + MIGSUN    +    NOEMP       +  PARENT       + PEFNTVTY      + PEMNTVTY  +    PENATVTY    +  PRCITSHP     + SEOTR         + VETQVA    +    VETYN       +  WKSWORK            + EDUC      +    GRIN_ST_REG +  HDD_REL_FMX  + EMPLOY ,
					data=learn_cleaned, 
					control=rpart.control(minsplit=10))

	# ouverture d'une nouvelle fenêtre
		dev.new()
	# affichage de l'arbre de décision
		plot(arbre, compress=TRUE)
		text(arbre, use.n=TRUE)


#! Le premier essai n'est pas très concluant, on reessaie en 
#! ajoutant des poids aux lignes de l'ensemble d'apprentissage

	# calcul du ratio nb_-50ke / nb_+50ke
		ratio <- sum(learn_cleaned$PEARNVAL == " - 50000.") / sum(learn_cleaned$PEARNVAL == " 50000+.")

	# affectation des poids 1 pour les individus <50k€ ,
	# et du poids 15.11396 pour les individus à >50k€
		learn_cleaned$POIDS = 1
		learn_cleaned$POIDS[which(learn_cleaned$PEARNVAL==" 50000+.")] <- ratio

	# determination de l'arbre
		arbre2 <- rpart(PEARNVAL ~ AAGE + ACLSWKR + ADTIND + ADTOCC + AHSCOL + AMARITL  +    AMJIND      +  AMJOCC       + ASEX          + AUNMEM    +    CAPGAIN     +  CAPLOSS      + DIVVAL        + FILESTAT  +    MIGMTR1     +  MIGMTR3      + MIGMTR4       + MIGSUN    +    NOEMP       +  PARENT       + PEFNTVTY      + PEMNTVTY  +    PENATVTY    +  PRCITSHP     + SEOTR         + VETQVA    +    VETYN       +  WKSWORK            + EDUC      +    GRIN_ST_REG +  HDD_REL_FMX  + EMPLOY , 
	   				weights=learn_cleaned$POIDS,
					data=learn_cleaned, 
					control=rpart.control(minsplit=10))

	# ouverture d'une nouvelle fenêtre
		dev.new()
	# affichage de l'arbre de décision
		plot(arbre2, compress=TRUE)
		text(arbre2, use.n=TRUE)


####################################################
#                                                  #
#   TEST DU MODELE GRACE A L'ECHANTILLON DE TEST   #
#                                                  #
####################################################

	# Prediction de PEARNVAL pour l'échantillon de test

		pred <- predict(arbre2, newdata = test_cleaned[,c("AAGE","ACLSWKR","ADTIND","ADTOCC","AHSCOL","AMARITL","AMJIND","AMJOCC","ASEX","AUNMEM","CAPGAIN","CAPLOSS","DIVVAL","FILESTAT","MIGMTR1","MIGMTR3","MIGMTR4","MIGSUN","NOEMP","PARENT","PEFNTVTY","PEMNTVTY","PENATVTY","PRCITSHP","SEOTR","VETQVA","VETYN","WKSWORK","EDUC","GRIN_ST_REG","HDD_REL_FMX","EMPLOY")],
					type = c("vector"))

	# Affichage des risques de premier et de seconde espèce

		contin <- ftable(pred ~ test_cleaned$PEARNVAL)
		contin/cbind(rowSums(contin),rowSums(contin))


