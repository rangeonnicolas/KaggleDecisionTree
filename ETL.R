#setwd("C:\\Users\\CKW7\\Desktop\\DATAIKU\\code")
source("functions.R")


##############################
#                            #
#   EXTRACTION               #
#                            #
##############################

	metadata <- read.csv(file.path("data","metadata.csv"), sep=";",stringsAsFactors=FALSE)
	rownames(metadata) <- metadata$VAR_ID
	learn    <- read.csv(file.path("data","census_income_learn.csv"),header=FALSE, sep=",")
	names(learn) <- metadata$VAR_ID




##############################
#                            #
#   TRANSFORMATION           #
#                            #
##############################

	#!	Certaines variables sont des variables quantitatives discrètes mais sont 
	#!  codées à l'importation comme des variables quantitatives dans le 
	#!  data.frame (car elles sont représentées par des entiers qui sont en fait 
	#!  des codes). Celles-ci sont ADTIND, ADTOCC, SEOTR, VETYN, YEAR
	#! 	Il faut donc spécifier qu'elles sont en fait qualitatives.

			to.transform <- c("ADTIND", "ADTOCC", "SEOTR", "VETYN", "YEAR")
			for(i in to.transform)
				learn[i] <- factor(unlist(learn[i]))
		



	#! 	On peut commencer par un aperçu des données quantitatives:

		# selection des variables quantitatives
			quant <- metadata[which(metadata["TYPE"]=="continuous"),"VAR_ID"]
		# Affichage
			summary(learn[quant])

	#! 	Affichage des variables quantitatives en boite à moustaches:

			par(mfrow=c(3,3))
			for(i in quant)
				boxplot(learn[i],xlab=i)

	#! 	Observations :
	#! 	AAGE: il y a des valeurs à 0 pour l'âge
	#!  AHRSPAY : les salaires par heure sont souvent à 0 ("3e quantile à 0") et 
	#! 												salaire max très élevé 9999
	#! 	CAPGAIN, CAPLOS et DIVVAL : même anomalie que pour AHRSPAY
	
	#! 	Aperçu des données qualitatives:

			str(learn)

	#! 	Affichage des valeurs différentes pour chaque variable:

			disp.distinct.values(learn,metadata)


	#!	Pour chaque variable, le manque d'information est codé de manière 
	#!	multiple: " Not in universe", " ?" ou " NA" par exemple.
	#! 	Voici, pour chaque variable concernée par le manque d'information, le ou 
	#!	la chaîne de caractère indiquant ce manque:

			metadata["ACLSWKR",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["ADTIND",   	"STR_LACKOFINFO_1"] <- "0"
			metadata["ADTOCC",   	"STR_LACKOFINFO_1"] <- "0"
			metadata["AHRSPAY",   	"STR_LACKOFINFO_1"] <- 0
			metadata["AHSCOL",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["AMJIND",   	"STR_LACKOFINFO_1"] <- " Not in universe or children"
			metadata["AMJOCC",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["ARACE",   	"STR_LACKOFINFO_1"] <- " Other"
			metadata["AREORGN",   	"STR_LACKOFINFO_1"] <- " NA" 	
				metadata["AREORGN", "STR_LACKOFINFO_2"] <- " All other"
			metadata["AUNMEM",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["AUNTYPE",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["CAPGAIN",   	"STR_LACKOFINFO_1"] <- 0
			metadata["CAPLOSS",   	"STR_LACKOFINFO_1"] <- 0
			metadata["DIVVAL",   	"STR_LACKOFINFO_1"] <- 0
			metadata["GRINREG",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["GRINST",   	"STR_LACKOFINFO_1"] <- " ?"
				metadata["GRINST",  "STR_LACKOFINFO_2"] <- " Not in universe"
			metadata["MIGMTR1",   	"STR_LACKOFINFO_1"] <- " ?"
				metadata["MIGMTR1", "STR_LACKOFINFO_2"] <- " Not in universe"
			metadata["MIGMTR3",   	"STR_LACKOFINFO_1"] <- " ?"
				metadata["MIGMTR3", "STR_LACKOFINFO_2"] <- " Not in universe"
			metadata["MIGMTR4",   	"STR_LACKOFINFO_1"] <- " ?"
				metadata["MIGMTR4", "STR_LACKOFINFO_2"] <- " Not in universe"
			metadata["MIGSAME",   	"STR_LACKOFINFO_1"] <- " Not in universe under 1 year old"
			metadata["MIGSUN",   	"STR_LACKOFINFO_1"] <- " ?"
				metadata["MIGSUN",  "STR_LACKOFINFO_2"] <- " Not in universe"
			metadata["PARENT",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["PEFNTVTY",   	"STR_LACKOFINFO_1"] <- " ?"
			metadata["PEMNTVTY",   	"STR_LACKOFINFO_1"] <- " ?"
			metadata["PENATVTY",   	"STR_LACKOFINFO_1"] <- " ?"
			metadata["SEOTR",   	"STR_LACKOFINFO_1"] <- "0"
			metadata["VETQVA",   	"STR_LACKOFINFO_1"] <- " Not in universe"
			metadata["VETYN",   	"STR_LACKOFINFO_1"] <- "0"

		# Modification dans le jeu de données du manque d'information par la 
		# valeur NA, reconnue par le langage R
		for(VAR in unlist(metadata["VAR_ID"])){
			if(!is.na(replace <- metadata[VAR,"STR_LACKOFINFO_1"]))
				learn[which(learn[VAR] == replace),VAR] <- NA
			if(!is.na(replace <- metadata[VAR,"STR_LACKOFINFO_2"]))
				learn[which(learn[VAR] == replace),VAR] <- NA	
		}

	#! 	Visualisation de la proportion du manque d'information pour chaque variable :

			par(mfrow=c(1,1))
		# Nombre de valeurs NA par colonne :
			cnt <- count.na(learn)
		# Graphique :
			temp <- rbind(cnt,nrow(learn)-cnt)
			barplot(temp, col=c("red","grey"))

	#! 	Observations :
	#! 	Beaucoup de variables sont très peu renseignées  et seront donc sûrement 
	#! 	inutilisables...
	#! 	9 variables semblent avoir exactement 50% de valeurs manquantes
	#! 	(ACLSWKR ADTIND ADTOCC AMJIND AMJOCC MIGMTR1 MIGMTR3 MIGMTR4 MIGSAME)
	#! 	Pourquoi?
			ftable(learn[c("MIGSAME","YEAR")],exclude = NULL)
	#! 	Les données de migrations semblent ne pas avoir été renseignées en 1995

	#!	Après avoir étudié les différentes instances des variables, voici 
	#! 	quelques observations:
	#! 	
	#! 	VARIABLE AHGA :
	#!
	#!	AHGA est une variable dont les valeurs pourraient être classées (selon 
	#!	le nombre d'années d'études par exemple). Intuitivement, le salaire dune 
	#! 	personne risque d'être très corrélé au niveau d'études.
	#! 	Par ce classement, on gagne de l'information par rapport au jeu de 
	#! 	données initiales.
	#!	Cette variable pourrait donc être transformée en variable quantitative 
	#! 	discrète selon la classification suivante:
	#!	0 	" Children"  
	#!	1 	" Less than 1st grade"
	#!	2 	" 1st 2nd 3rd or 4th grade" 
	#!	3 	" 5th or 6th grade"                      
	#!	4 	" 7th and 8th grade"                     
	#!	5 	" 9th grade"
	#!	6 	" 10th grade"                            
	#!	7 	" 11th grade"                            
	#!	8 	" 12th grade no diploma"                 
	#!	9 	" High school graduate"          
	#!	10 	" Some college but no degree" 
	#!	11 	" Associates degree-occup /vocational"   
	#!	12 	" Associates degree-academic program"
	#!	13 	" Bachelors degree(BA AB BS)" 
	#!	14 	" Masters degree(MA MS MEng MEd MSW MBA)"
	#!	15 	" Prof school degree (MD DDS DVM LLB JD)"
	#!	16 	" Doctorate degree(PhD EdD)" 

		# tableau de correspondance
			edu.corresp <- setNames(0:16 , c(" Children"  ," Less than 1st grade"," 1st 2nd 3rd or 4th grade" ," 5th or 6th grade"                      ," 7th and 8th grade"                     ," 9th grade"," 10th grade"                            ," 11th grade"                            ," 12th grade no diploma"                 ," High school graduate"          ," Some college but no degree" ," Associates degree-occup /vocational"   ," Associates degree-academic program"," Bachelors degree(BA AB BS)" ," Masters degree(MA MS MEng MEd MSW MBA)"," Prof school degree (MD DDS DVM LLB JD)"," Doctorate degree(PhD EdD)" ))
		# extraction de la colonne AHGA sous forme de chaines de caractères
			EDUC <- levels(learn$AHGA)[learn$AHGA]
		# creation d'une nouvelle variable: "EDUC", transformation des 
		# occurences selon le tableau de correspondances
			learn$EDUC <- edu.corresp[EDUC]
		# verification
			ftable(learn[c("EDUC","AHGA")],exclude=NULL)
		# desactivation de AHGA et activation de EDUC
			metadata["AHGA"          ,"USED"      ] <- "no" 					# desactivation de AHGA
			metadata[nrow(metadata)+1,"USED"      ] <- "yes" 					# creation d'une nouvelle ligne dans metadata
			rownames(metadata)[nrow(metadata)     ] <- "EDUC" 					# nomage de la nouvelle ligne
			metadata["EDUC"          ,"VAR_ID"    ] <- "EDUC"  					# nomage de la nouvelle ligne
			metadata["EDUC"          ,"VAR_DESCR1"] <- "quantification de la variable AHGA-niveau d'education"
			metadata["EDUC"          ,"TYPE"      ] <- "continuous"
			metadata["EDUC"          ,"ORIGIN"    ] <- "manual"

	#! 	VARIABLES GRINREG et GRINST
	#! 	L'état et la région sont très probablement fortement corrélées.
	#! 	Vérification:
 			ftable(learn$GRINREG ~ learn$GRINST,exclude=NULL)
 	#! 	Effectivement, GRINREG est entièrement déductible de GRINST, sauf dans 
 	#! 	le cas où GRINST=" Abroad", où GRINREG est soit " West" soit " Abroad"
 	#! 	Cela semble au passage illogique. Il y peut-être erreur de saisie.
 	#! 	La variable GRINREG pourrait dont être supprimée sans pour autant perdre
 	#! 	d'information. Mais à cause de l'ambiguité levée ci dessus, il est 
 	#! 	préférable pour ne pas perdre d'information de transformer ces deux 
 	#! 	variables en une seule, par exemple par concaténation.
 	#! 	Ainsi, on obtiendrait par exemple, pour l'état de l'Utah:" Utah-- South"
 	#! 	Le nombre d'instances différentes de cette variable sera donc le même 
 	#! 	que celui de GRINST, auquel on ajoute les instances " Abroad-- Abroad"
 	#! 	et " Abroad -- West".
 	#! 	Ainsi, nous avons une variable en moins, pas de perte d'information, et 
 	#! 	la nouvelle variable n'a qu'une seule instance de plus que GRINST, ce qui
 	#! 	est peu.
 	 	# Voici la concaténation placée dans une nouvelle colonne nommée "GRIN_ST_REG":
 	 		learn$GRIN_ST_REG <- factor(paste0(learn$GRINST,"--",learn$GRINREG))
 	 	# Il faut également actualiser les metadonnées
 	 		metadata["GRINREG"          ,"USED"      ] <- "no" 	
 	 		metadata["GRINST"           ,"USED"      ] <- "no" 
			metadata[nrow(metadata)+1   ,"USED"      ] <- "yes"
			rownames(metadata)[nrow(metadata)        ] <- "GRIN_ST_REG"
			metadata["GRIN_ST_REG"      ,"VAR_ID"    ] <- "GRIN_ST_REG"
			metadata["GRIN_ST_REG"      ,"VAR_DESCR1"] <- "concatenation de GRINREG et GRINST"
			metadata["GRIN_ST_REG"      ,"TYPE"      ] <- "nominal"
			metadata["GRIN_ST_REG"      ,"ORIGIN"    ] <- "manual"

	#! 	VARIABLES HHDREL ET HHDFMX
	#! 	Ces variables sont proablement corrélées.
			ftable(learn$HHDREL ~ learn$HHDFMX,exclude=NULL)
	#! 	Ici, le même cas que précedemment survient, avec la valeur particulière de
	#! 	" In group quarters" pour HHDFMX, qui peut prendre toutes les valeurs
	#! 	pour HHDREL.
	#! 	Nous faisons donc ici aussi une concaténation.
 	 		learn$HDD_REL_FMX <- factor(paste0(learn$HHDFMX,"--",learn$HHDREL))
 	 	# Il faut également actualiser les metadonnées
 	 		metadata["HHDREL"           ,"USED"      ] <- "no" 	
 	 		metadata["HHDFMX"           ,"USED"      ] <- "no" 
			metadata[nrow(metadata)+1   ,"USED"      ] <- "yes"
			rownames(metadata)[nrow(metadata)        ] <- "HDD_REL_FMX"
			metadata["HDD_REL_FMX"      ,"VAR_ID"    ] <- "HDD_REL_FMX"
			metadata["HDD_REL_FMX"      ,"VAR_DESCR1"] <- "concatenation de HHDREL et HHDFMX"
			metadata["HDD_REL_FMX"      ,"TYPE"      ] <- "nominal"
			metadata["HDD_REL_FMX"      ,"ORIGIN"    ] <- "manual"

	#! 	VARIABLES AMJOCC ET AMJIND :
	#! 	Ces deux variables sont peut-être corrélées (elles sont toutes les deux 
	#!	liées au métier excercé). Vérification:
			ftable(learn$AMJOCC ~ learn$AMJIND,exclude=NULL)
	#! 	Il y a sûrement corrélation, mais pas assez pour déduire quelquechose.


	#! 	VARIABLES AREORGN ET ARACE
	#! 	Ces deux variables sont peut-être corrélées. Vérification:
			ftable(learn$AREORGN ~ learn$ARACE,exclude=NULL)
 	#! 	Il y a corrélation évidente (nous apprenons notamment qu'aucun Esquimo 
 	#! 	n'est Cubain)
	#! 	La concaténation donnerait 26 instances différentes, ce qui est encore
	#! 	acceptable:
	 	 	learn$A_REORGN_RACE <- factor(paste0(learn$AREORGN,"--",learn$ARACE))
 	 	# Il faut également actualiser les metadonnées
 	 		metadata["ARACE"              ,"USED"      ] <- "no" 	
 	 		metadata["AREORGN"            ,"USED"      ] <- "no" 
			metadata[nrow(metadata)+1     ,"USED"      ] <- "yes"
			rownames(metadata)[nrow(metadata)          ] <- "A_REORGN_RACE"
			metadata["A_REORGN_RACE"      ,"VAR_ID"    ] <- "A_REORGN_RACE"
			metadata["A_REORGN_RACE"      ,"VAR_DESCR1"] <- "concatenation de ARACE et AREORGN"
			metadata["A_REORGN_RACE"      ,"TYPE"      ] <- "nominal"
			metadata["A_REORGN_RACE"      ,"ORIGIN"    ] <- "manual"

	#! 	VARIABLES AUNTYPE ET AWKSTAT
	#! 	Ces variables sont probablement corrélées:
			ftable(learn$AUNTYPE ~ learn$AWKSTAT,exclude=FALSE)
	#! 	Par concaténation, on obtient le status de la personnes (ie. s'il est
	#! 	employé ou pas), et s'il n'est pas employé, nous en connaissons la raison.
	 	 	learn$EMPLOY <- factor(paste0(learn$AWKSTAT,"--",learn$AUNTYPE))
 	 	# Il faut également actualiser les metadonnées
 	 		metadata["AUNTYPE"        ,"USED"      ] <- "no" 	
 	 		metadata["AWKSTAT"        ,"USED"      ] <- "no" 
			metadata[nrow(metadata)+1 ,"USED"      ] <- "yes"
			rownames(metadata)[nrow(metadata)      ] <- "EMPLOY"
			metadata["EMPLOY"         ,"VAR_ID"    ] <- "EMPLOY"
			metadata["EMPLOY"         ,"VAR_DESCR1"] <- "concatenation de AUNTYPE et AWKSTAT"
			metadata["EMPLOY"         ,"TYPE"      ] <- "nominal"
			metadata["EMPLOY"         ,"ORIGIN"    ] <- "manual"

	#! 	VARIABLES MIGMTR1, MIGMTR3, MIGMTR4, MIGSUN et MIGSAME
	#! 	Elles sont toutes liées aux migrations.
	#! 	Visualisons leurs éventuelles corrélations possibles:
			pairs(learn[c("MIGMTR1","MIGMTR3","MIGMTR4","MIGSUN","MIGSAME")])
	#!	MIGSAME est déductible entièrement de MIGMTR1. On peut donc supprimer cette 
	#!	variable sans perdre d'information et en ayant la même granularité.
	 	# Désactivation de MIGSAME :
	 		metadata["MIGSAME","USED"] <- "no"


	#! 	VARIABLE AHRSPAY
	#! 	Elle est censée indiquer le salaire par heure (selon les indications 
	#! 	fournies dans les metadata). Cependant, ses valeurs sont bien trop grandes
 	#! 	pour correspondre vraiment à un salaire par heure.
 	#! 	Il y a également beaucoup de valeurs à 0.
 	#! 	L'exercice vise à étudier si les personnes gagnent plus ou moins de 50k€.
 	#! 	Cette variable semble logiquement beaucoup trop corrélée au fait de gagner
 	#! 	plus ou moins de 50k€; c'est à dire qu'il est innintéressant de conclure
 	#! 	une phrsase du type "L'un des facterus sociologiques le plus déterminant
 	#! 	de fait qu'une personne gagne plus ou moins de 50k€ est le salaire par heure".
 	#! 	Supprimons donc cette variable:
 			metadata["AHRSPAY","USED"] <- "no"

 	#! 	VARIABLE MARSUPWT:
 	#! 	Il est demandé dans les metadata de ne pas tenir compte de la variable
 	#! 	MARSUPWT (instance weight). Cette information nous semble effectivement
 	#! 	inutile pour l'execice. Desactivons-là:
 	 		metadata["MARSUPWT","USED"] <- "no"

 	#! 	VARIABLE YEAR
 	#! 	Les données ont été collectées sur les années 94 et 95. Cette information
 	#! 	n'est d'aucune utilité.
 			metadata["YEAR","USED"] <- "no"

 	#! 	VARIABLES SEOTR et VETYN
 	#! 	Ces variables semblent binaires, mais sont codées avec les valeurs 0,1,2 .
 	#! 	On ne sait donc pas si l'individu a la caractéristique "veterans benefits"
 	#! 	et s'il est "self employed", car on ne connait pas la correspondance.
 	#! 	Mais l'âge des individus pourrait peut-être nous permettre de déduire 
 	#! 	ces informations, car les enfants ne touchent probablement pas de 
 	#! 	d'allocations pour vétérans, et ne travaillent probablement pas:
 			ftable(learn$AAGE ~ learn$VETYN,exclude=NULL)
 			ftable(learn$AAGE ~ learn$SEOTR,exclude=NULL)
 	#! 	Observations :
 	#! 	Des enfants de 16ans ont VETYN à 1 et d'autres à 2. Cela est très étonnant.
 	#! 	Aucune conclusion ne sera tirée pour VETYN
 	#! 	Idem pour la variable SEOTR, où des enfants de 15ans ont l'instance 1 et 
 	#! 	d'autres 2. Aucune conclusion non plus.
 			

	#!  Visualisation des corrélations entre les variables quantitatives après toutes les
	#! 	modifications précéentes :
		# Selection des variables quantitatives actives :
			quant2 <- metadata[which(metadata["TYPE"]=="continuous" & metadata$USED=="yes"),"VAR_ID"]
			pairs(learn[quant2])
	#! 	Observations :
	#! 	Pour toutes ces variables, quasiment rien ne se passe lorsque l'âge de la
	#! 	personne est très bas.






##############################
#                            #
#   ExPORTATION DE LA DONNEE #
#                            #
##############################

	# selection des colonnes non désactivées
		learn_cleaned <- learn[which(metadata$USED == "yes")]
	# enregistrement dans un csv
		write.csv(learn_cleaned,file.path("data","CLEANED","learn.csv"))








