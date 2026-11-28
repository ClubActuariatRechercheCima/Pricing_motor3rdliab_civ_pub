require(RMySQL)
require(compare)
require(stringr)
require(stringdist)
require(plyr)
require(reshape2)


chemin_base="<CHEMIN_LOCAL>/"

Chemin_resultat= paste0(chemin_base,"Resultat/")

chemin_input= paste0(chemin_base, "Input_CSV/")





Liste_Fichiers=list.files(paste0(chemin_input,"Prod 0618"),pattern = "*.csv")
#colnames(Table9)[colnames(Table9)=="anciennum"] <- "ancienpol"

for (i in 1:length(Liste_Fichiers))
{
  
  Nom = paste("Table",i,sep="")
  assign( Nom,read.csv2(paste0(chemin_base,"Input_CSV/Prod 0618/",Liste_Fichiers[i]),  stringsAsFactors = FALSE ))
  
}

### Choix des colonnes à utiliser dans les traitements
liste_colname=colnames(Table1)
for (i in 1:9)
{
  
  liste_colname = intersect(liste_colname, colnames(get(paste0("Table",i)))) 
  
}
liste_colname


###### Fusion des inputs de bases

BaseFusionPrime=get(paste("Table",1,sep=""))[,liste_colname]

#TableEntiere = merge(get(paste("Table",6)),get(paste("Table",7)),all=TRUE)


for (i in 2:length(Liste_Fichiers))
{
  
  BaseFusionPrime=rbind(BaseFusionPrime,get(paste0("Table",i))[,liste_colname])
  
  #PrimeAnalyse  = merge(PrimeAnalyse, get(paste("Table",i,sep="")),all=TRUE)
  
}


#### Construction de la base néttoyée ####
BasePropre= BaseFusionPrime


#
#BasePropre$police=Retraitement_police(BasePropre$ancienpol)
#BaseFusionPrime$date_facture=as.Date(BaseFusionPrime$date_facture)
###Convertir un format en Date
BasePropre$date_facture=as.Date(BasePropre$date_facture,format = "%d/%m/%Y")
#BaseFusionPrime$periode=as.Date(BaseFusionPrime$periode,format = "%d/%m/%Y")
BasePropre$date_effet=as.Date(BasePropre$date_effet,format = "%d/%m/%Y")
BasePropre$fin_garantie=as.Date(BasePropre$fin_garantie,format = "%d/%m/%Y")



#### recodification des Branches, segement et canal "##
  ## Types intermédiares

BasePropre$typeinter= as.factor(BasePropre$typeinter)
levels(BasePropre$typeinter) = c("Bureaux Direct","Courtiers","Agences","Agences","Agences", "Bancassurance","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Courtiers","Courtiers","Courtiers")
table(BasePropre$typeinter)

BasePropre$typeinter=ordered(BasePropre$typeinter,c("Courtiers","Bureaux Direct","Agences","Bancassurance"))
  # Branches   


BasePropre$branche= as.factor(BasePropre$branche)
levels(BasePropre$branche) = c("Assistance","Inc et DAB","Dommages corporels","Inc et DAB","Sante","Responsabilite civile","Responsabilite civile","Responsabilite civile","Risques Agricoles","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Vehicule Ter","Vehicule Ter","Vehicule Ter")
table(BasePropre$branche)

  ## Segment 
table(BasePropre$codepartenaire)

#BasePropre$client[BasePropre$codepartenaire=="partner"]
BasePropre$codepartenaire= as.factor(BasePropre$codepartenaire)
levels(BasePropre$codepartenaire) = c("Autres","Particulier","Entreprise","Particulier","Entreprise")
BasePropre$codepartenaire= as.ordered(BasePropre$codepartenaire)


#### Traitement des données : Intermediaire, Client, Police,

  ###1 Les intermadiaires// types intermédaires ####
  Codification_interm_Court =Codification(unique(BasePropre$intermediaire))
  Codification_interm_long =Codification(BasePropre$intermediaire)
 
  ##  1.a Check des recodifications ###
   analyse_doublon_intermediaire_long=   Comparaison_vecteur(Codification_interm_long$Lib_init, Codification_interm_long$Lib_travaille)
  
   ## Modifie avec les noms d'intermédiaires travaillés
   BasePropre$intermediaire=analyse_doublon_intermediaire_long$Vec_Recodifie
   
  #construction du référent unique d'intermédiaire (en qualifiant le type intermediaire)

  Table_intermediaire=data.frame(BasePropre$intermediaire,Codification_interm_long$Code,BasePropre$typeinter)
  colnames(Table_intermediaire)=c("intermediaire","Code","typeinter")
  
  Vec_Variable=c("typeinter")
  by_variable=c("intermediaire","Code")
 
  Table_intermediaire=aggregate(Table_intermediaire[,Vec_Variable],by=Table_intermediaire[,by_variable], FUN = max, na.rm=TRUE)
   colnames(Table_intermediaire)[3]=Vec_Variable
  
   write.table(Table_intermediaire,paste0(Chemin_resultat,"tbIntermediaire.csv"), sep =";", dec = ".")
    
    ### Correction de l'adéquation TypeInter vs Intermediaire
   
      test= merge(BasePropre[,-c(1)],Table_intermediaire[c("intermediaire","typeinter")],by="intermediaire",all.x = TRUE,all.y = FALSE)   
      BasePropre=test
  
  ### Les clients #######
  TTT= Recodificiation(Base =BasePropre ,NomVecteur ="client" )
  
  ### Correction des noms de clients ###
  BasePropre$client=TTT$analyse_doublon_long.Vec_Recodifie
  
     #construction du référent unique de client en qualifiant le type de segment 
  Table_Client=data.frame(BasePropre$client,TTT$Codification_long.Code,BasePropre$codepartenaire)
  colnames(Table_Client)=c("client","code_client","codepartenaire")
  
  Vec_Variable=c("codepartenaire")
  by_variable=c("client","code_client")
  
  Table_Client=aggregate(Table_Client[,Vec_Variable],by=Table_Client[,by_variable], FUN = max, na.rm=TRUE)
  colnames(Table_Client)[3]=Vec_Variable
  
  write.table(Table_Client,paste0(Chemin_resultat,"tbClient.csv"), sep =";", dec = ".")
  
     ### Correction de l'adéquation Client vs Segment
  
      test= merge(BasePropre[,-c(2)],Table_Client[c("client","codepartenaire")],by="client",all.x = TRUE,all.y = FALSE)   
       BasePropre=test
  
  ### Les polices uniques ####
          ### Croiser la police et le client et prendre un seul client à chaque fois
      traiement_police= Fiabiliser_police(BasePropre$ancienpol)
       
       summary(traiement_police$Vec_CodePolOK)
       table(nchar(traiement_police$Vec_police_travaille))
       traiement_police$Liste_Police_a_corriger[1:10]
       #View(traiement_police$Table_police)
       
       
     
       BasePropre$ancienpol=traiement_police$Vec_police_travaille
       
       BasePropre=BasePropre[traiement_police$Vec_CodePolOK,]
       
       
       
     #  BasePropre2=BasePropre[!(BasePropre$branche%in% c("Assistance","Vehicule Ter")),]
       ##### Cohérence Numéros de police, Branches et Clients
       
     #  length(unique(BasePropre$ancienpol))
      # clePolice=paste0(BasePropre$branche,BasePropre$client,BasePropre$intermediaire)
      # length(unique(clePolice))
       
      # clePolice[nchar(clePolice)==8]
      #Analyse_doublon_police=  Comparaison_vecteur(Vecteur_Init = BasePropre$ancienpol, Vecteur_travaille = clePolice)
      
      #NbreCas= length(Analyse_doublon_police$Liste_cas_unique)
      
    #  id=runif(1,1,NbreCas)
     # Analyse_doublon_police$Liste_cas_unique[id]
      #Pos_id=Analyse_doublon_police$Liste_cas_position[[id]]
      #View(BasePropre[Pos_id,])
      
          #### hors Auto et Assistance ###
      #Analyse_doublon_police2=  Comparaison_vecteur(Vecteur_Init = BasePropre2$ancienpol, Vecteur_travaille = BasePropre2$ClePolice)
      #NbreCas2= length(Analyse_doublon_police2$Liste_cas_unique)
      
    #  id=runif(1,1,NbreCas2)
     # Analyse_doublon_police2$Liste_cas_unique[id]
     # Pos_id=Analyse_doublon_police2$Liste_cas_position[[id]]
    #  View(BasePropre2[Pos_id,])
      
     # length(unique(BasePropre2$ClePolice))
    #  length(unique(BasePropre2$ancienpol))
      
    #  Analyse_doublon_police2$Liste_cas_unique
      
     # write.table(BasePropre[Pos_id,],paste0(Chemin_resultat,"Anomalie_",round(id),".csv"), sep =";", dec = ".",row.names = FALSE)
      
      #### les dates
       