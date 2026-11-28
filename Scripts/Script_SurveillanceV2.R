chemin_base= "<CHEMIN_LOCAL>/"
require(stringr)
require(RMySQL)
require(compare)
require(stringr)
require(stringdist)
require(plyr)
require(reshape2)

setwd(chemin_base)
#Data à fin Sept 2018

Liste_Fichiers=list.files(paste0(chemin_base,"\\Data Production"),pattern = "*.csv")
Sys.getenv("TZ")


##-----Importer les bases production-----------------##############
for (i in 1:length(Liste_Fichiers))
{
  
  Nom = paste("Table",i,sep="")
  assign( Nom,read.csv2(paste0(chemin_base,"\\Data Production\\",Liste_Fichiers[i]),  stringsAsFactors = FALSE ))
}

###----------Renommer les colonnes de la table 9----------------------------#
colnames(Table9)[colnames(Table9)=="anciennum"] <- "ancienpol"
colnames(Table9)[colnames(Table9)=="categ_interm"] <- "typeinter"
#-------------Trouver les colonnes identiques--------------#
liste_colname=colnames(Table1)
for (i in 2:length(Liste_Fichiers))
{
  
  liste_colname = intersect(liste_colname, colnames(get(paste0("Table",i)))) 
  
}
liste_colname

#---------------- fusionner les bases production-----------#

BaseFusionPrime=get(paste("Table",1,sep=""))[,liste_colname]

#TableEntiere = merge(get(paste("Table",6)),get(paste("Table",7)),all=TRUE)


for (i in 2:length(Liste_Fichiers))
{
  BaseFusionPrime=rbind(BaseFusionPrime,get(paste0("Table",i))[,liste_colname])
  
  
  #PrimeAnalyse  = merge(PrimeAnalyse, get(paste("Table",i,sep="")),all=TRUE)
  
}
#-----------------retraiter les numeros de police de la base fusionnée-----------#
BaseFusionPrime$numeropolice=str_trim(BaseFusionPrime$ancienpol)
BaseFusionPrime$numeropolice=str_to_upper(BaseFusionPrime$numeropolice)
BaseFusionPrime$numeropolice=Retraitement_police(BaseFusionPrime$numeropolice)
#--------------Visualiser la base fusionnée----------------#
table(nchar(BaseFusionPrime$numeropolice))
table(nchar(t$numeropolice))
c=BaseFusionPrime[nchar(BaseFusionPrime$numeropolice)==6,]
baseanomalie=t
summary(BaseFusionPrime)


table(BaseFusionPrime[BaseFusionPrime$typeinter==3,]$intermediaire)

#### recodification des typeintermediare
colnames(BaseFusionPrime)[colnames(BaseFusionPrime)=="typeinter"] <- "CodeIntermediaire"
BaseFusionPrime$CodeIntermediaire= as.factor(BaseFusionPrime$CodeIntermediaire)
levels(BaseFusionPrime$CodeIntermediaire) = c("Bureaux Direct","Courtiers","Agences","Agences","Agences", "Bancassurance","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Courtiers","Courtiers","Courtiers")
table(BaseFusionPrime$CodeIntermediaire)
BaseFusionPrime$CodeIntermediaire=ordered(BaseFusionPrime$CodeIntermediaire,c("Courtiers","Bureaux Direct","Agences","Bancassurance"))

### recodification de code partenaire
table(BaseFusionPrime$codepartenaire)
BaseFusionPrime$codepartenaire= as.factor(BaseFusionPrime$codepartenaire)
levels(BaseFusionPrime$codepartenaire)
levels(BaseFusionPrime$codepartenaire) = c("autres","Particulier","Entreprise","Particulier","Entreprise")

###recodification de code branche
table(BaseFusionPrime$branche)
BaseFusionPrime$branche= as.factor(BaseFusionPrime$branche)
levels(BaseFusionPrime$branche) = c("Assistance","Inc et DAB","Dommages corporels","Inc et DAB","Santé","Responsabilité civile","Responsabilité civile","Responsabilité civile","Risques Agricoles","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","auto","auto","auto")
table(BaseFusionPrime$branche)


tb_intermediaire=read.csv2("table_intermediaire.csv",  stringsAsFactors = FALSE )
tb_intermediaire$Intermediaire.SUNU.PAC=str_trim(tb_intermediaire$Intermediaire.SUNU.PAC)
BaseFusionPrime$intermediaire=str_trim(BaseFusionPrime$intermediaire)
BaseFusionPrime$intermediairegood=tb_intermediaire$Intermediairegood[match(BaseFusionPrime$intermediaire,tb_intermediaire$Intermediaire.SUNU.PAC)]
BaseFusionPrime[is.na(BaseFusionPrime$intermediairegood),]$intermediairegood=BaseFusionPrime[is.na(BaseFusionPrime$intermediairegood),]$intermediaire

###------------------- Les polices uniques ---------------------------- ####
vecMauvaizPol=nchar(BaseFusionPrime$numeropolice)!=9
BaseFusionPrime$numeropolicecommun=""
BaseFusionPrime$numeropolicecommun2=str_sub(BaseFusionPrime$police,6,15)
BaseFusionPrime[vecMauvaizPol,]$numeropolicecommun=str_sub(BaseFusionPrime[vecMauvaizPol,]$police,6,15)
BaseFusionPrime[!vecMauvaizPol,]$numeropolicecommun=BaseFusionPrime[!vecMauvaizPol,]$numeropolice
table(nchar(BaseFusionPrime$numeropolicecommun))
#BaseFusionPrime$numeropolicecommun=str_sub(BaseFusionPrime$police,1,15)


###Convertir un format en Date
BaseFusionPrime$date_facture=as.Date(BaseFusionPrime$date_facture,format = "%d/%m/%Y")
BaseFusionPrime$periode=as.Date(BaseFusionPrime$periode,format = "%d/%m/%Y")
BaseFusionPrime$date_effet=as.Date(BaseFusionPrime$date_effet,format = "%d/%m/%Y")
BaseFusionPrime$fin_garantie=as.Date(BaseFusionPrime$fin_garantie,format = "%d/%m/%Y")
BaseFusionPrime$date_facture=as.Date(BaseFusionPrime$date_facture)

BaseFusionPrime$primeTTC=BaseFusionPrime$pnette+BaseFusionPrime$cp+BaseFusionPrime$taxe


Basepropre_311018=BaseFusionPrime


#BaseFusionPrime$IdUnique= as.numeric(row.names(BaseFusionPrime))

#### Créer l'identifiant unique#####

Basepropre_311018$Identifiant=paste0(Basepropre_311018$numeropolicecommun,as.numeric(Basepropre_311018$fin_garantie))
summary(duplicated(Basepropre_311018$Identifiant))

###Recoder variable d'une table#
Basepropre_311018$primeTTC=as.numeric(Basepropre_311018$primeTTC)
Basepropre_311018$pnette=as.numeric(Basepropre_311018$pnette)
Basepropre_311018$coass=as.numeric(Basepropre_311018$coass)
Basepropre_311018$commission=as.numeric(Basepropre_311018$commission)
Basepropre_311018$cp=as.numeric(Basepropre_311018$cp)
Basepropre_311018$nbprime=1

####---------- Traitement des données : Intermediaire, Client, Police,
{
###--------Client-------------

Basepropre_311018$client=str_trim(Basepropre_311018$client)
tbclient=data.frame(Basepropre_311018$numeropolicecommun,Basepropre_311018$client,Basepropre_311018$codepartenaire)
colnames(tbclient)=c("numeropolicecommun","client","codepartenaire")
tbclient$client=Retraitement_client(tbclient$client)
tbclient$client1=Basepropre_311018$client[match(tbclient$numeropolicecommun,Basepropre_311018$numeropolicecommun)]

tbclient$flag=(tbclient$client!=tbclient$client1)
tbclientEntreprise=tbclient[tbclient$codepartenaire=="Entreprise",]
tbclientEntreprise=tbclientEntreprise[!(duplicated(tbclientEntreprise$client)),]

write.table(tbclientEntreprise,file = "tbclientEntreprise.csv",sep=";",dec=",", row.names = FALSE)
read.csv2("tbclientEntreprise.csv",  stringsAsFactors = FALSE )


###--------Intermediaire-------------

Basepropre_311018$intermediaire=str_trim(Basepropre_311018$intermediaire)
tbintermediaire=data.frame(Basepropre_311018$numeropolicecommun,Basepropre_311018$client,Basepropre_311018$intermediaire)
colnames(tbintermediaire)=c("numeropolicecommun","client","intermediaire")
tbintermediaire$intermediaire1=Basepropre_311018$intermediaire[match(tbintermediaire$numeropolicecommun,Basepropre_311018$numeropolicecommun)]
tbintermediaire=tbintermediaire[!(duplicated(tbintermediaire$intermediaire)),]
tbintermediaire$flag=(tbintermediaire$intermediaire!=tbintermediaire$intermediaire1)


write.table(tbintermediaire,file = "tbintermediaire.csv",sep=";",dec=",", row.names = FALSE)
read.csv2("tbintermediaire.csv",  stringsAsFactors = FALSE )



#--------------traitement Nom client------------------------------------------
#by_variable1= c("lenghtclient","numeropolicecommun")
#Table_client2= aggregate(Basepropre_311018$client,by=Basepropre_311018[,by_variable1], FUN = max, na.rm=TRUE)

}
#--------------traitement date effet Prodcution------------------------------------------
Table_effet=Basepropre_311018
by_variable1 = c("numeropolicecommun","Identifiant","fin_garantie")
Vec_Variable = c("date_effet")
Table_effet= aggregate(Basepropre_311018[,Vec_Variable],by=Basepropre_311018[,by_variable1], FUN = min, na.rm=TRUE)
colnames(Table_effet)[4]="date_effet"
Table_effet$Duree=DureePolice(Table_effet$date_effet,Table_effet$fin_garantie)
Table_effet$AnneePolice=Exposition(Table_effet$date_effet,Table_effet$fin_garantie)
Table_effet_client= Table_effet
Table_effet_client$client=Basepropre_311018$client[match(Table_effet_client$Identifiant,Basepropre_311018$Identifiant)]


#----------------Faire une somme des contrat en doublons--------------------#
Vec_Variable = c("primeTTC","pnette","coass", "cp","commission","nbprime","taxe")
by_variable = c("Identifiant","numeropolicecommun","fin_garantie")
Basepropre1 <-aggregate(Basepropre_311018[,Vec_Variable],by=Basepropre_311018[,by_variable], FUN = SumDouble, na.rm=TRUE)

#----------------Création base anomalie Prod--------------------#
Vec_VariableAno = c("pnette","coass", "cp","commission","nbprime","taxe")
by_variableAno = c("codepartenaire","intermediairegood","branche","Identifiant","client","numeropolice","fin_garantie","numeropolicecommun")
base_anomalie_prod <-aggregate(Basepropre_311018[,Vec_VariableAno],by=Basepropre_311018[,by_variableAno], FUN = SumDouble, na.rm=TRUE)
base_anomalie_prod=base_anomalie_prod[duplicated(base_anomalie_prod$Identifiant),]

###### ---------------------- Fusionner base fusionnée et les variables importantes -------------------- ###
BasePropreProd=Basepropre1
VecdeBase=c("date_effet","Duree","AnneePolice","client")
VecdeBase1=c("codepartenaire","intermediaire","branche")
BasePropreProd$date_effet=Table_effet_client$date_effet[match(BasePropreProd$Identifiant,Table_effet_client$Identifiant)]
BasePropreProd$Duree=Table_effet_client$Duree[match(BasePropreProd$Identifiant,Table_effet_client$Identifiant)]
BasePropreProd$AnneePolice=Table_effet_client$AnneePolice[match(BasePropreProd$Identifiant,Table_effet_client$Identifiant)]
BasePropreProd$client=Table_effet_client$client[match(BasePropreProd$Identifiant,Table_effet_client$Identifiant)]
BasePropreProd$codepartenaire=Basepropre_311018$codepartenaire[match(BasePropreProd$Identifiant,Basepropre_311018$Identifiant)]
BasePropreProd$intermediaire=Basepropre_311018$intermediairegood[match(BasePropreProd$Identifiant,Basepropre_311018$Identifiant)]
BasePropreProd$branche=Basepropre_311018$branche[match(BasePropreProd$Identifiant,Basepropre_311018$Identifiant)]
BasePropreProd$codeintermediaire=Basepropre_311018$CodeIntermediaire[match(BasePropreProd$Identifiant,Basepropre_311018$Identifiant)]
BasePropreProd$produit=Basepropre_311018$produit[match(BasePropreProd$Identifiant,Basepropre_311018$Identifiant)]
BasePropreProd$ancienPol=Basepropre_311018$numeropolice[match(BasePropreProd$Identifiant,Basepropre_311018$Identifiant)]
BasePropreProd$newPol=Basepropre_311018$police[match(BasePropreProd$Identifiant,Basepropre_311018$Identifiant)]

###### ---------------------- Indiquer les identifiants anormales -------------------- ###
BasePropreProd$anomalie=(BasePropreProd$Identifiant%in%base_anomalie_prod$Identifiant )
#--------------------------------------------------------------------------------------------------------
#--------Traitement de la Base sinistre------------------------------------------##""""
#--------------------------------------------------------------------------------------------
basesinistre=read.csv2("<CHEMIN_LOCAL>/ip_detaillée_vu_31102018.csv")

##retraitement police###
basesinistre$ancpolice=as.character(basesinistre$ancpolice)
basesinistre$numeropolice=str_trim(basesinistre$ancpolice)
basesinistre$numeropolice=str_to_upper(basesinistre$numeropolice)
basesinistre$numeropolice=Retraitement_police(basesinistre$numeropolice)
vecMauvaizPolsinistre=nchar(basesinistre$numeropolice)!=9
basesinistre$numeropolicecommun=""
basesinistre$numeropolicecommun2=str_sub(basesinistre$police,6,15)
basesinistre[vecMauvaizPolsinistre,]$numeropolicecommun=str_sub(basesinistre[vecMauvaizPolsinistre,]$police,6,15)
basesinistre[!vecMauvaizPolsinistre,]$numeropolicecommun=basesinistre[!vecMauvaizPolsinistre,]$numeropolice
table(nchar(basesinistre$numeropolicecommun))
basesinistre[vecMauvaizPolsinistre,]$numeropolicecommun=Basepropre_311018$numeropolicecommun[match(
                                                                 basesinistre[vecMauvaizPolsinistre,]$numeropolicecommun2,Basepropre_311018$numeropolicecommun2)]




###Convertir un format en Date
#basesin$date_facture=as.Date(basesin$date_facture)
basesinistre$dateeffet=as.Date(basesinistre$dateeffet,format = "%d/%m/%Y")
basesinistre$datefineffet=as.Date(basesinistre$datefineffet,format = "%d/%m/%Y")
basesinistre$Identifiant=paste0(basesinistre$numeropolicecommun,as.numeric(basesinistre$datefineffet))
sum(as.numeric(basesinistre$Provision_depense),na.rm = TRUE)
sum(as.numeric(basesinistre$cumul_paye),na.rm = TRUE)
summary(duplicated(basesinistre$Identifiant))
basesinistre$cumul_paye=as.numeric(basesinistre$cumul_paye)
basesinistre$Provision_depense=as.numeric(basesinistre$Provision_depense)
basesinistre$nbsin=1

#--------------traitement date effet Sinistre------------------------------------------
Table_effet_Sin=basesinistre
by_variable1 = c("numeropolicecommun","Identifiant","datefineffet")
Vec_Variable = c("dateeffet")
Table_effet_Sin= aggregate(basesinistre[,Vec_Variable],by=basesinistre[,by_variable1], FUN = min, na.rm=TRUE)
colnames(Table_effet_Sin)[4]="dateeffet"
Table_effet_Sin$Duree=DureePolice(Table_effet_Sin$dateeffet,Table_effet_Sin$datefineffet)
Table_effet_Sin$AnneePolice=Exposition(Table_effet_Sin$dateeffet,Table_effet_Sin$datefineffet)
Table_effet_client_Sin= Table_effet_Sin
Table_effet_client_Sin$clientsin=basesinistre$client[match(Table_effet_client_Sin$Identifiant,basesinistre$Identifiant)]



#---------------Methode1: Somme des sinistre de la même police--------------
Vec_VariableSin = c("cumul_paye","Provision_depense","Recours_exercice","nbsin" )
by_variableSin = c("Identifiant","datefineffet","numeropolicecommun")
BasePropresin <-aggregate(basesinistre[,Vec_VariableSin],by=basesinistre[,by_variableSin], FUN=SumDouble, na.rm=TRUE)

BasePropresin$DateEffet=Table_effet_client_Sin$dateeffet[match(BasePropresin$Identifiant,Table_effet_client_Sin$Identifiant)]
#sinistre=BasePropresin2
summary(duplicated(BasePropresin$Identifiant))


#???-----------------------Merge------------------------------------------------------------------------
baseprod_basesin= merge(BasePropreProd,BasePropresin, by.x ="Identifiant",by.y= "Identifiant",all.x=T)
#baseprod_basesin2= merge(prime,sinistre2, by.x ="Identifiant",by.y= "Identifiant",all.x=T)
#-----------------------Ramener des variables clés dans la base sommée (équivalent de Recherche V d'Excel) ------------------------------------------------------------------------
baseprod_basesin$frais_gestion=0.16*baseprod_basesin$pnette
baseprod_basesin$Autres_charges=baseprod_basesin$frais_gestion+baseprod_basesin$commission
#baseprod_basesin$type_intermediaire=Basepropre_311018$CodeIntermediaire[match(baseprod_basesin$Identifiant,Basepropre_311018$Identifiant)]
#baseprod_basesin$code_partenaire=Basepropre_311018$codepartenaire[match(baseprod_basesin$Identifiant,Basepropre_311018$Identifiant)]

baseprod_basesin$charge_totale=baseprod_basesin$cumul_paye+baseprod_basesin$Provision_depense
baseprod_basesin$charge_totalenetderecours=baseprod_basesin$cumul_paye+baseprod_basesin$Provision_depense-baseprod_basesin$Recours_exercice
baseprod_basesin$part_cummulé_payé_dans_la_Charge=baseprod_basesin$cumul_paye/baseprod_basesin$charge_totale

##--------------------------Creer la variable class--------------------#
baseprod_basesin$class_de_Robustesse[baseprod_basesin$part_cummulé_payé_dans_la_Charge>=0.7]="Class1"
#baseprod_basesin$class_de_Robustesse[baseprod_basesin$part_cummulé_payé_dans_la_Charge<0.6 & baseprod_basesin$part_cummulé_payé_dans_la_Charge<0.8 ]="class2"
baseprod_basesin$class_de_Robustesse[baseprod_basesin$part_cummulé_payé_dans_la_Charge<0.7]="Class2" 
#baseprod_basesin2$S_P=((baseprod_basesin$charge_totale)+(baseprod_basesin2$autres_charges))/(baseprod_basesin$pnette)
#baseprod_basesin2$charge_totale=baseprod_basesin2$cumul_paye+baseprod_basesin2$provision_depense_final
baseprod_basesin$class_de_Robustesse[is.na(baseprod_basesin$class_de_Robustesse)]=""
#--------------------Supprimer une variable
baseprod_basesin$pourcentage_ajustement=NULL
baseprod_basesin$Ratio_combiné=0
baseprod_basesin$pourcentage_ajustement=0
VecClass1=(baseprod_basesin$class_de_Robustesse=="Class1")
VecClass2=(baseprod_basesin$class_de_Robustesse=="Class2")
baseprod_basesin$charge_totale[is.na(baseprod_basesin$charge_totale)]=0
baseprod_basesin$pnette[is.na(baseprod_basesin$pnette)]=0
baseprod_basesin$Ratio_combiné[!VecClass2]=(baseprod_basesin$charge_totale[!VecClass2]+baseprod_basesin$Autres_charges[!VecClass2])/(baseprod_basesin$pnette[!VecClass2])
baseprod_basesin$Ratio_combiné[VecClass2]=((baseprod_basesin$cumul_paye[VecClass2]+0.8*baseprod_basesin$provision_depense[VecClass2])+(baseprod_basesin$Autres_charges[VecClass2]))/(baseprod_basesin$pnette[VecClass2])
baseprod_basesin$pourcentage_ajustement[!VecClass2]=(baseprod_basesin$charge_totale[!VecClass2]-((baseprod_basesin$Ratio_combiné[!VecClass2]-0.30)*baseprod_basesin$pnette[!VecClass2]))/((baseprod_basesin$pnette[!VecClass2])*(baseprod_basesin$Ratio_combiné[!VecClass2]-0.30))
baseprod_basesin$pourcentage_ajustement[VecClass2]=((baseprod_basesin$cumul_paye[VecClass2]+0.8*baseprod_basesin$provision_depense[VecClass2])-(baseprod_basesin$Ratio_combiné[VecClass2]-0.30)*baseprod_basesin$pnette[VecClass2])/((baseprod_basesin$pnette[VecClass2])*(baseprod_basesin$Ratio_combiné[VecClass2]-0.30))

#------------------------Creation base historique------------------------------------

baseprod_basesin$nbsin_précédent=""
baseprod_basesin$ratio_combiné_précédent=""
baseprod_basesin$Decision_provisoire=""
baseprod_basesin$Decision_finale=""
baseprod_basesin$Nouvelle_prime=""
baseprod_basesin$date_effet_pred=as.Date("",format = "%d/%m/%Y")
baseprod_basesin$date_fin_pred=as.Date("",format = "%d/%m/%Y")
BaseTravail=baseprod_basesin
colnames(BaseTravail)[colnames(BaseTravail)=="numeropolicecommun.x"] <- "numeropolicecommun"
{
  {  
    BaseTravail$RefRenouvellement=0
    BaseTravail$boolRenouveller=0
    BaseTravail$Nouvelle_prime=0
    BaseTravail$nbsin_précédent=0
    BaseTravail$ratio_combiné_précédent=0
    BaseTravail$date_effet_pred=as.Date("",format = "%d/%m/%Y")
    BaseTravail$date_fin_pred=as.Date("",format = "%d/%m/%Y")
    BaseTravail$IdUnique= c(1:dim(BaseTravail)[1])
    
    Existence_historique_2<-function(idUnique)
    {
      #idUnique=281
      
      NumeroPolice=BaseTravail$numeropolicecommun[idUnique]
      DateEffet=BaseTravail$date_effet[idUnique]
      #DateFin=BaseTravail$fin_garantie[idUnique]
      VecPolice = BaseTravail$numeropolicecommun
      VecDateEffet= BaseTravail$date_effet
      VecDatefin = BaseTravail$fin_garantie
      VecIdUnique =BaseTravail$IdUnique
      VecPrime=BaseTravail$pnette
      Vecnbsinpred=BaseTravail$nbsin
      VecRatioCom=BaseTravail$Ratio_combiné
      #VecChargesinpred=BaseTravail$charge_totale
      #VecautreChargespred=BaseTravail$Autres_charges
      res= Existence_historique(NumeroPolice,DateEffet,VecPolice,VecDateEffet,VecDatefin,VecIdUnique,VecPrime,Vecnbsinpred,VecRatioCom)
      #if (res$bool_renouveller)
      #{
      # BaseFusionPrime$boolRenouveller[idUnique]=res$bool_renouveller
      #BaseFusionPrime$RefRenouvellement[idUnique]=res$id_renouvellement
      #BaseFusionPrime$Prime[idUnique]=res$prime
      # }*%
      return(res)
      
    }
    
    #i=1
    ### executer sur la base
    for (i  in 1:18)
    {
      T1<-Sys.time()
      
      vec=c((10000*(i-1)+1):(10000*(i-1)+10000))
      #Resultat=lapply(BaseTravail$IdUnique[vec], Existence_renouvellement_2)
      #DF=as.data.frame(do.call(rbind, Resultat))
      #BaseTravail$RefRenouvellement[vec]=as.numeric(DF$id_renouvellement)
      #BaseTravail$boolRenouveller[vec]=as.numeric(DF$bool_renouveller)
      #BaseTravail$prime[vec]=as.numeric(DF$prime)
      Resultat2=sapply(BaseTravail$IdUnique[vec],Existence_historique_2)
      #DF=as.data.frame(do.call(rbind, Resultat))
      BaseTravail$RefRenouvellement[vec]=Resultat2[1,]#as.numeric(DF$id_renouvellement)
      BaseTravail$boolRenouveller[vec]=Resultat2[2,]#as.numeric(DF$bool_renouveller)
      BaseTravail$Nouvelle_prime[vec]=Resultat2[3,]#as.numeric(DF$prime)
      BaseTravail$nbsin_précédent[vec]=Resultat2[4,]
      BaseTravail$ratio_combiné_précédent[vec]=Resultat2[5,]
      BaseTravail$date_effet_pred[vec]=as.Date(as.numeric(Resultat2[6,]),origin = "1970-01-01")
      BaseTravail$date_fin_pred[vec]=as.Date(as.numeric(Resultat2[7,]),origin = "1970-01-01")
      T2<-Sys.time()
      Tdiff= difftime(T2, T1)
      Tdiff
    }
  }
  vec=c(180001:192173)
  
  Resultat2=sapply(BaseTravail$IdUnique[vec], Existence_historique_2)
  #DF=as.data.frame(do.call(rbind, Resultat))
  BaseTravail$RefRenouvellement[vec]=Resultat2[1,]#as.numeric(DF$id_renouvellement)
  BaseTravail$boolRenouveller[vec]=Resultat2[2,]#as.numeric(DF$bool_renouveller)
  BaseTravail$Nouvelle_prime[vec]=Resultat2[3,]#as.numeric(DF$prime)
  BaseTravail$nbsin_précédent[vec]=Resultat2[4,]
  BaseTravail$ratio_combiné_précédent[vec]=Resultat2[5,]
  BaseTravail$date_effet_pred[vec]=as.Date(as.numeric(Resultat2[6,]),origin = "1970-01-01")
  BaseTravail$date_fin_pred[vec]=as.Date(as.numeric(Resultat2[7,]),origin = "1970-01-01")
  
} 
BaseTravail$S_P=((BaseTravail$charge_totale))/(baseprod_basesin$pnette)

BaseTravail$date_effet_pred=as.Date(as.numeric(BaseTravail$date_effet_pred),origin = "1970-01-01")
BaseTravail$date_fin_pred=as.Date(as.numeric(BaseTravail$date_fin_pred),origin = "1970-01-01")

#-----------------------Repartition par Blocs de risk----------------------
BaseTravail$charge_totale[is.na(BaseTravail$charge_totale)]=0
BaseTravail$Ratio_combiné[is.na(BaseTravail$Ratio_combiné)]=0
BaseTravail$part_cummulé_payé_dans_la_Charge[is.na(BaseTravail$part_cummulé_payé_dans_la_Charge)]=0
BaseTravail$S_P[is.na(BaseTravail$S_P)]=0
BaseTravail$pourcentage_ajustement[is.na(BaseTravail$pourcentage_ajustement)]=0
BaseTravail$nbsin_précédent[is.na(BaseTravail$nbsin_précédent)]=0
BaseTravail$ratio_combiné_précédent[is.na(BaseTravail$ratio_combiné_précédent)]=0
BaseTravail$nbsin[is.na(BaseTravail$nbsin)]=0

BaseTravail$blokRisk=""
BaseTravail$blokRisk[(BaseTravail$codepartenaire=="Particulier" & BaseTravail$branche=="auto" & BaseTravail$produit!="Automobile Flotte")|(BaseTravail$codepartenaire=="Particulier" & 
                                                                                                                                             BaseTravail$branche=="Santé" & BaseTravail$pnette<2000000)|
                       ( BaseTravail$codepartenaire=="Particulier" & BaseTravail$branche=="Dommages corporels"& BaseTravail$produit!="COLLECTIVES DIVERSES" & BaseTravail$produit!="COLLECTIVES SCOLAIRES" &
                           BaseTravail$produit!="INDIVIDUELLE COLLECTIVE EMPLOYES")|(BaseTravail$codepartenaire=="Particulier" &
                                                                                       BaseTravail$branche=="Inc et DAB" & BaseTravail$pnette<=500000)|(BaseTravail$codepartenaire=="Particulier"&
                                                                                                                                                          BaseTravail$branche=="Responsabilité civile" & BaseTravail$pnette<=250000)]="Risque individuel"

BaseTravail$blokRisk[(BaseTravail$branche=="auto" & BaseTravail$codepartenaire!="Particulier")|(BaseTravail$codepartenaire!="Particulier" & 
                                                                                                  BaseTravail$branche=="Santé")|(BaseTravail$codepartenaire=="Particulier" &  BaseTravail$branche=="Santé" & BaseTravail$pnette>=2000000)|(BaseTravail$codepartenaire=="Particulier" &
                                                                                                                                                                                                                                             BaseTravail$branche=="Inc et DAB" & BaseTravail$pnette>500000)|
                       (BaseTravail$codepartenaire=="Particulier"& BaseTravail$branche=="Responsabilité civile" & BaseTravail$pnette >250000)|
                       (BaseTravail$branche=="Dommages corporels"|(BaseTravail$produit=="COLLECTIVES DIVERSES" | BaseTravail$produit=="COLLECTIVES SCOLAIRES"|
                                                                     BaseTravail$produit=="INDIVIDUELLE COLLECTIVE EMPLOYES"))]="Risque collectif"
BaseTravail$blokRisk[BaseTravail$blokRisk==""]="Gros risques"

str(BaseTravail)
#-----------------Implementation des décisions--------------------------------------
BaseTravail$nbsin_précédent=as.numeric(BaseTravail$nbsin_précédent)
BaseTravail$ratio_combiné_précédent=as.numeric(BaseTravail$ratio_combiné_précédent)
BaseTravail$boolRenouveller=as.numeric(BaseTravail$boolRenouveller)
BaseTravail$RefRenouvellement=as.numeric(BaseTravail$RefRenouvellement)
rep_tarif_commun="renouveler"
rep_renouv_l_etat="renouveler"
rep_analyse_autrRisk="A observer"
rep_ajustement="ajuster"
rep_resilier="Resilier"
BaseTravail$Decision_finale=""
BaseTravail$Decision_provisoire=""
#BaseTravail$pourcentage_ajustement=0

BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque individuel" & BaseTravail$branche!="Santé"
                                 & BaseTravail$Ratio_combiné<=1)]=rep_tarif_commun
BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque individuel" & BaseTravail$branche=="Santé"
                                 & BaseTravail$Ratio_combiné<=2)]=rep_tarif_commun
BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque individuel" & BaseTravail$branche!="Santé"
                                 & (BaseTravail$Ratio_combiné>1 & BaseTravail$nbsin<=2) & (BaseTravail$RefRenouvellement==0|BaseTravail$nbsin_précédent==0)
)]=rep_tarif_commun
BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque individuel" & BaseTravail$branche!="Santé"&
                                   BaseTravail$Ratio_combiné>1 & BaseTravail$nbsin<=2 & BaseTravail$nbsin_précédent>=1)
                                ]=rep_resilier
BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque individuel" & BaseTravail$branche!="Santé"
                                 & BaseTravail$Ratio_combiné>1 & BaseTravail$nbsin>=3) ]=rep_resilier
BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque individuel" & BaseTravail$branche=="Santé"
                                 & BaseTravail$Ratio_combiné>2)]=rep_resilier

BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque collectif" & BaseTravail$Ratio_combiné<=1  
)]=rep_tarif_commun

BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Risque collectif" & BaseTravail$Ratio_combiné>1 
)]=rep_analyse_autrRisk

BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Gros risques" & BaseTravail$Ratio_combiné<=1
)]=rep_renouv_l_etat

BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Gros risques" & BaseTravail$Ratio_combiné>1 & BaseTravail$nbsin==1
)]=rep_renouv_l_etat


BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Gros risques" & BaseTravail$Ratio_combiné>1 & BaseTravail$nbsin>=2 &  BaseTravail$nbsin_précédent<=1)]=rep_ajustement

BaseTravail$Decision_provisoire[(BaseTravail$blokRisk=="Gros risques" & BaseTravail$Ratio_combiné>1 & BaseTravail$nbsin>=2 & BaseTravail$nbsin_précédent>=2)]=rep_analyse_autrRisk

BaseTravail$pourcentage_ajustement[((!VecClass2) & BaseTravail$Decision_provisoire==rep_ajustement)]=
  (BaseTravail$charge_totale[((!VecClass2) & BaseTravail$Decision_provisoire==rep_ajustement)]/0.69-
     BaseTravail$pnette[((!VecClass2) & BaseTravail$Decision_provisoire==rep_ajustement)])/(BaseTravail$pnette[((!VecClass2) & BaseTravail$Decision_provisoire==rep_ajustement)])*1


BaseTravail$pourcentage_ajustement[(VecClass2 & BaseTravail$Decision_provisoire==rep_ajustement)]=
  ((BaseTravail$cumul_paye[(VecClass2 & BaseTravail$Decision_provisoire==rep_ajustement)]+0.8*(BaseTravail$provision_depense[(VecClass2 & BaseTravail$Decision_provisoire==rep_ajustement)]))/0.69-
     BaseTravail$pnette[(VecClass2 & BaseTravail$Decision_provisoire==rep_ajustement)])/(BaseTravail$pnette[(VecClass2 & BaseTravail$Decision_provisoire==rep_ajustement)])*1
BaseTravail$Nouvelle_prime=0
BaseTravail$Nouvelle_prime[BaseTravail$Decision_provisoire!=rep_resilier]=BaseTravail$pnette[BaseTravail$Decision_provisoire!=rep_resilier]*((1+BaseTravail$pourcentage_ajustement[BaseTravail$Decision_provisoire!=rep_resilier]/1))
BaseTravail$Decision_finale[(BaseTravail$Decision_provisoire==rep_renouv_l_etat | BaseTravail$Decision_provisoire==rep_tarif_commun)]=BaseTravail$Decision_provisoire[(BaseTravail$Decision_provisoire==rep_renouv_l_etat | BaseTravail$Decision_provisoire==rep_tarif_commun)]
BaseTravail$primeTTC=BaseTravail$taxe+BaseTravail$pnette+BaseTravail$cp 
BaseTravail[nchar(BaseTravail$numeropolicecommun)!=9,]$numeropolicecommun=str_sub(BaseTravail[nchar(BaseTravail$numeropolicecommun)!=9,]$newPol,1,15)
#-----------------Definir le mois d'échéance--------------------------------------
#mois=as.Date("30/09/2018",format = "%d/%m/%Y")
baseEcheanceDec2018=BaseTravail[((BaseTravail$branche=="Inc et DAB"|BaseTravail$branche=="Dommages corporels"|BaseTravail$branche=="Santé"|
                                         BaseTravail$branche=="Responsabilité civile"|BaseTravail$branche=="Transport"|BaseTravail$branche=="auto")  &
                                        BaseTravail$fin_garantie>="2018-12-01" & BaseTravail$fin_garantie<="2018-12-31") ,]


colnames(baseEcheanceDec2018)
VecImportant=c("Identifiant","numeropolicecommun","ancienPol","codeintermediaire","intermediaire","codepartenaire","branche","client",
               "produit","date_effet","fin_garantie","Duree","primeTTC","cp","taxe","pnette","cumul_paye","provision_depense","commission","Autres_charges","charge_totale","nbsin","nbprime", 
               "part_cummulé_payé_dans_la_Charge","Ratio_combiné","S_P","class_de_Robustesse","anomalie","nbsin_précédent","ratio_combiné_précédent",
               "boolRenouveller","RefRenouvellement","blokRisk","Decision_provisoire","pourcentage_ajustement","Decision_finale","Nouvelle_prime")

VecImportant1=c("numeropolicecommun.x","ancienPol","newPol","branche","client","intermediaire","date_effet","fin_garantie","pnette","cumul_paye","charge_totale","nbsin","Autres_charges")
baseRenouvellement=baseprod_basesin[,VecImportant1]
#???-----------------------Extraction------------------------------------------------------------------------
write.table(baseprod_basesin,file = "ProdSin_310818.csv",sep=";",dec=",", row.names = FALSE)
write.table(Basepropre_311018,file = "base_verification_Dec18.csv",sep=";",dec=",", row.names = FALSE)
write.table(baseprod_basesin2,file = "data_renouvellement_310718.csv",sep=";",dec=",", row.names = FALSE)
write.table(BaseTravail,file = "BaseTravailSept2018.csv",sep=";",dec=",", row.names = FALSE)
write.table(baseEcheanceDec2018,file = "baseEcheanceDec2018.csv",sep=";",dec=",", row.names = FALSE)
write.table(sinistre,file = "sinistre_310718.csv",sep=";",dec=",", row.names = FALSE)
write.table(prime,file = "prime_310718.csv",sep=";",dec=",", row.names = FALSE)
write.table(baseanomalie,file = "base_anomalie.csv",sep=";",dec=",", row.names = FALSE)
write.csv2(baseRenouvellement,file = "baseRenouvellement2018.csv",sep=" ",eol = "\n", na = "NA", dec = ".", row.names = FALSE)

