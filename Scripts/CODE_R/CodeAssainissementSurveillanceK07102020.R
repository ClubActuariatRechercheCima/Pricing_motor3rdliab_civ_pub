chemin_base= "<CHEMIN_LOCAL>/"
Chemin_resultat= paste(chemin_base, "/Livrables100920/",sep="")
require(data.table)
require(lubridate)
require(stringr)
require(RMySQL)
require(compare)
require(stringdist)
require(plyr)
require(reshape2)
setwd(chemin_base)
library(dplyr)
source('code/Bibliotheques_Fonctions_03102017.R')
#options(java.parameters = "Xmx8000m")
#Sys.setenv(JAVA_HOME='<CHEMIN_LOCAL>/')
#library(openxlsx)
#--------------------------------------------------------------------
Sys.setenv(TZ='GMT')
Liste_Fichiers <- list.files(path = "Data Production")

##-----Importer les bases production----------------##############
for (i in 1:length(Liste_Fichiers))
{
  
  Nom = paste("Table",i,sep="")
  assign( Nom,read.csv2(paste0("Data Production/",Liste_Fichiers[i]), sep = ";", stringsAsFactors = FALSE ))
  if("anciennum" %in% colnames(get(paste0("Table",i)))){
    k <- get(paste0("Table",i)) %>% dplyr::rename("ancienpol"="anciennum")
    assign(Nom,k)
  }
  if("categ_interm" %in% colnames(get(paste0("Table",i)))){
    k <- get(paste0("Table",i)) %>% dplyr::rename("typeinter"="categ_interm")
    assign(Nom,k)
  }
}

#-------------Trouver les colonnes identiques--------------#
liste_colname=colnames(Table1)
for (i in 2:length(Liste_Fichiers))
{
  
  liste_colname = intersect(liste_colname, colnames(get(paste0("Table",i)))) 
  
}
#---------------- fusionner les bases production-----------#
BaseFusionPrime=get(paste("Table",1,sep=""))[,liste_colname]

for (i in 2:length(Liste_Fichiers))
{
  BaseFusionPrime=rbind(BaseFusionPrime,get(paste0("Table",i))[,liste_colname])
}
#write.table(BaseFusionPrime,file = paste0(Chemin_resultat,"BaseFusionPrime.csv"),sep=";",dec=",", row.names = FALSE)
#BaseFusionPrime <- read.csv2(paste0(Chemin_resultat,"BaseFusionPrime.csv"),  stringsAsFactors = FALSE )

write.table(BaseFusionPrime,file = paste0(Chemin_resultat,"check.csv"),sep=";",dec=",", row.names = FALSE)

#importation branchGoog
branchGood <- read.csv2("Livrables100920/branchGood.csv",header = T,sep = ";")

BaseFusionPrime$branche_prime = branchGood$branche_good[match(BaseFusionPrime$branche,branchGood$branche)]
#Consolidation branche
BaseFusionPrime$branche_propre = fifelse(is.na(BaseFusionPrime$branche_prime), 
                                         BaseFusionPrime$branche, BaseFusionPrime$branche_prime)

BaseFusionPrime$branche_propre = as.factor(BaseFusionPrime$branche_propre)
levels(BaseFusionPrime$branche_propre) = c("Assistance","Inc et DAB","Inc et DAB","Dommages corporels","Inc et DAB","Sante","Responsabilite civile","Responsabilite civile","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","auto","auto","auto")
write.table(BaseFusionPrime,file = paste0(Chemin_resultat,"check.csv"),sep=";",dec=",", row.names = FALSE)

#renommer branche
BaseFusionPrime$branche = BaseFusionPrime$branche_propre

BaseFusionPrime$coass[is.na(BaseFusionPrime$coass)]=0
BaseFusionPrime$coass <- as.numeric(gsub(",", ".", gsub("\\s+", "", BaseFusionPrime$coass)))
BaseFusionPrime$pnette[is.na(BaseFusionPrime$pnette)]=0
BaseFusionPrime$pnette <- as.numeric(gsub(",", ".", gsub("\\s+", "", BaseFusionPrime$pnette)))
BaseFusionPrime$commission = as.numeric(gsub(",", ".", gsub("\\s+", "", BaseFusionPrime$commission)))
BaseFusionPrime$taxe <- as.numeric(gsub(",", ".", gsub("\\s+", "", BaseFusionPrime$taxe)))
BaseFusionPrime$cp <- as.numeric(gsub(",", ".", gsub("\\s+", "", BaseFusionPrime$cp)))
BaseFusionPrime$primeTTC=BaseFusionPrime$pnette+BaseFusionPrime$cp+BaseFusionPrime$taxe
BaseFusionPrime$nbprime=1
BaseFusionPrime$date_facture=as.Date(BaseFusionPrime$date_facture,format = "%d/%m/%Y")
BaseFusionPrime$periode=as.Date(BaseFusionPrime$periode,format = "%d/%m/%Y")
BaseFusionPrime$date_effet=as.Date(BaseFusionPrime$date_effet,format = "%d/%m/%Y")
BaseFusionPrime$fin_garantie=as.Date(BaseFusionPrime$fin_garantie,format = "%d/%m/%Y")
#-----------------retraiter les numeros de police de la base fusionnée-----------
BaseFusionPrime$policeclone <- str_trim(BaseFusionPrime$police)
BaseFusionPrime$policeclone <- gsub("\\s+","", gsub("-", "K", BaseFusionPrime$policeclone))
BaseFusionPrime$policeclone <- str_sub(BaseFusionPrime$policeclone,1,15)

####----------------recodification des typeintermediare-------------
colnames(BaseFusionPrime)[colnames(BaseFusionPrime)=="typeinter"] <- "CodeIntermediaire"
BaseFusionPrime$CodeIntermediaire= as.factor(BaseFusionPrime$CodeIntermediaire)

levels(BaseFusionPrime$CodeIntermediaire) = c("Bureaux Direct","Courtiers","Agences","Agences","Agences", "Bancassurance","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Bureaux Direct","Courtiers","Courtiers","Courtiers","Bureaux Direct")
BaseFusionPrime$CodeIntermediaire=ordered(BaseFusionPrime$CodeIntermediaire,c("Courtiers","Bureaux Direct","Agences","Bancassurance"))
###-----------------recodification de code partenaire----------------
BaseFusionPrime$codepartenaire= as.factor(BaseFusionPrime$codepartenaire)
levels(BaseFusionPrime$codepartenaire) = c("autres","autres","Particulier","Entreprise","Entreprise","Particulier","Particulier","Entreprise")
###-----------------recodification de code branche--------------------
#BaseFusionPrime$branche <- str_replace_all(BaseFusionPrime$branche, "[[:punct:]]", "")
BaseFusionPrime$branche <- str_replace_all(BaseFusionPrime$branche, "[^[:alnum:]]", " ")


###------------------------Table Intermediaire au secours pour traiter la colonne intermediaire de basefusion--------------------------------------
tb_intermediaire=read.csv2("Intermediaire/table_intermediaire.csv",  stringsAsFactors = FALSE)
BaseFusionPrime$intermediaire=str_trim(BaseFusionPrime$intermediaire)
tb_intermediaire$Intermediaire.SUNU.PAC=str_trim(tb_intermediaire$Intermediaire.SUNU.PAC)
BaseFusionPrime$intermediairegood=tb_intermediaire$Intermediairegood[match(BaseFusionPrime$intermediaire,tb_intermediaire$Intermediaire.SUNU.PAC)]
BaseFusionPrime[is.na(BaseFusionPrime$intermediairegood),]$intermediairegood=BaseFusionPrime[is.na(BaseFusionPrime$intermediairegood),]$intermediaire
###------------------- Les polices uniques ---------------------------- ####

Basepropre_0820 <- BaseFusionPrime
####----------Traitement des colonnes : Intermediaire, Client, Police-----------------
###-----------Client d'abord----------------------------------------------------------
Basepropre_0820$client=str_trim(Basepropre_0820$client)
tbclient=data.frame(Basepropre_0820$policeclone,Basepropre_0820$client,Basepropre_0820$codepartenaire)
colnames(tbclient)=c("policeclone","client","codepartenaire")
###----------------------------Intermediaire maintenat------------------------------
Basepropre_0820$intermediaire=str_trim(Basepropre_0820$intermediaire)
tbintermediaire=data.frame(Basepropre_0820$policeclone,Basepropre_0820$client,Basepropre_0820$intermediaire)
colnames(tbintermediaire)=c("policeclone","client","intermediaire")
#tbintermediaire$intermediaire1=Basepropre_0820$intermediaire[match(tbintermediaire$numeropolicecommun,Basepropre_0820$numeropolicecommun)]
tbintermediaire=tbintermediaire[!(duplicated(tbintermediaire$intermediaire)),]
Basepropre_0820$Identifiant=paste0(Basepropre_0820$policeclone,as.numeric(Basepropre_0820$date_effet),as.numeric(Basepropre_0820$fin_garantie))
Basepropre_0820$Duree=DureePolice(Basepropre_0820$date_effet,Basepropre_0820$fin_garantie)
Basepropre_0820$AnneePolice=Exposition(Basepropre_0820$date_effet,Basepropre_0820$fin_garantie)
Basepropre_0820$anneeffet <- ExtractionAnnee(Basepropre_0820$date_effet)
write.table(Basepropre_0820,file = paste0(Chemin_resultat,"basepropreprod_1320.csv"),sep=";",dec=",", row.names = FALSE)

#----------------Faire une somme des contrat en doublons--------------------#
Vec_Variable = c("primeTTC","pnette","coass", "cp","commission","nbprime","taxe")
by_variable = c("Identifiant","policeclone","date_effet","fin_garantie")
Basepropre1 <- aggregate(Basepropre_0820[,Vec_Variable],by=Basepropre_0820[,by_variable], FUN = sum, na.rm=TRUE)
BasePropreProd=Basepropre1
BasePropreProd$Duree=Basepropre_0820$Duree[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$AnneePolice=Basepropre_0820$AnneePolice[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$client=Basepropre_0820$client[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$codepartenaire=Basepropre_0820$codepartenaire[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$Intermediaire=Basepropre_0820$intermediairegood[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$branche=Basepropre_0820$branche[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$codeintermediaire=Basepropre_0820$CodeIntermediaire[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$produit=Basepropre_0820$produit[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$ancienpol=Basepropre_0820$ancienpol[match(BasePropreProd$Identifiant,Basepropre_0820$Identifiant)]
BasePropreProd$anneeffet = lubridate::year(BasePropreProd$date_effet)
table(BasePropreProd$branche)
BasePropreProd$branche=as.character(BasePropreProd$branche)
write.table(BasePropreProd,file = paste0(Chemin_resultat,"basepropreprod_agg_par_ID1.csv"),sep=";",dec=",", row.names = FALSE)

# verification
vecagg1=c("anneeffet","branche")
#vecagg2=c("pnette","commission","cp")
vecagg2=c("pnette")
BasePropreProd_ag= aggregate(BasePropreProd[,vecagg2],by=BasePropreProd[,vecagg1], FUN = sum, na.rm=TRUE)
colnames(BasePropreProd_ag)
colnames(BasePropreProd_ag)[3]="pnette"
write.table(BasePropreProd_ag,file = paste0(Chemin_resultat,"basepropreprod_agg_par_anneeeffet.csv"),sep=";",dec=",", row.names = FALSE)

###### ---------------------- Indiquer les identifiants anormales -------------------- ###
############# Ajout de la base Insto a la base production
tb_Insto <- read.csv2("baseinsto_to_joinBassepropre2.csv")
tb_Insto$Date.de.survenance=as.Date(tb_Insto$Date.de.survenance,format = "%d/%m/%Y")
tb_Insto$Date.effet=as.Date(tb_Insto$Date.effet,format = "%d/%m/%Y")
tb_Insto$Date.echeance=as.Date(tb_Insto$Date.echeance,format = "%d/%m/%Y")
tb_Insto$new_date_effet=as.Date(tb_Insto$new_date_effet,format = "%d/%m/%Y")
tb_Insto$new_date_eheance=as.Date(tb_Insto$new_date_eheance,format = "%d/%m/%Y")
tb_Insto$Duree=DureePolice(tb_Insto$new_date_effet,tb_Insto$new_date_eheance)
tb_Insto$AnneePolice=Exposition(tb_Insto$new_date_effet,tb_Insto$new_date_eheance)
tb_Insto$Branche= as.factor(tb_Insto$Branche)
tb_Insto$numero_sinistre= str_trim(tb_Insto$numero_sinistre)
tb_Insto$numero_sinistre=str_to_upper(tb_Insto$numero_sinistre)
tb_Insto$policeclone=tb_Insto$numero_sinistre

levels(tb_Insto$Branche) = c("auto","Assistance","Inc et DAB","Dommages corporels","Inc et DAB","Responsabilité civile","auto")
insto <- tb_Insto %>% dplyr::rename(branche=Branche, client=Assure,pnette=`new.PN`,dateeffet=`new_date_effet`,datefineffet = `new_date_eheance`, ancienpol=Police, 
                                    provision_fin = `Provision.insto.révisé`, intermediaire = Intermediaire)%>% 
  dplyr::mutate( commission="", nbprime=1, parcoas1="", mois_effet="", taxe="",primeTTC="", codeintermediaire="",produit="",coass="",
                 codepartenaire="", sinister_id="", cp="", idreas="")
insto$Identifiant=paste0(insto$numero_sinistre,as.numeric(insto$dateeffet),as.numeric(insto$datefineffet))

Basepropre_0820 <- Basepropre_0820 %>% dplyr::rename(dateeffet=date_effet,datefineffet=fin_garantie)
list_col1 <- intersect(names(insto),names(Basepropre_0820))
base_prod1320_insto = rbind(Basepropre_0820[,list_col1],insto[,list_col1])
write.table(base_prod1320_insto,file = paste0(Chemin_resultat,"base_prod_insto.csv"),sep=";",dec=",", row.names = FALSE)

#                ************* Traitement sur production **********

# On remplace les ancpol vide par police

prodPropre_sauvegarde[nchar(prodPropre_sauvegarde$ancienpol) != 9,"ancienpol"] = prodPropre_sauvegarde[nchar(prodPropre_sauvegarde$ancienpol) != 9,"policeclone"]

#Verify
table(nchar(prodPropre_sauvegarde$ancienpol))
#creation id1 et 2
#ID2
prodPropre_sauvegarde$id_avenant2 = paste0(prodPropre_sauvegarde$ancienpol,
                                           prodPropre_sauvegarde$datefineffet)

#ID1
prodPropre_sauvegarde$id_avenant1 = prodPropre_sauvegarde$Identifiant


##### **************** CREATION BASE POLICE ***********************************************
prodPropre_sauvegarde <- base_prod1320_insto
# Aggregation par date effet  et dateEcheance
# Ici on garde tous les chps calculés
effet_echean_grpby = prodPropre_sauvegarde %>% 
  dplyr::group_by(ancienpol) %>% 
  dplyr::summarise(dte_premier_effet     = min(dateeffet, na.rm = T),
                   dte_dernier_effet = max(datefineffet, na.rm = T),
                   prime        = sum(pnette, na.rm = T),
                   commission   = sum(as.numeric(commission), na.rm = T),
                   primeTTC     = sum(as.numeric(primeTTC), na.rm = T),
                   taxe         = sum(as.numeric(taxe), na.rm = T),
                   cp           = sum(as.numeric(cp), na.rm = T),
                   coass        = sum(as.numeric(coass), na.rm = T),
                   Duree        = sum(Duree, na.rm = T),
                   AnneePolice  = sum(AnneePolice, na.rm = T)
                   
  )

#Rajout des autres colonnes
#client
effet_echean_grpby$client = prodPropre_sauvegarde$client[match(effet_echean_grpby$ancienpol, prodPropre_sauvegarde$ancienpol)]

#branche
effet_echean_grpby$branche = prodPropre_sauvegarde$branche[match(effet_echean_grpby$ancienpol, prodPropre_sauvegarde$ancienpol)]

#codepartenaire
effet_echean_grpby$codepartenaire = prodPropre_sauvegarde$codepartenaire[match(effet_echean_grpby$ancienpol, prodPropre_sauvegarde$ancienpol)]


#produit
effet_echean_grpby$produit = prodPropre_sauvegarde$produit[match(effet_echean_grpby$ancienpol, prodPropre_sauvegarde$ancienpol)]

#Creation prime moyenne
effet_echean_grpby$primeMoy = effet_echean_grpby$prime/effet_echean_grpby$AnneePolice 

# baseprod_propre <- BasePropreProd %>% dplyr::rename(dateeffet=date_effet,datefineffet=fin_garantie)
# 
# list_col1 <- intersect(names(insto),names(baseprod_propre))
# base_prod_insto = rbind(baseprod_propre[,list_col1],insto[,list_col1])
# write.table(base_prod_insto,file = paste0(Chemin_resultat,"base_prod_insto_agg.csv"),sep=";",dec=",", row.names = FALSE)

#----------------------------------------------------------------------------------------------------
#--------------------------------Traitement de la Base sinistre------------------------------------------
#--------------------------------------------------------------------------------------------------------
basesinistre=read.csv2("Base_sinistre/ip_detaillee_vu_31072020.csv")

basesinistre <- basesinistre %>% dplyr::rename(codepartenaire="typinter")
# filtre sur les données de 2013 à juillet 2020
basesinistre=basesinistre[basesinistre$an_surv >= 2013,]
basesinistre$ancpolice= str_trim(basesinistre$ancpolice)
basesinistre$policeclone <- str_trim(basesinistre$police)
basesinistre$cumul_paye <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$cumul_paye)))
basesinistre$provision_fin <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$provision_fin)))
basesinistre$prevision_fin <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$prevision_fin)))
basesinistre$mt_eval <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$mt_eval)))
basesinistre$eval_recours <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$eval_recours)))
basesinistre$cumul_recours <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$cumul_recours)))
basesinistre$reglement_exercice <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$reglement_exercice)))
basesinistre$recours_exercice <- as.numeric(gsub(",", ".", gsub("\\s+", "", basesinistre$recours_exercice)))
basesinistre$dateeffet=as.Date(basesinistre$dateeffet,format = "%d/%m/%Y")
basesinistre$datefineffet=as.Date(basesinistre$datefineffet,format = "%d/%m/%Y")
basesinistre$declaration=as.Date(basesinistre$declaration,format = "%d/%m/%Y")

###-----------------recodification de code branche--------------------
basesinistre$branche= as.factor(basesinistre$branche)
levels(basesinistre$branche) = c("Inc et DAB","Dommages corporels","Inc et DAB","Santé","Responsabilité civile","Transport","auto")



basesinistre$policeclone <- gsub("\\s+","", gsub("-", "K", basesinistre$policeclone))
basesinistre$policeclone <- str_sub(basesinistre$policeclone,1,15)
basesinistre$nbsin=1
basesinistre$Identifiant <- paste0(basesinistre$policeclone,as.numeric(basesinistre$dateeffet),as.numeric(basesinistre$datefineffet))


############################ Verification
vecagg3=c("an_surv","branche")
#vecagg2=c("pnette","commission","cp")
vecagg4=c("cumul_paye","provision_fin")
basesinistre_ag = aggregate(basesinistre[,vecagg4],by=basesinistre[,vecagg3], FUN = sum, na.rm=TRUE)
write.table(basesinistre_ag,file = paste0(Chemin_resultat,"basesinistre_ag.csv"),sep=";",dec=",", row.names = FALSE)
############################

# ajout boni mali
Diff1 <- read.csv2("Differentiel Provision_v2_06102020.csv")
#Diff1 <- Diff1 %>% dplyr::rename("numeropolicecommun"="ancpolice")
tmp1 <- basesinistre
Diff1$survenance <- as.Date(Diff1$survenance,format = "%d/%m/%Y")
tmp1$survenance <- as.Date(tmp1$survenance,format = "%d/%m/%Y")
tmp1$cleprjoinfrboni <- paste0(tmp1$numero_sinistre,as.numeric(tmp1$survenance))
Diff1$cleprjoinfrboni <- paste0(Diff1$numero_sinistre,as.numeric(Diff1$survenance))
#tmp1 <- merge(tmp1,Diff1, by.x =c("numero_sinistre","dateeffet"),by.y= c("numero_sinistre","dateeffet"),all.x = T)
tmp1$boni_mali <- Diff1$Differentiel.1.officiel[match(tmp1$cleprjoinfrboni,Diff1$cleprjoinfrboni)]
write.table(tmp1,file = paste0(Chemin_resultat,"tmp1_boni_mali.csv"),sep=";",dec=",", row.names = FALSE)
#
#tmp4 <- merge(BasePropresin1,Diff1, by = c("intermediaire","client","dateeffet","datefineffet"), all.x=TRUE, all.y=F)
tmp1$provision_fin = as.numeric(tmp1$provision_fin)
tmp1$boni_mali = as.numeric(tmp1$boni_mali)
SumDouble(Diff1$Differentiel.1.officiel,na.rm = T)
SumDouble(tmp1$boni_mali,na.rm = T)+SumDouble(tmp1$provision_fin)

ctrl <- setdiff(Diff1$cleprjoinfrboni,tmp1$cleprjoinfrboni)
som_bon_mali <- SumDouble(Diff1$Differentiel.1.officiel,na.rm = T)

tmp1$provision_fin <- rowSums(tmp1[,c("provision_fin", "boni_mali")], na.rm=TRUE)
SumDouble(tmp1$provision_fin)

############################### Verification
# Aggregation svt branche et ansurv avt ajout sante
vecagg3=c("an_surv","branche")
vecagg4=c("cumul_paye","provision_fin", "nbsin")
basesinistre_apres_boni_mali = tmp1
basesinistre_apres_boni_mali[is.na(basesinistre_apres_boni_mali$nbsin),"nbsin"] = 1

basesinistre_apres_boni_mali_agg = aggregate(basesinistre_apres_boni_mali[,vecagg4],by=basesinistre_apres_boni_mali[,vecagg3], FUN = sum, na.rm=TRUE)
write.table(basesinistre_apres_boni_mali_agg,file = paste0(Chemin_resultat,"basesinistre_apres_boni_mali_agg.csv"),sep=";",dec=",", row.names = FALSE)
################################


# Remplacement charge sinistre sante 2019 
sante <- read.csv2("consolidé all_santé_2019_for replacement.csv")
sante$Sinistres <- gsub('\\s+', '', sante$Sinistres)
sante$Sinistres <- as.numeric(sante$Sinistres)

#sante$Sinistres <- as.numeric(gsub(",", ".", gsub("\\s+", "", sante$Sinistres)))
# Identification des lignes sinistre maladie 2019 à remplacer ds la base sinstre
tmp1$sante_remplace <- ((tmp1$an_surv==2019) & tmp1$ancpolice %in% sante$Police)
SumDouble(tmp1[tmp1$sante_remplace,]$cumul_paye,na.rm = T)+SumDouble(tmp1[tmp1$sante_remplace,]$provision_fin,na.rm = T)
SumDouble(tmp1$cumul_paye,na.rm = T)

# Scission de la base sinistre
tmp1_aremplace <- tmp1[tmp1$sante_remplace,]
tmp1_agarder <- tmp1[!tmp1$sante_remplace,]
sante <- sante %>% dplyr::rename(client = Client, intermediaire = Intermediaire,
                                 ancpolice = Police, cumul_paye = Sinistres) %>% 
                    dplyr::select(-c(Primes)) 

sante$an_surv = "2019"
sante$branche = "Santé"

SumDouble(tmp1_agarder$cumul_paye, na.rm = T) + SumDouble(sante$cumul_paye, na.rm = T)
#65756502582

#Creation colonne de sante
tmpDf = data.frame(matrix(ncol = length(setdiff(names(tmp1_agarder), names(sante))), nrow = nrow(sante)))
names(tmpDf) = setdiff(names(tmp1_agarder), names(sante))

 


sante_New = cbind(tmpDf, sante) 
#•Creation cle sante
sante_New$numero_sinistre = paste0("INSTOSANTE", "_", 1:nrow(sante))

#Rbind sante avec sinsitre
tmp_ajuste = rbind(tmp1_agarder, sante_New)
SumDouble(tmp_ajuste$cumul_paye, na.rm = T)# 6 313 576 716
write.table(tmp_ajuste,file = paste0(Chemin_resultat,"tmp_ajuste.csv"),sep=";",dec=",", row.names = FALSE)

SumDouble(tmp_ajuste$cumul_paye) # Montant cumul payé : 65 766758526

#Omission des colonnes initule
tmp_ajuste = tmp_ajuste %>% dplyr::select(-c("cleprjoinfrboni","sante_remplace" ,"boni_mali"))


########################## Verification
# Aggregation svt branche et ansurv avt ajout sante
vecagg3=c("an_surv","branche")
vecagg4=c("cumul_paye","provision_fin", "nbsin")
basesinistre_apres_sante_2019 = tmp_ajuste
SumDouble(basesinistre_apres_sante_2019$cumul_paye, na.rm = T)

basesinistre_apres_sante_2019[is.na(basesinistre_apres_sante_2019$nbsin),"nbsin"] = 1

#Verify
SumDouble(basesinistre_apres_sante_2019$cumul_paye)

basesinistre_apres_sante_2019_agg = aggregate(basesinistre_apres_sante_2019[,vecagg4],by=basesinistre_apres_sante_2019[,vecagg3], FUN = SumDouble, na.rm=TRUE)
SumDouble(basesinistre_apres_sante_2019_agg$cumul_paye, na.rm = T)

write.table(basesinistre_apres_sante_2019_agg,file = paste0(Chemin_resultat,"basesinistre_apres_sante_2019_agg.csv"),sep=";",dec=",", row.names = FALSE)
#####################################################


#Creation de base insto a concatener a sinistre
insto_nv <- insto
#Renommer les champs de insto a ajouter a base sinistre
insto_nv = insto_nv %>% dplyr::rename(intermediaire = Intermediaire,
                               survenance =`Date.de.survenance`
                                ) %>% 
                      dplyr::mutate(an_surv = ExtractionAnnee(survenance))

insto_nv = insto_nv[, intersect(names(tmp_ajuste), names(insto_nv))]


#difference de colonne
setdiff(names(tmp_ajuste), names(insto_nv))
intersect(names(tmp_ajuste), names(insto_nv))

tmpDf1 = data.frame(matrix(ncol = length(setdiff(names(tmp_ajuste), names(insto_nv))), nrow = nrow(insto_nv)))
names(tmpDf1) = setdiff(names(tmp_ajuste), names(insto_nv))

#On concatene
instoFinale = cbind(insto_nv, tmpDf1)
SumDouble(instoFinale$provision_fin, na.rm = T) + SumDouble(tmp_ajuste$provision_fin,na.rm = T)
# Verification : 20260960451

# Fusion de instoFinale et tmp_ajuster
fusion_insto_tmpajust = rbind(tmp_ajuste, instoFinale)
write.table(fusion_insto_tmpajust,file = paste0(Chemin_resultat,"fusion_insto_tmpajust_avt_agg.csv"),sep=";",dec=",", row.names = FALSE)

#Verification
SumDouble(fusion_insto_tmpajust$provision_fin, na.rm = T) %>% prettyNum(" ")
# "20 260 960 451"

############ Verification
# Aggregation svt branche et anssurv
vecagg3=c("an_surv","branche")
vecagg4=c("cumul_paye","provision_fin", "nbsin")
fusion_insto_tmpajust[is.na(fusion_insto_tmpajust$nbsin),"nbsin"] = 1

basesinistre_inst_agg = aggregate(fusion_insto_tmpajust[,vecagg4],by=fusion_insto_tmpajust[,vecagg3], FUN = sum, na.rm=TRUE)
write.table(basesinistre_inst_agg,file = paste0(Chemin_resultat,"fusion_insto_sin_agg.csv"),sep=";",dec=",", row.names = FALSE)
###########################




#**************************************************************************
# -------------------- SUITE TRAITEMENT DES BASES PROD ET SINISTRES
#Sauvegarde bases

sins_inst_sauvegarde  <- fusion_insto_tmpajust


#                 ************ Traitement sur sinistre   ************

#Creation id1 et id2
#ID2
table(nchar(sins_inst_sauvegarde$ancpolice))

#remplacer les ancpol de taille diff de 9 !
sins_inst_sauvegarde[nchar(sins_inst_sauvegarde$ancienpol) != 9,"ancienpol"] = sins_inst_sauvegarde[nchar(sins_inst_sauvegarde$ancienpol) != 9,"policeclone"]
sins_inst_sauvegarde$id_avenant2 = paste0(sins_inst_sauvegarde$ancpolice,
                                           as.numeric(sins_inst_sauvegarde$datefineffet))

#ID1
sins_inst_sauvegarde$id_avenant1 = sins_inst_sauvegarde$Identifiant

#Flagguer les sinistres graves
sins_inst_sauvegarde$charges_sinistre = rowSums(sins_inst_sauvegarde[,c("provision_fin", "cumul_paye")],
                                             na.rm = T) - dplyr::coalesce(sins_inst_sauvegarde$cumul_recours, 0)


#Reliqualification sinistre
sins_inst_sauvegarde$CSgrave <- fifelse(sins_inst_sauvegarde$charges_sinistre >= 30000000,"grave","mineur")
sins_inst_sauvegarde$CSgrave <- fifelse(sins_inst_sauvegarde$branche == "Santé", "", sins_inst_sauvegarde$CSgrave)

#Calucul charge net de recours
sins_inst_sauvegarde$charge_totale_net_recours = dplyr::coalesce(sins_inst_sauvegarde$cumul_paye,0) + 
                                                  dplyr::coalesce(sins_inst_sauvegarde$provision_fin,0) - 
                                                  dplyr::coalesce(sins_inst_sauvegarde$recours_exercice,0)
#part_cummulé_payé_dans_la_Charge
sins_inst_sauvegarde$part_cummule_paye_dans_la_Charge = sins_inst_sauvegarde$cumul_paye/sins_inst_sauvegarde$charges_sinistre

#Rajout date survenance sante
sins_inst_sauvegarde[sins_inst_sauvegarde$branche == "Santé","survenance"] = paste0("01/06/",
                                                                                  sins_inst_sauvegarde[sins_inst_sauvegarde$branche == "Santé","an_surv"] )
#Colonne nbre de sinistre grave
sins_inst_sauvegarde$nb_grave = fifelse(sins_inst_sauvegarde$CSgrave == "grave", 1,0)
sins_inst_sauvegarde$charge_grave = sins_inst_sauvegarde$nb_grave* sins_inst_sauvegarde$charges_sinistre


#Enregistrement des bases
write.table(prodPropre_sauvegarde,file = paste0(Chemin_resultat,"prodPropre_avec_id1&id2.csv"),sep=";",dec=",", row.names = FALSE)
write.table(sins_inst_sauvegarde,file  = paste0(Chemin_resultat,"sins_inst_avec_id1&id2.csv"),sep=";",dec=",", row.names = FALSE)



#Aggregation par date effet  et dateEcheance
# Ici on garde tous les chps calculés
effet_echean_grpby_sin = sins_inst_sauvegarde %>% 
  dplyr::group_by(ancpolice) %>% 
  dplyr::summarise(dte_premier_sin  = min(as.Date(dateeffet), na.rm = T),
                   dte_dernier_sin = max(as.Date(datefineffet), na.rm = T),
                   cumul_paye   = sum(cumul_paye, na.rm = T),
                   provision_fin = sum(as.numeric(provision_fin), na.rm = T),
                   charges_sinistre = sum(as.numeric(charges_sinistre), na.rm = T),
                   nbsin           = sum(as.numeric(nbsin), na.rm = T),
                   nb_grave = sum(nb_grave,na.rm = T),
                   charge_grave = sum(charge_grave, na.rm = T)
                   )

#Ajout autres colonne
#$client = sins_inst_sauvegarde$client[match(effet_echean_grpby_sin$ancpolice, sins_inst_sauvegarde$ancpolice)]
#effet_echean_grpby_sin$branche = sins_inst_sauvegarde$branche[match(effet_echean_grpby_sin$ancpolice, sins_inst_sauvegarde$ancpolice)]

#Sauvegarde
write.table(effet_echean_grpby,file = paste0(Chemin_resultat,"prod_inst_aggByPol.csv"),sep=";",dec=",", row.names = FALSE)
write.table(effet_echean_grpby_sin,file = paste0(Chemin_resultat,"sins_inst_aggByPol.csv"),sep=";",dec=",", row.names = FALSE)

# -***************** MERGING DES DEUX BASES  *************

ProdSin_07_10_2020 = merge(effet_echean_grpby,effet_echean_grpby_sin, 
                           by.x ="ancienpol",by.y= "ancpolice",all.x = T)

#colnames(ProdSin_07_10_2020)
#Enregistrement
write.table(ProdSin_07_10_2020, file = paste0(Chemin_resultat,"ProdSin_07_10_2020.csv"),sep=";",dec=",", row.names = FALSE)

##############�  Flagguer les polices
BaseTravail = ProdSin_07_10_2020
#BaseTravail$codepartenaire = BaseTravail$codepartenaire.x
BaseTravail$blokRisk=""
BaseTravail$blokRisk[(BaseTravail$codepartenaire == "Particulier" & BaseTravail$branche == "auto" & BaseTravail$produit!="Automobile Flotte")|(BaseTravail$codepartenaire == "Particulier" & 
                                                                                                                                                BaseTravail$branche == "Santé" & BaseTravail$primeMoy <2000000)|
                       ( BaseTravail$codepartenaire == "Particulier" & BaseTravail$branche == "Dommages corporels"& BaseTravail$produit!="COLLECTIVES DIVERSES" & BaseTravail$produit!="COLLECTIVES SCOLAIRES" &
                           BaseTravail$produit!="INDIVIDUELLE COLLECTIVE EMPLOYES")|(BaseTravail$codepartenaire == "Particulier" &
                                                                                       BaseTravail$branche == "Inc et DAB" & BaseTravail$primeMoy <= 500000)|(BaseTravail$codepartenaire == "Particulier"&
                                                                                                                                                              BaseTravail$branche == "Responsabilité civile" & BaseTravail$primeMoy <= 250000)]="Risque individuel"
#BaseTravail$blokRisk[(BaseTravail$codepartenaire == "Particulier" & BaseTravail$branche == "auto")]="Risque individuel"

BaseTravail$blokRisk[(BaseTravail$branche == "auto" & BaseTravail$codepartenaire!="Particulier")|(BaseTravail$codepartenaire!="Particulier" & 
                                                                                                    BaseTravail$branche == "Santé")|(BaseTravail$codepartenaire == "Particulier" &  BaseTravail$branche == "Santé" & BaseTravail$primeMoy >= 2000000)|(BaseTravail$codepartenaire == "Particulier" &
                                                                                                                                                                                                                                                       BaseTravail$branche == "Inc et DAB" & BaseTravail$primeMoy>500000)|
                       (BaseTravail$codepartenaire == "Particulier"& BaseTravail$branche == "Responsabilité civile" & BaseTravail$primeMoy >250000)|
                       (BaseTravail$branche == "Dommages corporels"|(BaseTravail$produit == "COLLECTIVES DIVERSES" | BaseTravail$produit == "COLLECTIVES SCOLAIRES"|
                                                                       BaseTravail$produit == "INDIVIDUELLE COLLECTIVE EMPLOYES"))]="Risque collectif"
BaseTravail$blokRisk[BaseTravail$blokRisk == ""]="Gros risques"


#calcul des indicateurs de decision
BaseTravail$Autres_charges = dplyr::coalesce(0.16*BaseTravail$prime, 0)+dplyr::coalesce(BaseTravail$commission,0)
BaseTravail$ratiocombine = (BaseTravail$charges_sinistre+BaseTravail$Autres_charges)/BaseTravail$prime
BaseTravail$nbre_sin_moy =  dplyr::coalesce(BaseTravail$nbsin)/BaseTravail$AnneePolice
BaseTravail$ratiocomb_hors_sin_grav = (BaseTravail$charges_sinistre - BaseTravail$charge_grave +  BaseTravail$Autres_charges)/BaseTravail$prime

#*********************** IMPLEMENTATION DES DECISIONS *****************************
#Seuils
seuil_ratio_nbre    = 1.5
seuil_ratio_combine = 0.9

BaseTravail$qualif = 0
BaseTravail$qualif[(BaseTravail$blokRisk == "Risque individuel" 
                                   & BaseTravail$Ratio_combine >seuil_ratio_combine & BaseTravail$nbre_sin_moy > seuil_ratio_nbre)]  = 1

BaseTravail$qualif[(BaseTravail$blokRisk == "Risque collectif"
                    & BaseTravail$Ratio_combine > seuil_ratio_combine)]  = 1


BaseTravail$qualif[(BaseTravail$blokRisk == "Gros risques"
                    & BaseTravail$ratiocomb_hors_sin_grav > seuil_ratio_combine & BaseTravail$nbre_sin_moy > seuil_ratio_nbre)]  = 1



write.table(BaseTravail,file = paste0(Chemin_resultat, "Output_qualificatif.csv"),sep=";",dec=",", row.names = FALSE)
