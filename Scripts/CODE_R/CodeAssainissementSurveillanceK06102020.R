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
  assign( Nom,read.csv2(paste0("Data Production/",Liste_Fichiers[i]),  stringsAsFactors = FALSE ))
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
BaseFusionPrime <- read.csv2(paste0(Chemin_resultat,"BaseFusionPrime.csv"),  stringsAsFactors = FALSE )

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
BaseFusionPrime$branche= as.factor(BaseFusionPrime$branche)
levels(BaseFusionPrime$branche) = c("Assistance","Inc et DAB","Dommages aux biens","Dommages corporels","Inc et DAB","Santé","Responsabilité civile","Responsabilité civile","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","auto","auto","auto")
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
BasePropreProd$anneeffet=as.Date(BasePropreProd$date_effet,format = "%Y")
BasePropreProd$anneeffet = lubridate::year(BasePropreProd$date_effet)
table(BasePropreProd$branche)
BasePropreProd$branche=as.character(BasePropreProd$branche)
vecagg1=c("anneeffet","branche")
#vecagg2=c("pnette","commission","cp")
vecagg2=c("pnette")
BasePropreProd_ag= aggregate(BasePropreProd[,vecagg2],by=BasePropreProd[,vecagg1], FUN = sum, na.rm=TRUE)
colnames(BasePropreProd_ag)
colnames(BasePropreProd_ag)[3]="pnette"
###### ---------------------- Indiquer les identifiants anormales -------------------- ###
write.table(BasePropreProd_ag,file = paste0(Chemin_resultat,"basepropreprod_agg_par_anneeeffet.csv"),sep=";",dec=",", row.names = FALSE)
############# Ajout de la base Insto a la base production
tb_Insto <- read.csv2("baseinsto_to_joinBassepropre2.csv")
tb_Insto$Date.de.survenance=as.Date(tb_Insto$Date.de.survenance,format = "%d/%m/%Y")
tb_Insto$Date.effet=as.Date(tb_Insto$Date.effet,format = "%d/%m/%Y")
tb_Insto$Date.échéance=as.Date(tb_Insto$Date.échéance,format = "%d/%m/%Y")
tb_Insto$new_date_effet=as.Date(tb_Insto$new_date_effet,format = "%d/%m/%Y")
tb_Insto$new_date_eheance=as.Date(tb_Insto$new_date_eheance,format = "%d/%m/%Y")
tb_Insto$Duree=DureePolice(tb_Insto$new_date_effet,tb_Insto$new_date_eheance)
tb_Insto$AnneePolice=Exposition(tb_Insto$new_date_effet,tb_Insto$new_date_eheance)
tb_Insto$Branche= as.factor(tb_Insto$Branche)
tb_Insto$numero_sinistre= str_trim(tb_Insto$numero_sinistre)
tb_Insto$numero_sinistre=str_to_upper(tb_Insto$numero_sinistre)
tb_Insto$policeclone=tb_Insto$numero_sinistre
levels(tb_Insto$Branche) = c("auto","Assistance","Dommages aux biens","Dommages corporels","Inc et DAB","Responsabilité civile","auto")
baseprod_propre <- BasePropreProd %>% dplyr::rename(dateeffet=date_effet,datefineffet=fin_garantie)
insto <- tb_Insto %>% dplyr::rename(branche=Branche, client=Assuré,pnette=`new.PN`,dateeffet=`new_date_effet`,datefineffet = `new_date_eheance`, ancpolice=Police, 
                                    provision_fin = `Provision.insto.révisé`)%>% 
  dplyr::mutate( commission="", nbprime=1, parcoas1="", mois_effet="", taxe="",primeTTC="", codeintermediaire="",produit="",coass="",
                 codepartenaire="", sinister_id="", cp="", idreas="")
insto$Identifiant=paste0(insto$numero_sinistre,as.numeric(insto$dateeffet),as.numeric(insto$datefineffet))

list_col1 <- intersect(names(insto),names(baseprod_propre))
base_prod_insto = rbind(baseprod_propre[,list_col1],insto[,list_col1])
write.table(base_prod_insto,file = paste0(Chemin_resultat,"base_prod_insto.csv"),sep=";",dec=",", row.names = FALSE)
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
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


basesinistre$policeclone <- gsub("\\s+","", gsub("-", "K", basesinistre$policeclone))
basesinistre$policeclone <- str_sub(basesinistre$policeclone,1,15)
basesinistre$nbsin=1
basesinistre$Identifiant <- paste0(basesinistre$policeclone,as.numeric(basesinistre$dateeffet),as.numeric(basesinistre$datefineffet))



vecagg3=c("an_surv","branche")
#vecagg2=c("pnette","commission","cp")
vecagg4=c("cumul_paye","provision_fin")
basesinistre_ag = aggregate(basesinistre[,vecagg4],by=basesinistre[,vecagg3], FUN = sum, na.rm=TRUE)
write.table(basesinistre_ag,file = paste0(Chemin_resultat,"basesinistre_ag.csv"),sep=";",dec=",", row.names = FALSE)
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

# Aggregation svt branche et ansurv avt ajout sante
vecagg3=c("an_surv","branche")
vecagg4=c("cumul_paye","provision_fin", "nbsin")
basesinistre_apres_boni_mali = tmp1
basesinistre_apres_boni_mali[is.na(basesinistre_apres_boni_mali$nbsin),"nbsin"] = 1

basesinistre_apres_boni_mali_agg = aggregate(basesinistre_apres_boni_mali[,vecagg4],by=basesinistre_apres_boni_mali[,vecagg3], FUN = sum, na.rm=TRUE)
write.table(basesinistre_apres_boni_mali_agg,file = paste0(Chemin_resultat,"basesinistre_apres_boni_mali_agg.csv"),sep=";",dec=",", row.names = FALSE)

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
sante$branche = "Maladie"

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

#On concatenance
instoFinale = cbind(insto_nv, tmpDf1)
SumDouble(instoFinale$provision_fin, na.rm = T) + SumDouble(tmp_ajuste$provision_fin,na.rm = T)
# Verification : 20260960451

# Fusion de instoFinale et tmp_ajuster
fusion_insto_tmpajust = rbind(tmp_ajuste, instoFinale)
write.table(fusion_insto_tmpajust,file = paste0(Chemin_resultat,"fusion_insto_tmpajust_avt_agg.csv"),sep=";",dec=",", row.names = FALSE)

#Verification
SumDouble(fusion_insto_tmpajust$provision_fin, na.rm = T) %>% prettyNum(" ")
# "20 260 960 451"

# Aggregation svt branche et anssurv
vecagg3=c("an_surv","branche")
vecagg4=c("cumul_paye","provision_fin", "nbsin")
fusion_insto_tmpajust[is.na(fusion_insto_tmpajust$nbsin),"nbsin"] = 1

basesinistre_inst_agg = aggregate(fusion_insto_tmpajust[,vecagg4],by=fusion_insto_tmpajust[,vecagg3], FUN = sum, na.rm=TRUE)
write.table(basesinistre_inst_agg,file = paste0(Chemin_resultat,"fusion_insto_sin_agg.csv"),sep=";",dec=",", row.names = FALSE)










tmp1$CS <- rowSums(tmp1[,c("porvision_boni_mali", "cumul_paye")], na.rm=TRUE)
tmp1$CSgrave <- fifelse(tmp1$CS >= 10000000,"grave","Pas_grave")
write.table(tmp1,file = paste0(Chemin_resultat, "base_sinistre_into_agg.csv"),sep=";",dec=",", row.names = FALSE)
tmp1$nbgrave <- fifelse(tmp1$CSgrave=="grave",1,0)

vec1 = c("provision_fin","boni_mali","porvision_boni_mali","cumul_paye","CS","CSgrave","reglement_exercice","cumul_recours","eval_recours","mt_eval","nbsin","nbgrave")
#vec2 = c("numero_sinistre","dateeffet","datefineffet","an_surv")

tmp2 <- aggregate(tmp1[,vec1],by=tmp1[,vec2], FUN = sum, na.rm = TRUE)

write.table(basesinistre_ag,file = paste0(Chemin_resultat,"basesinistre_ag.csv"),sep=";",dec=",", row.names = FALSE)

# Remplacement santé






# Vec_Variable = c("provision_fin","prevision_fin","cumul_paye","mt_eval","eval_recours","cumul_recours","reglement_exercice","recours_exercice","nbsin")
# by_variable = c("Identifiant","policeclone","dateeffet","datefineffet")
# basesinistre1 <-aggregate(basesinistre[,Vec_Variable],by=basesinistre[,by_variable], FUN = sum, na.rm=TRUE)
# BasePropresin1 <- basesinistre1
# BasePropresin1$dateeffet <- basesinistre$dateeffet[match(BasePropresin1$Identifiant,basesinistre$Identifiant)]
# BasePropresin1$numero_sinistre<- basesinistre$numero_sinistre[match(BasePropresin1$Identifiant,basesinistre$Identifiant)]


base2 <- read.csv2("SITUATION INSTO AU 31 07 2020_avec explicationV.csv")
base2vrai <- base2[base2$Statut=="Non Régularisé",]
insto_nv$provision_fin <- base2vrai$Provision.insto.revise
insto_nv$nbsin <- 1
insto_nv <- insto_nv %>% dplyr::rename(survenance="Date.de.survenance") %>% dplyr::mutate(cumul_paye="", mt_eval="", recours_exercice="", reglement_exercice="", cumul_recours="", eval_recours="", prevision_fin="")

insto_nv$provision_fin <- as.numeric(gsub(",", ".", gsub("\\.", "", insto_nv$provision_fin)))
insto_nv$cumul_paye <- as.numeric(gsub(",", ".", gsub("\\.", "", insto_nv$cumul_paye)))
insto_nv$mt_eval <- as.numeric(gsub(",", ".", gsub("\\.", "", insto_nv$mt_eval)))
insto_nv$recours_exercice <- as.numeric(gsub(",", ".", gsub("\\.", "", insto_nv$recours_exercice)))
insto_nv$cumul_recours<- as.numeric(gsub(",", ".", gsub("\\.", "", insto_nv$cumul_recours)))
insto_nv$eval_recours <- as.numeric(gsub(",", ".", gsub("\\.", "", insto_nv$eval_recours)))

insto_nv$dateeffet=as.Date(insto_nv$dateeffet,format = "%d/%m/%Y")
insto_nv$datefineffet=as.Date(insto_nv$datefineffet,format = "%d/%m/%Y")

list_col2 <- intersect(names(BasePropresin1),names(insto_nv))
basesinistre_Insto = rbind(BasePropresin1[,list_col2],insto_nv[,list_col2])
##############

write.table(basesinistre_Insto,file = paste0(Chemin_resultat,"basesinistre_Insto.csv"),sep=";",dec=",", row.names = FALSE)
#com <- intersect(basesinistre_Insto$Identifiant,base_prod_insto$Identifiant)
##-----------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------
###-----------------------Merge BasePropreProd et BasePropresin----------------------------


#TT3<-basesinistre_Insto_agg_sansdoubl[!duplicated(basesinistre_Insto_agg_sansdoubl$Identifiant),]
tmp3= merge(base_prod_insto,basesinistre_Insto, by.x ="Identifiant",by.y= "Identifiant",all.x = T)
write.table(tmp3,file = paste0(Chemin_resultat, "basetrav.csv"),sep=";",dec=",", row.names = FALSE)












#### Ajout des ref
prsingrp <- tmp3
Ref1 <- read.xlsx("Activités_Portefeuille_Mars2020_Ref1.xlsx",sheet = "ActiviteSSdoublon",detectDates = T)
#prsingrp$ref1=Ref1$Activités[match(prsingrp$client.y,Ref1$client)]
Ref2 <- read.xlsx("Activités_Clients_Juillet2019_V1_Ref2.xlsx",sheet = "liste_Polices_activite",detectDates = T)
Ref2prim <- read.xlsx("Activités_Clients_Juillet2019_V1_Ref2.xlsx",sheet = "Reduction de capacité",detectDates = T)
Ref2$Activité.detail <- Ref2prim$activite[match(Ref2$code_activite,Ref2prim$code.activite)]

prsingrp$ref1=Ref1$Activités[match(prsingrp$client,Ref1$client)]
prsingrp$ref2=Ref2$Activité.detail[match(prsingrp$client,Ref2$client)]
prsingrp$ref3 <- fifelse(!is.na(prsingrp$ref1),prsingrp$ref1,prsingrp$ref2)
#prsingrp$branch_auto <- prsingrp$branche.x[prsingrp$branche.x=="auto"]
prsingrp$branch_auto <- ""
prsingrp$branch_auto <- fifelse(prsingrp$branche=="auto",as.character(prsingrp$branche),prsingrp$branch_auto)
prsingrp$ref3 <- fifelse(!is.na(prsingrp$ref3),prsingrp$ref3,prsingrp$branch_auto)
prsingrp$ref3[prsingrp$ref3=="auto"] <- "Particulier"
prsingrp$branch_auto2 <- ""
prsingrp$branch_auto2 <- fifelse(prsingrp$branche=="Véhicule terrestre à  moteur",as.character(prsingrp$branche),prsingrp$branch_auto2)
prsingrp$ref4 <- fifelse(!is.na(prsingrp$ref3),prsingrp$ref3,prsingrp$branch_auto2)
prsingrp$ref4[prsingrp$ref4=="Véhicule terrestre à  moteur"] <- "Particulier"
trav_ben_komla <- read.xlsx("Base_unknowed_activities_komla.xlsx",sheet = "dictio",detectDates = T)
prsingrp$ref5=trav_ben_komla$Activité[match(prsingrp$client,trav_ben_komla$Client)]
prsingrp$Activite <- fifelse(!is.na(prsingrp$ref4),prsingrp$ref4,prsingrp$ref5)
prsingrp <- prsingrp %>% dplyr::select(c("Identifiant","numeropolicecommun","numero_sinistre","branche","client","codeintermediaire","Activite","pnette","commission","nbsin",
                                         "dateeffet","datefineffet","nbprime","Duree","cumul_paye","provision_fin","recours_exercice"))



write.table(prsingrp,file = paste0(Chemin_resultat, "ProdSin_Xsansdoublon2013_juillet2020_Activie.csv"),sep=";",dec=",", row.names = FALSE)

Diff1 <- read.xlsx("Differentiel Provision_Version 1_Coupe off_21092020.xlsx",sheet = "version 1",detectDates = T)
Diff1 <- Diff1 %>% dplyr::rename("numeropolicecommun"="ancpolice")

tmp4 <- merge(prsingrp,Diff1, by = c("intermediaire","client","dateeffet","datefineffet"), all.x=TRUE, all.y=F)
tmp4$provision_fin = as.numeric(tmp4$provision_fin)
tmp4$porvision_total <- rowSums(tmp4[,c("provision_fin", "Differentiel.1.officiel")], na.rm=TRUE)

write.table(tmp4,file = paste0(Chemin_resultat, "ProdSin_Xsansdoublon2013_juillet2020_Activiev1.csv"),sep=";",dec=",", row.names = FALSE)



Diff2 <- read.xlsx("Differentiel Provision_Version 2_Ttes coupes_21092020.xlsx",sheet = "Feuil1",detectDates = T)
Diff2 <- Diff2 %>% dplyr::rename("numeropolicecommun"="ancpolice")


tmp5 <- merge(prsingrp,Diff2, by = c("numeropolicecommun","dateeffet","datefineffet"), all.x=TRUE, all.y=TRUE)
tmp5$provision_fin = as.numeric(tmp5$provision_fin)
tmp5$porvision_total <- rowSums(tmp5[,c("provision_fin", "Differentiel.2.toutes.coupes")], na.rm=TRUE)
write.table(tmp5,file = paste0(Chemin_resultat, "ProdSin_Xsansdoublon2013_juillet2020_Activiev2.csv"),sep=";",dec=",", row.names = FALSE)

############
#kk <- unique(prsingrp[!is.na(prsingrp$Activite),]$client.y)
#write.table(kk,file = paste0(Chemin_resultat, "Activite_unknown.csv"),sep=";",dec=",", row.names = FALSE)
#ooo <- prsingrp[!is.na(prsingrp$Activite),]$client
#############
sante <- read.xlsx("1ProdSin_Xsansdoublon1v1_rev_santé.xlsx",sheet = "1ProdSin_Xsansdoublon1v1",detectDates = T)
sante$sinistre <- gsub('\\s+', '', sante$sinistre)
sante$sinistre <- as.numeric(sante$sinistre)

tmp4$frais_gestion=0.16*tmp4$pnette.x
tmp4$commission=as.numeric(tmp4$commission)
tmp4$Autres_charges <- rowSums(tmp4[,c("frais_gestion", "commission")], na.rm=TRUE)
#tmp4$Autres_charges=tmp4$frais_gestion+tmp4$commission
#class(tmp4$cumul_paye)
tmp4$recours_exercice <- as.numeric(tmp4$recours_exercice)
tmp4$charge_totale <- rowSums(tmp4[,c("cumul_paye", "porvision_total")], na.rm=TRUE)
tmp4$sinistre_sante <- sante$sinistre[match(tmp4$Identifiant.x,sante$Identifiant)]
tmp4$charge_totale <- fifelse(!is.na(tmp4$sinistre_sante),tmp4$sinistre_sante,tmp4$charge_totale)
#tmp4$charge_totale=tmp4$cumul_paye+tmp4$porvision_total
tmp4$charge_totalenetderecours=tmp4$cumul_paye +tmp4$porvision_total - tmp4$recours_exercice

tmp4$part_cummulé_payé_dans_la_Charge=tmp4$cumul_paye/tmp4$charge_totale
tmp4$S_P=((tmp4$charge_totale))/(tmp4$pnette.x)
tmp4$ratiocombine=(tmp4$charge_totale+tmp4$Autres_charges)/tmp4$pnette.x
#tmp4$nban_parpol <- 
#tmp4$nbsin_parnban_parpol <- 


write.table(tmp4,file = paste0(Chemin_resultat, "basetrav.csv"),sep=";",dec=",", row.names = FALSE)













