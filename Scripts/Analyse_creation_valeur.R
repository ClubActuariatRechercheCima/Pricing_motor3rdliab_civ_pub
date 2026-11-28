library(ggplot2)
library(lattice)
library(plyr)

library(tm)
library(wordcloud)
library(RColorBrewer)
library(waterfall)

#https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/



Chemin_base="<CHEMIN_LOCAL>/"

Chemin_Input=paste(Chemin_base, "Prod 0618/",sep="")

Chemin_Anomalie= paste(Chemin_base, "Anomalie",sep="") 
Chemin_application_resultat= paste(Chemin_base, "Resultat",sep="") 


#### Reconciliation Des Montants de Primes ###


setwd(paste0(Chemin_Input,"Prime"))

Liste_Fichiers=list.files(paste0(Chemin_Input,"Prime"),pattern = "*.csv")


for (i in 1:length(Liste_Fichiers))
{
  
  Nom = paste("Table",i,sep="")
  assign( Nom,read.csv2(Liste_Fichiers[i],  stringsAsFactors = FALSE ))
  
}

Table7$impaye=0
Table7$ptotale=0
#table(PrimeAnalyse$impaye)

PrimeAnalyse=get(paste("Table",1,sep=""))

#TableEntiere = merge(get(paste("Table",6)),get(paste("Table",7)),all=TRUE)
for (i in 2:length(Liste_Fichiers) )
{
  PrimeAnalyse  = merge(PrimeAnalyse, get(paste("Table",i,sep="")),all=TRUE)
  
  
}

# Table8$AnneeEffet = ExtractionAnnee(Table8$date_effet)

 
PrimeAnalyse$branche=as.factor(PrimeAnalyse$branche)
levels(PrimeAnalyse$branche)

levels(PrimeAnalyse$branche)<-c("Assistance","D.A.B","Dommages corporels","Incendie", "Sante", "Resp. civile", "Resp. civile", "Resp. civile", "Risques Agricoles","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport","Transport", "Auto", "Auto", "Auto")  
#levels(PrimeAnalyse$branche)<-c("Assistance","Dommages aux biens","Dommages corporels","Incendie", "Maladie", "Resp. civile", "Risques Agricoles","Transport","Transport","Transport","Transport", "Auto")  

levels(PrimeAnalyse$branche)


table(PrimeAnalyse$branche, PrimeAnalyse$exercice)

PrimeAnalyse$typeinter = as.factor(PrimeAnalyse$typeinter)
levels(PrimeAnalyse$typeinter )
levels(PrimeAnalyse$typeinter ) <-c("Bureau Direct", "Courtier","Agence" ,"Agence","Agence" ,"Bureau Direct", "Courtier")

View(PrimeAnalyse[PrimeAnalyse$typeinter==3,])


table(PrimeAnalyse$codepartenaire)
PrimeAnalyse$codepartenaire = as.factor(PrimeAnalyse$codepartenaire)
levels(PrimeAnalyse$codepartenaire)
levels(PrimeAnalyse$codepartenaire) <-c("Particulier","Entreprise","Particulier","Entreprise")
levels(PrimeAnalyse$codepartenaire)


PrimeAnalyse$coass= as.numeric(PrimeAnalyse$coass)
PrimeAnalyse$coass[is.na(PrimeAnalyse$coass)]=0
PrimeAnalyse$coass= as.numeric(PrimeAnalyse$coass)

PrimeAnalyse$coass[is.na(PrimeAnalyse$coass)]=0
PrimeAnalyse$pnette[is.na(PrimeAnalyse$pnette)]=0
PrimeAnalyse$cp[is.na(PrimeAnalyse$cp)]=0

PrimeAnalyse$pnette_coass = PrimeAnalyse$pnette + PrimeAnalyse$coass + PrimeAnalyse$cp - PrimeAnalyse$impaye
PrimeAnalyse$pnette_coass_com =PrimeAnalyse$pnette_coass  - PrimeAnalyse$commission

PrimeAnalyse$Annee_Emission= ExtractionAnnee(PrimeAnalyse$periode) 
PrimeAnalyse$Duree= DureePolice(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie)
PrimeAnalyse$AnneePolice=Exposition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie)
PrimeAnalyse$Valide=  (PrimeAnalyse$AnneePolice<3)&(PrimeAnalyse$AnneePolice>0)
PrimeAnalyse$AnneeEffet = ExtractionAnnee(PrimeAnalyse$date_effet)
PrimeAnalyse$NbrePolice=1

#T1=PrimeAnalyse$AnneeEffet<2017
#T2=

PrimeAnalyse_retrait=PrimeAnalyse[!((PrimeAnalyse$AnneeEffet<2017) & PrimeAnalyse$pnette>100000000 & PrimeAnalyse$Annee_Emission==2017),]
Retrait_emission_tardives= PrimeAnalyse[((PrimeAnalyse$AnneeEffet<2017) & PrimeAnalyse$pnette>100000000 & PrimeAnalyse$Annee_Emission==2017),]
write.table(Retrait_emission_tardives,paste0(Chemin_Anomalie,"/Retrait_emission_tardives.csv"),sep=";",dec=",", row.names = FALSE)  



Vec_Variable = c("pnette","coass","pnette_coass","pnette_coass_com","NbrePolice", "commission","taxe", "cp", "AnneePolice", "impaye")

#Vec_Variable = c("pnette", "pnette2","taxe","ptotale","commission","impaye","cp","coass","Duree","NbrePolice")
  by_variable = c("Annee_Emission","branche" , "typeinter", "codepartenaire")
PrimeEmisssion_Aggreg <-aggregate(PrimeAnalyse[,Vec_Variable],by=PrimeAnalyse[,by_variable], FUN = SumDouble, na.rm=TRUE)
write.table(PrimeEmisssion_Aggreg,paste(Chemin_application_resultat,"/PrimeEmise_agg_20102017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

write.table(PrimeAnalyse_Auto,paste(Chemin_application_resultat,"/PrimeEmise_auto.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

  ###################### Analyse graphique de l'évolution du CA#######
#### la plus forte croissance vient d'ou ####
  ###                                   ###
  ###                                   ### 
###### #### #### ### #### ### ### ### ### ###
 
 #Description de la croissance par Canal et pour les branches princiaples
  ### 
  ###
setwd("<CHEMIN_LOCAL>/")
Principales_Branches= c("Sante et Corp.","Auto","Transport","Incendie & DAB", "Resp. civile")


for (i in 1:5)
{
  # Principales_Branches
  sBranche = Principales_Branches[i]
  bd = PrimeEmisssion_Aggreg[PrimeEmisssion_Aggreg$branche==sBranche & PrimeEmisssion_Aggreg$Annee_Emission<2018, ]
  g <- ggplot(data=bd, aes(Annee_Emission,pnette_coass, fill=typeinter))
  g<-g+geom_bar(stat = "identity",position="stack")
 # g<-g+geom_point(aes(group=Entite, colour=Entite,  size=2))
  g<-g+ labs(fill = "Canal de distribution")
  g<-g+labs(x="Année", y="Chiffre d'affaires")
  g<-g+ggtitle(label=paste0("Evolution du C.A. ",sBranche, "  de la compagnie"))
  g<-g+scale_y_continuous(label=function(x){return(paste0(x/1e9," Mds"))})
 # g<-g+scale_color_manual(values=c("orange","blue", "purple", "green","red"))
  g<- g+  theme(panel.grid.major=element_line(colour = "grey"))
  g<-g + theme(axis.line = element_line(size = 3, colour = "grey80"))
  
  g
  ggsave(paste0("Evolution CA de la compagnie ",sBranche,".png"), device="png", width=12,height = 7, units = "cm" )
  
  
}



for (i in 1:5)
{
  # Principales_Branches
  sBranche = Principales_Branches[i]
  bd = PrimeEmisssion_Aggreg[PrimeEmisssion_Aggreg$branche==sBranche & PrimeEmisssion_Aggreg$Annee_Emission<2017, ]
  g <- ggplot(data=bd, aes(Annee_Emission,pnette_coass, fill= codepartenaire))
  g<-g+geom_bar(stat = "identity",position="stack")
  # g<-g+geom_point(aes(group=Entite, colour=Entite,  size=2))
  g<-g+ labs(fill = "Segment")
  g<-g+labs(x="Année", y="Chiffre d'affaires")
  g<-g+ggtitle(label=paste0("Evolution du C.A. ",sBranche, "  de la compagnie"))
  g<-g+scale_y_continuous(label=function(x){return(paste0(x/1e9," Mds"))})
  # g<-g+scale_color_manual(values=c("orange","blue", "purple", "green","red"))
  g<- g+  theme(panel.grid.major=element_line(colour = "grey"))
  g<-g + theme(axis.line = element_line(size = 3, colour = "grey80"))
  
  g
  ggsave(paste0("Evolution CA par Segment ",sBranche,".png"), device="png", width=12,height = 7, units = "cm" )
  
  
}


########### Prime Acquise et PREC ################
#as.numeric(as.Date(PrimeAnalyse$fin_garantie[1], "%d/%m/%Y")-as.Date(PrimeAnalyse$date_effet[1], "%d/%m/%Y"))/365.25

#### Acquisition ####  
by_variable = c("Annee_Emission","branche" , "typeinter", "codepartenaire")

PrimeAnalyse$AcquisitionPrec=0
iAnnee=2010
PrimeAnalyse$Acquisition = AnneePoliceAcquisition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie,paste0("01/01/",iAnnee),paste0("31/12/",iAnnee))
PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$AcquisitionPrec = AnneePoliceAcquisition(PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$date_effet,PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$fin_garantie,paste0("01/01/",iAnnee+1),"31/12/2020")

#sum(PrimeAnalyse$Acquisition*PrimeAnalyse$pnette>0, na.rm = TRUE)
#tapply(PrimeAnalyse$Acquisition*PrimeAnalyse$pnette, PrimeAnalyse$Annee_Emission, FUN=function(x){sum(x>0,na.rm = TRUE)})
#tapply(PrimeAnalyse$Acquisition*PrimeAnalyse$pnette, PrimeAnalyse$Annee_Emission, sum, na.rm  =TRUE)


#write.table(PrimeAnalyse[PrimeAnalyse$Acquisition>0,],paste(Chemin_Anomalie,"/Acquisition2010.csv",sep=""), sep=";", dec=",")


PrimeAnalyse$util = (PrimeAnalyse$Valide)&(PrimeAnalyse$Acquisition>0)

PrimeAnalyse$PrimeAcquise = PrimeAnalyse$pnette_coass *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
PrimeAnalyse$Prec = (PrimeAnalyse$pnette_coass *PrimeAnalyse$AcquisitionPrec)/PrimeAnalyse$AnneePolice

PrimeAnalyse$PrimeAcquiseNetCommission=   PrimeAnalyse$pnette_coass_com *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
#by(PrimeAnalyse$PrimeAcquise, PrimeAnalyse$exercice, FUN = SumDouble)
#by(PrimeAnalyse$commission, PrimeAnalyse$exercice, FUN = SumDouble)
#by(PrimeAnalyse$PrimeAcquise, PrimeAnalyse$AnneeEffet, FUN = SumDouble)
#by(PrimeAnalyse$pnette_coass, PrimeAnalyse$exercice, FUN = SumDouble)
#SumDouble(PrimeAnalyse$PrimeAcquise[PrimeAnalyse$util==TRUE])

 #= PrimeAnalyse_Val$pnette*PrimeAnalyse_Val$Acquisition/PrimeAnalyse_Val$Duree
#write.table(PrimeAnalyse[!PrimeAnalyse$Valide &PrimeAnalyse$Annee_Emission==2017 ,],paste(Chemin_Anomalie,"/CheckPeriode.csv",sep=""),sep=";",dec=",", row.names = FALSE)  




Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Prec","Acquisition")
#Vec_Variable = c("pnette","coass","pnette_coass","pnette_coass_com","NbrePolice", "commission","taxe", "cp", "AnneePolice", "impaye")

#PrimeProv <-aggregate(PrimeAnalyse[,Vec_Variable],by=list(Effet=PrimeAnalyse$AnneeEffet,Exercice=PrimeAnalyse$exercice, Branche=PrimeAnalyse$branche ), FUN = SumDouble, na.rm=TRUE)
PrimeProv <-aggregate(PrimeAnalyse[,Vec_Variable],by=PrimeAnalyse[,by_variable], FUN = SumDouble, na.rm=TRUE)

PrimeProv$AnneeAcquisition=iAnnee

#write.table(PrimeAnalyse[PrimeAnalyse$AnneeEffet==2016,],paste(Chemin_application_resultat,"/CAS2016.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

PrimeAcqui_Prec_Aggreg = PrimeProv

for(iAnnee in 2011:2017)
{
  PrimeAnalyse$AcquisitionPrec=0
  PrimeAnalyse$Acquisition = AnneePoliceAcquisition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie,paste0("01/01/",iAnnee),paste0("31/12/",iAnnee))
  #PrimeAnalyse$AcquisitionPrec = AnneePoliceAcquisition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie,paste0("01/01/",iAnnee+1),"31/12/2020")
  PrimeAnalyse$AcquisitionPrec[PrimeAnalyse$Annee_Emission==iAnnee] = AnneePoliceAcquisition(PrimeAnalyse$date_effet[PrimeAnalyse$Annee_Emission==iAnnee],PrimeAnalyse$fin_garantie[PrimeAnalyse$Annee_Emission==iAnnee],paste0("01/01/",iAnnee+1),"31/12/2030")
  
  PrimeAnalyse$util = (PrimeAnalyse$Valide)&(PrimeAnalyse$Acquisition>0)
  
  PrimeAnalyse$PrimeAcquise = PrimeAnalyse$pnette_coass *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
  PrimeAnalyse$Prec = (PrimeAnalyse$pnette_coass *PrimeAnalyse$AcquisitionPrec)/PrimeAnalyse$AnneePolice
  
  PrimeAnalyse$PrimeAcquiseNetCommission=   PrimeAnalyse$pnette_coass_com *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
 # Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Prec","AnneePolice","NbrePolice","Acquisition")
 
  
  PrimeProv <-aggregate(PrimeAnalyse[,Vec_Variable],by=PrimeAnalyse[,by_variable], FUN = SumDouble, na.rm=TRUE)
  
  PrimeProv$AnneeAcquisition=iAnnee
  
  PrimeAcqui_Prec_Aggreg = rbind(PrimeAcqui_Prec_Aggreg,PrimeProv)
  
  
}
write.table(PrimeAcqui_Prec_Aggreg,paste(Chemin_application_resultat,"/PrimeAcquise_agg_20102017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  
write.table(PrimeAnalyse,paste(Chemin_application_resultat,"/PrimeAnalyse.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

#iAnnee= 2019
### compléments


#### Acquisition sans PREC ####  
by_variable = c("Annee_Emission","branche" , "typeinter", "codepartenaire")

PrimeAnalyse$AcquisitionPrec=0
iAnnee=2007

PrimeAnalyse$Acquisition = AnneePoliceAcquisition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie,paste0("01/01/",iAnnee),paste0("31/12/",iAnnee))
#PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$AcquisitionPrec = AnneePoliceAcquisition(PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$date_effet,PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$fin_garantie,paste0("01/01/",iAnnee+1),"31/12/2020")
PrimeAnalyse$util = (PrimeAnalyse$Valide)&(PrimeAnalyse$Acquisition>0)
PrimeAnalyse$NbrePolice=1*PrimeAnalyse$util

PrimeAnalyse$PrimeAcquise = PrimeAnalyse$pnette_coass *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
#PrimeAnalyse$Prec = (PrimeAnalyse$pnette_coass *PrimeAnalyse$AcquisitionPrec)/PrimeAnalyse$AnneePolice
PrimeAnalyse$PrimeAcquiseNetCommission=   PrimeAnalyse$pnette_coass_com *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
#Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Acquisition")
Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","NbrePolice","Acquisition")


PrimeProv <-aggregate(PrimeAnalyse[,Vec_Variable],by=PrimeAnalyse[,by_variable], FUN = SumDouble, na.rm=TRUE)
PrimeProv$AnneeAcquisition=iAnnee

PrimeAcqui_Aggreg = PrimeProv

for(iAnnee in 2008:2025)
{
  PrimeAnalyse$AcquisitionPrec=0
  PrimeAnalyse$Acquisition = AnneePoliceAcquisition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie,paste0("01/01/",iAnnee),paste0("31/12/",iAnnee))
  PrimeAnalyse$util = (PrimeAnalyse$Valide)&(PrimeAnalyse$Acquisition>0)
  PrimeAnalyse$NbrePolice=1*PrimeAnalyse$util
  PrimeAnalyse$PrimeAcquise = PrimeAnalyse$pnette_coass *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
  PrimeAnalyse$PrimeAcquiseNetCommission=   PrimeAnalyse$pnette_coass_com *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
  PrimeProv <-aggregate(PrimeAnalyse[,Vec_Variable],by=PrimeAnalyse[,by_variable], FUN = SumDouble, na.rm=TRUE)
  
  PrimeProv$AnneeAcquisition=iAnnee
  
  PrimeAcqui_Aggreg = rbind(PrimeAcqui_Aggreg,PrimeProv)
  
  
}

 write.table(PrimeAcqui_Aggreg,paste(Chemin_application_resultat,"/PrimeAcquise_agg_20072025.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

###### evaluaton de la PREC au 31/12/2017
 Table_PREC_2017 = PrimeAnalyse[as.Date(PrimeAnalyse$fin_garantie, "%d/%m/%Y")>as.Date("31/12/2017", "%d/%m/%Y") ,]
 #Table_PREC_2017$AcquisitionPrec= AnneePoliceAcquisition(Table_PREC_2017$date_effet,Table_PREC_2017$fin_garantie,"01/01/2018","31/12/2030")
 Table_PREC_2017$TypePrec100 = (as.Date(Table_PREC_2017$fin_garantie, "%d/%m/%Y")>as.Date("31/12/2018", "%d/%m/%Y"))*1 
 Table_PREC_2017$AcquisitionPrec72= AnneePoliceAcquisition(Table_PREC_2017$date_effet,Table_PREC_2017$fin_garantie,"01/01/2018","31/12/2018")
 Table_PREC_2017$AcquisitionPrec100= AnneePoliceAcquisition(Table_PREC_2017$date_effet,Table_PREC_2017$fin_garantie,"01/01/2019","31/12/2030")
 
    
 write.table(Table_PREC_2017,paste(Chemin_application_resultat,"/Table_PREC_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  
 
##### Sinitres ####
Seuil_Gravite= 100e6
Sinistre_2016=read.csv2(paste(Chemin_Input,"sinistres_detaillées_31122016_definitif.csv",sep=""  ))   

Sinistre_2016$reglement_exercice[is.na(Sinistre_2016$reglement_exercice)]=0
Sinistre_2016$cumul_paye[is.na(Sinistre_2016$cumul_paye)]=0
Sinistre_2016$provision_depense_final[is.na(Sinistre_2016$provision_depense_final)]=0
Sinistre_2016$recours_exercice[is.na(Sinistre_2016$recours_exercice)]=0
Sinistre_2016$cumul_recours[is.na(Sinistre_2016$cumul_recours)]=0
Sinistre_2016$prevision_recours_final[is.na(Sinistre_2016$prevision_recours_final)]=0


levels(Sinistre_2016$branche)<-c("Assistance","D.A.B","Dommages corporels","Incendie", "Sante", "Resp. civile","Transport","Auto")
Sinistre_2016$an_surv =ExtractionAnnee(Sinistre_2016$survenance) 
Sinistre_2016$an_Declaration =  ExtractionAnnee(Sinistre_2016$declaration)

#### Resume des sinistralit?s par suvernance, Branche, Canal,Segment

Sinistre_2016$Charge = Sinistre_2016$cumul_paye + Sinistre_2016$provision_depense_final

Sinistre_2016$reglement_exercice_net = Sinistre_2016$reglement_exercice - Sinistre_2016$recours_exercice
Sinistre_2016$cumul_paye_net = Sinistre_2016$cumul_paye - Sinistre_2016$cumul_recours
Sinistre_2016$provision_net = Sinistre_2016$provision_depense_final - Sinistre_2016$prevision_recours_final
Sinistre_2016$Charge = Sinistre_2016$cumul_paye + Sinistre_2016$provision_depense_final
Sinistre_2016$Charge_net = Sinistre_2016$cumul_paye_net + Sinistre_2016$provision_net

Sinistre_2016$NbSin=1

Vec_Variable = c("Charge","cumul_paye","provision_depense_final","reglement_exercice","Charge_net","cumul_paye_net","provision_net" ,"reglement_exercice_net", "NbSin")

Sinistre_Analyse=Sinistre_2016[Sinistre_2016$an_surv>=1980,]

Sinistre_2016_grave = Sinistre_2016[Sinistre_2016$Charge_net>=Seuil_Gravite &Sinistre_2016$an_surv>=1980,]
Sinistre_2016_attritionnel =  Sinistre_2016[Sinistre_2016$Charge_net<Seuil_Gravite & Sinistre_2016$an_surv>=1980 ,]
table(Sinistre_2016_grave$an_surv)

Sinistre_Aggreg_2016<-aggregate(Sinistre_Analyse[,Vec_Variable], list(Surv=Sinistre_Analyse$an_surv,Declaratin=Sinistre_Analyse$an_Declaration, Branche=Sinistre_Analyse$branche),  FUN = SumDouble, na.rm = TRUE)
Sinistre_Aggreg_attri_2016<-aggregate(Sinistre_2016_attritionnel[,Vec_Variable], list(Surv=Sinistre_2016_attritionnel$an_surv,Declaratin=Sinistre_2016_attritionnel$an_Declaration, Branche=Sinistre_2016_attritionnel$branche),  FUN = SumDouble, na.rm = TRUE)


#write.table(PrimeAnalyse[PrimeAnalyse$AnneeEffet==2016,],paste(Chemin_application_resultat,"/CAS2016.csv",sep=""),sep=";",dec=",", row.names = FALSE)  
write.table(Sinistre_Aggreg_2016,paste(Chemin_application_resultat,"/Sinistre_Agg_2016.csv",sep=""),sep=";",dec=",", row.names = FALSE)  
#write.table(PrimeProv,paste(Chemin_application_resultat,"/PrimeAcquise2016.csv",sep=""),sep=";",dec=",", row.names = FALSE)  
write.table(Sinistre_Aggreg_attri_2016,paste(Chemin_application_resultat,"/Sinistre_Agg_attri_2016.csv",sep=""),sep=";",dec=",", row.names = FALSE)  
write.table(Sinistre_2016_grave,paste(Chemin_application_resultat,"/Sinistre_2016_grave.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

##### 2017
Sinistre_2017=read.csv2(paste(Chemin_Input,"sinistres_IP_detaillées_31122017_vu_03022018.csv",sep=""  ))   

###  ####

Sinistre_2017$reglement_exercice[is.na(Sinistre_2017$reglement_exercice)]=0
Sinistre_2017$cumul_paye[is.na(Sinistre_2017$cumul_paye)]=0
Sinistre_2017$provision_fin=Sinistre_2017$provision_depense

Sinistre_2017$provision_fin[is.na(Sinistre_2017$provision_fin)]=0
Sinistre_2017$recours_exercice[is.na(Sinistre_2017$recours_exercice)]=0
Sinistre_2017$cumul_recours[is.na(Sinistre_2017$cumul_recours)]=0
Sinistre_2017$prevision_fin =Sinistre_2017$prevision_recours
Sinistre_2017$prevision_fin[is.na(Sinistre_2017$prevision_fin)]=0


 ## ## ##
#  #  #  #

levels(Sinistre_2017$branche)<-c("Assistance","D.A.B","Dommages corporels","Incendie", "Sante", "Resp. civile", "Risques Agricoles","Transport","Transport","Transport","Transport","Transport","Auto")
Sinistre_2017$an_surv =ExtractionAnnee(Sinistre_2017$survenance) 
Sinistre_2017$an_Declaration =  ExtractionAnnee(Sinistre_2017$declaration)

#### Resume des sinistralit?s par suvernance, Branche, Canal,Segment

Sinistre_2017$Charge = Sinistre_2017$cumul_paye + Sinistre_2017$provision_fin

Sinistre_2017$reglement_exercice_net = Sinistre_2017$reglement_exercice - Sinistre_2017$recours_exercice
Sinistre_2017$cumul_paye_net = Sinistre_2017$cumul_paye - Sinistre_2017$cumul_recours
Sinistre_2017$provision_net = Sinistre_2017$provision_fin - Sinistre_2017$prevision_fin
Sinistre_2017$Charge = Sinistre_2017$cumul_paye + Sinistre_2017$provision_fin
Sinistre_2017$Charge_net = Sinistre_2017$cumul_paye_net + Sinistre_2017$provision_net

Sinistre_2017$NbSin=1

by(Sinistre_2017$cumul_paye, Sinistre_2017$an_surv, FUN = SumDouble)


Vec_Variable = c("Charge","cumul_paye","provision_fin","reglement_exercice","Charge_net","cumul_paye_net","provision_net" ,"reglement_exercice_net", "NbSin","prevision_recours","cumul_recours")
liste_by= c("an_surv","an_Declaration" , "branche" )
Sinistre_Analyse=Sinistre_2017#[Sinistre_2017$an_surv>=2000,]
Sinistre_Aggreg_2017<-aggregate(Sinistre_Analyse[,Vec_Variable],by=Sinistre_Analyse[,liste_by],  FUN = SumDouble, na.rm = TRUE)

write.table(Sinistre_Aggreg_2017,paste(Chemin_application_resultat,"/Sinistre_Agg_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  


Sinistre_2017_grave = Sinistre_2017[Sinistre_2017$Charge_net>=Seuil_Gravite & Sinistre_2017$branche!="Sante",]
Sinistre_2017_attritionnel =  Sinistre_2017[Sinistre_2017$Charge_net<Seuil_Gravite|  Sinistre_2017$branche=="Sante" ,]
Sinistre_Analyse=Sinistre_2017_attritionnel#[Sinistre_2017$an_surv>=2000,]
Sinistre_Aggreg_attri_2017<-aggregate(Sinistre_Analyse[,Vec_Variable],by=Sinistre_Analyse[,liste_by],  FUN = SumDouble, na.rm = TRUE)

by(Sinistre_2017_attritionnel[Sinistre_2017_attritionnel$an_surv==2017,]$Charge_net, Sinistre_2017_attritionnel[Sinistre_2017_attritionnel$an_surv==2017,]$branche, FUN = SumDouble)

by(Sinistre_Aggreg_attri_2017[Sinistre_Aggreg_attri_2017$an_surv==2017,]$Charge_net, Sinistre_Aggreg_attri_2017[Sinistre_Aggreg_attri_2017$an_surv==2017,]$branche, FUN = SumDouble)


write.table(Sinistre_Aggreg_attri_2017,paste(Chemin_application_resultat,"/Sinistre_Agg_attri_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  
write.table(Sinistre_2017_grave,paste(Chemin_application_resultat,"/Sinistre_grave_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  


## fusions analyses primes et sinsitres 

### Tableau pour analyse sinstralité ###
dim(Sinistre_Aggreg_2017[Sinistre_Aggreg_2017$an_surv>=2010,])
Vec_Variable = c("Charge","cumul_paye","provision_fin","reglement_exercice","Charge_net","cumul_paye_net","provision_net" ,"reglement_exercice_net", "NbSin","prevision_recours","cumul_recours")
Sin_pour_analyse<-aggregate(Sinistre_Aggreg_2017[Sinistre_Aggreg_2017$an_surv>=2010,Vec_Variable],by=Sinistre_Aggreg_2017[Sinistre_Aggreg_2017$an_surv>=2010,c("an_surv","branche")],  FUN = SumDouble, na.rm = TRUE)
colnames(Sin_pour_analyse)[1]="Annee"

Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Acquisition","NbrePolice")
prim_pour_analyse <-aggregate(PrimeAcqui_Aggreg[,Vec_Variable],by=PrimeAcqui_Aggreg[,c("AnneeAcquisition","branche")], FUN = SumDouble, na.rm=TRUE)
colnames(prim_pour_analyse)[1]="Annee"

Analyse_SP = merge(prim_pour_analyse, Sin_pour_analyse, by = intersect(names(Sin_pour_analyse),names(prim_pour_analyse)), all.x =TRUE , all.y = FALSE )

write.table(Analyse_SP,paste(Chemin_application_resultat,"/Analyse_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

##attri
dim(Sinistre_Aggreg_2017[Sinistre_Aggreg_2017$an_surv>=2010,])
Vec_Variable = c("Charge","cumul_paye","provision_fin","reglement_exercice","Charge_net","cumul_paye_net","provision_net" ,"reglement_exercice_net", "NbSin","prevision_recours","cumul_recours")
Sin_pour_analyse<-aggregate(Sinistre_Aggreg_attri_2017[Sinistre_Aggreg_attri_2017$an_surv>=2010,Vec_Variable],by=Sinistre_Aggreg_attri_2017[Sinistre_Aggreg_attri_2017$an_surv>=2010,c("an_surv","branche")],  FUN = SumDouble, na.rm = TRUE)
colnames(Sin_pour_analyse)[1]="Annee"

Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Acquisition","NbrePolice")
prim_pour_analyse <-aggregate(PrimeAcqui_Aggreg[,Vec_Variable],by=PrimeAcqui_Aggreg[,c("AnneeAcquisition","branche")], FUN = SumDouble, na.rm=TRUE)
colnames(prim_pour_analyse)[1]="Annee"

Analyse_SP_attri = merge(prim_pour_analyse, Sin_pour_analyse, by = intersect(names(Sin_pour_analyse),names(prim_pour_analyse)), all.x =TRUE , all.y = FALSE )

write.table(Analyse_SP_attri,paste(Chemin_application_resultat,"/Analyse_SP_attri.csv",sep=""),sep=";",dec=",", row.names = FALSE)  




##### triangle de payés  ######

Fichier_reglements=read.csv2(paste(Chemin_Input,"reglements_sinistres_2005_2017_vu_12_2017.csv",sep=""  ))   


  ### fichier reglements hors sinistres graves 

liste_reglements_sinstregrave = Fichier_reglements$sinistre %in%  Sinistre_2017_grave$numero_sinistre
Fichier_reglements_attri = Fichier_reglements[!liste_reglements_sinstregrave,]

write.table(Fichier_reglements_attri,paste(Chemin_application_resultat,"/Reglement_attri.csv",sep=""),sep=";",dec=",", row.names = FALSE)  



###### Analyse de l'évolution des Branches #####
    ### Croissance de notre chiffre d'affaires par Branche ####
    #### Evolution 


by(PrimeAnalyse$pnette, PrimeAnalyse$exercice, FUN = SumDouble)


Sin_Auto = Sinistre_2017[Sinistre_2017$branche=="Auto",]

Charge_Auto_2017 <-aggregate(Sin_Auto[,Vec_Variable],by=PrimeAcqui_Aggreg[,c("AnneeAcquisition","branche")], FUN = SumDouble, na.rm=TRUE)

Vec_Variable = c("Charge_net")
by_variable = c("ancpolice" , "origin", "numero_sinistre")

Charge_Auto_2017 <-aggregate(Sin_Auto[Sin_Auto$an_surv==2017,Vec_Variable],by=Sin_Auto[Sin_Auto$an_surv==2017,by_variable], FUN = SumDouble, na.rm=TRUE)

Vec_Variable = c("Charge_net")
by_variable = c("police" , "origin", "numero_sinistre")

Charge_Auto_2016 <-aggregate(Sinistre_2016[(Sinistre_2016$an_surv==2016 & Sinistre_2016$branche=="Auto"),Vec_Variable],by=Sinistre_2016[(Sinistre_2016$an_surv==2016 & Sinistre_2016$branche=="Auto"),by_variable], FUN = SumDouble, na.rm=TRUE)



hist(Sin_Auto[Sin_Auto$an_surv==2017,]$Charge_net)
plot(density(Sin_Auto[Sin_Auto$an_surv==2017,]$Charge_net),type = 'l')
summary(Sin_Auto[Sin_Auto$an_surv==2017,]$Charge_net)





###### Analyse Fine de la maladie ####
##" Ajout des critère Produit et intermédiaire dans les résumés.

  ### Le résumé de la production ####





Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Acquisition","NbrePolice")


by_variable = c("Annee_Emission","branche" , "typeinter", "codepartenaire","intermediaire","produit")

PrimeAnalyse$AcquisitionPrec=0
iAnnee=2007

PrimeAnalyse$Acquisition = AnneePoliceAcquisition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie,paste0("01/01/",iAnnee),paste0("31/12/",iAnnee))
#PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$AcquisitionPrec = AnneePoliceAcquisition(PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$date_effet,PrimeAnalyse[PrimeAnalyse$Annee_Emission==iAnnee,]$fin_garantie,paste0("01/01/",iAnnee+1),"31/12/2020")
PrimeAnalyse$util = (PrimeAnalyse$Valide)&(PrimeAnalyse$Acquisition>0)
PrimeAnalyse$NbrePolice=1*PrimeAnalyse$util

PrimeAnalyse$PrimeAcquise = PrimeAnalyse$pnette_coass *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
#PrimeAnalyse$Prec = (PrimeAnalyse$pnette_coass *PrimeAnalyse$AcquisitionPrec)/PrimeAnalyse$AnneePolice
PrimeAnalyse$PrimeAcquiseNetCommission=   PrimeAnalyse$pnette_coass_com *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
#Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Acquisition")
Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","NbrePolice","Acquisition")


PrimeProv <-aggregate(PrimeAnalyse[,Vec_Variable],by=PrimeAnalyse[,by_variable], FUN = SumDouble, na.rm=TRUE)
PrimeProv$AnneeAcquisition=iAnnee
   
PrimeAcqui_Detail = PrimeProv

for(iAnnee in 2008:2025)
{
  PrimeAnalyse$AcquisitionPrec=0
  PrimeAnalyse$Acquisition = AnneePoliceAcquisition(PrimeAnalyse$date_effet,PrimeAnalyse$fin_garantie,paste0("01/01/",iAnnee),paste0("31/12/",iAnnee))
  PrimeAnalyse$util = (PrimeAnalyse$Valide)&(PrimeAnalyse$Acquisition>0)
  PrimeAnalyse$NbrePolice=1*PrimeAnalyse$util
  PrimeAnalyse$PrimeAcquise = PrimeAnalyse$pnette_coass *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
  PrimeAnalyse$PrimeAcquiseNetCommission=   PrimeAnalyse$pnette_coass_com *PrimeAnalyse$Acquisition/PrimeAnalyse$AnneePolice
  PrimeProv <-aggregate(PrimeAnalyse[,Vec_Variable],by=PrimeAnalyse[,by_variable], FUN = SumDouble, na.rm=TRUE)
  
  PrimeProv$AnneeAcquisition=iAnnee
  
  PrimeAcqui_Detail = rbind(PrimeAcqui_Detail,PrimeProv)
  
  
}

write.table(PrimeAcqui_Detail,paste(Chemin_application_resultat,"/PrimeacquiDetail.csv",sep=""),sep=";",dec=",", row.names = FALSE)  


########## Sinistres ####### 

Vec_Variable = c("Charge","cumul_paye","provision_fin","reglement_exercice","Charge_net","cumul_paye_net","provision_net" ,"reglement_exercice_net", "NbSin","prevision_recours","cumul_recours")
liste_by= c("an_surv" , "branche","produit","intermediaire")

Sinistre_Analyse=Sinistre_2017[Sinistre_2017$an_surv>=2010,]
Sinistre_Detail_2017<-aggregate(Sinistre_Analyse[,Vec_Variable],by=Sinistre_Analyse[,liste_by],  FUN = SumDouble, na.rm = TRUE)
write.table(Sinistre_Detail_2017,paste(Chemin_application_resultat,"/Sinistre_Detail_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  


#Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Acquisition","NbrePolice")
#by_variable= c("AnneeAcquisition","branche", "")

  ### ### ###
  # 
  #
  ### ### ###
Table_interemediare = PrimeAcqui_Detail[,c("typeinter","intermediaire")]
Table_interemediare=Table_interemediare[!duplicated(Table_interemediare$intermediaire),]

Table_client=
## Tableau pour analyse sinstralité ###

#sin
Vec_Variable = c("Charge","cumul_paye","provision_fin","reglement_exercice","Charge_net","cumul_paye_net","provision_net" ,"reglement_exercice_net", "NbSin","prevision_recours","cumul_recours")
by_variable = c("branche" ,"intermediaire","produit","an_surv")

Sin_pour_analyse<-aggregate(Sinistre_Detail_2017[Sinistre_Detail_2017$an_surv>=2010,Vec_Variable],by=Sinistre_Detail_2017[,by_variable],  FUN = SumDouble, na.rm = TRUE)
colnames(Sin_pour_analyse)[4]="Annee"

Sin_pour_analyse2=merge(Sin_pour_analyse, Table_interemediare, by=c("intermediaire"),  all.x =TRUE , all.y = FALSE )
write.table(Sin_pour_analyse2,paste(Chemin_application_resultat,"/Sin_pour_analyse_det_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  

#prime
Vec_Variable = c("PrimeAcquise","PrimeAcquiseNetCommission","Acquisition","NbrePolice")
by_variable = c("branche" , "typeinter","intermediaire","produit","AnneeAcquisition")
prim_pour_analyse <-aggregate(PrimeAcqui_Detail[,Vec_Variable],by=PrimeAcqui_Detail[,by_variable], FUN = SumDouble, na.rm=TRUE)
prim_pour_analyse=prim_pour_analyse[prim_pour_analyse$Annee>2009 &prim_pour_analyse$Annee<2018,]
colnames(prim_pour_analyse)[5]="Annee"


Analyse_SP_Detail = merge(prim_pour_analyse, Sin_pour_analyse2, by = c("intermediaire" ,"branche"  ,     "produit"   ,    "Annee" ), all.x =TRUE , all.y = FALSE )

Analyse_SP_Detail = merge(prim_pour_analyse, Sin_pour_analyse2, by = intersect(names(prim_pour_analyse),names(Sin_pour_analyse2)), all.x =TRUE , all.y = TRUE )


write.table(Analyse_SP_Detail,paste(Chemin_application_resultat,"/Analyse_detail_2017.csv",sep=""),sep=";",dec=",", row.names = FALSE)  




by(Analyse_SP_Detail[Analyse_SP_Detail$Annee==2016,]$Charge, Analyse_SP_Detail[Analyse_SP_Detail$Annee==2016,]$branche, FUN=SumDouble)
by(Sin_pour_analyse2[Sin_pour_analyse2$Annee==2016,]$Charge, Sin_pour_analyse2[Sin_pour_analyse2$Annee==2016,]$branche, FUN=SumDouble)
by(prim_pour_analyse[Sin_pour_analyse2$Annee==2017,]$p, Sin_pour_analyse2[Sin_pour_analyse2$Annee==2017,]$branche, FUN=SumDouble)

###### analyse des Malis 2017 - 2018 #####

#### identification des déclarations tardives ####



#### indentifications des variations des charges #####