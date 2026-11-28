'Ajout titre au debut'
###### LISTE URGENTE DE RECHERCHE DE CAPITAUX CROISEE AVEC LES VENDEURS ET INTERMEDIAIRES ######
Chemin_input= paste0(Chemin_Travaux,"Input/")
Chemin_Output= paste0(Chemin_Travaux,"Resultats/")
Chemin_Anomalie =paste0(Chemin_Travaux,"Anomalies/")


LISTE_CAPITAUX_ANOMALIES=read.csv2(paste(Chemin_Anomalie,"liste_capitaux_anomalie.csv",sep=""),stringsAsFactors = FALSE)
#### CALCUL DES PRIMES ANNUALISEE 
LISTE_CAPITAUX_ANOMALIES$Durerpolice=DureePolice(LISTE_CAPITAUX_ANOMALIES$date_effet,LISTE_CAPITAUX_ANOMALIES$date_fin)
LISTE_CAPITAUX_ANOMALIES$Prime_Annualisee=365*LISTE_CAPITAUX_ANOMALIES$Primes_aggreges/LISTE_CAPITAUX_ANOMALIES$Durerpolice
list_LISTE=c("ancien_pol","assure","date_effet","date_fin","risque_sunu_services","Primes_aggreges","Prime_Annualisee")

LISTE_EXTRAIRE=LISTE_CAPITAUX_ANOMALIES[LISTE_CAPITAUX_ANOMALIES$Prime_Annualisee>=1000000,list_LISTE]

###### ON AJOUTE INTERMEDIAIRE

Production=read.csv2(paste(Chemin_input,Fichier_Production_vendeurs,sep=""),stringsAsFactors = FALSE) 
List_prod=c("ancienpol","intermediaire","name","date_effet","fin_garantie")
Extrait_prod=Production[,List_prod]

##### LA RECHERCHE S'EFFECTUE UNIQUEMENT PAR LES NUMEROS DE POLICE
LISTE_EXTRAIRE$ancien_pol=Retraitement_police(LISTE_EXTRAIRE$ancien_pol)
Extrait_prod$ancienpol=Retraitement_police(Extrait_prod$ancienpol)

####Creation d'ID unique
LISTE_EXTRAIRE$ID=paste(LISTE_EXTRAIRE$ancien_pol,LISTE_EXTRAIRE$date_effet,sep="")
Extrait_prod$ID=paste(Extrait_prod$ancienpol,Extrait_prod$date_effet,sep="")


#View(LISTE_EXTRAIRE[LISTE_EXTRAIRE$ID %in% Extrait_prod$ID,] )
#View(LISTE_EXTRAIRE[!(LISTE_EXTRAIRE$ID %in% Extrait_prod$ID),] )

RECAP_TROUVE=merge(LISTE_EXTRAIRE,Extrait_prod[,c("ID","intermediaire","name")],by="ID",all.y = FALSE)
RECAP_NON_TROUVE=LISTE_EXTRAIRE[!(LISTE_EXTRAIRE$ID %in% Extrait_prod$ID),]
if (nrow(RECAP_NON_TROUVE)>0)
  {
 RECAP_NON_TROUVE$intermediaire=""
 RECAP_NON_TROUVE$name=""
 RECAP_RECHERCHE=rbind(RECAP_TROUVE,RECAP_NON_TROUVE)
 colnames(RECAP_RECHERCHE)[colnames(RECAP_RECHERCHE)=="name"]="Vendeur"
 
 }else
{
  RECAP_RECHERCHE = RECAP_TROUVE
}
####la liste des capitaux incendie à rechercher
write.table(RECAP_RECHERCHE[,2:10],paste(Chemin_Anomalie,"CAPITAUX_A RECHERCHER.csv",sep=""),sep=";",dec=",",row.names = FALSE)
