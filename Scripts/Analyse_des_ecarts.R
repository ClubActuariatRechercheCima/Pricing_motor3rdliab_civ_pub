##### Lecture des INPUTS POUR BCP ####
###
Chemin_PARAMETRES_BCP="<CHEMIN_LOCAL>/"
chemin_base_res_analyse  ="<CHEMIN_LOCAL>/"

####### Lecture des tables d'input #######

#Brute_IT =read.csv2(paste(Chemin_base,"tcka53_aout_2017.csv",sep=""  ), stringsAsFactors = FALSE)   ## Donnees Brutes

liste_activite =read.csv2(paste(Chemin_PARAMETRES_BCP,"liste_activite.csv",sep=""), colClasses = c("character", "character"))   ## Donnees Brutes
bd_garanties_IT =read.csv2(paste(Chemin_PARAMETRES_BCP,"bd_garanties_IT.csv",sep=""  ), stringsAsFactors = FALSE)   ## Donnees des garanties et des traités
bd_garanties_traites =read.csv2(paste(Chemin_PARAMETRES_BCP,"bd_garanties_traites.csv",sep=""  ), stringsAsFactors = FALSE)   ## Donnees des garanties et des traités

bd_traite_edp =read.csv2(paste(Chemin_PARAMETRES_BCP,"bd_traites_edp.csv",sep=""  ), stringsAsFactors = FALSE)   ## Donnees des garanties et des traités
reduction_traites_prop =read.csv2(paste(Chemin_PARAMETRES_BCP,"reduction_traites_prop.csv",sep=""  ), stringsAsFactors = FALSE, dec=",")   ## Reduction des traites
####





###############################•
### Ce traitement necessite  les tables ####
### Data_collecte_garantie, Fichier_situation et Data_pret_bcp du traitement_initial
###

#### Type d'analyse
#### Sur les polices qui sont Bien saisies dans les données de l'informatiques
#### et qui ont aus##### si fait l'objet de ressaisies, Dans quelles mesures les données sont comparab les tables les
### Data_collecte_garantie, Fichier_situation et Data_pret_bcp du traitement_initial
###

A1_Liste_saisi_ressaisie = Fichier_situation$idunique[(Fichier_situation$NonSaisi==FALSE) & (Fichier_situation$Garanties_Resaisies==TRUE)]
### 


#### Construire un fichier BCP restraint à ces données
A1_Input_BCP_Saisi= Brute_IT_Inc_DAB_cor[Brute_IT_Inc_DAB_cor$idunique %in%A1_Liste_saisi_ressaisie ,]    

Liste_ret= c("Coa_garantie","Coa_lib_garantie","Coa_pnette_garantie","Coa_capital_garantie","Coa_smp_garantie")
Liste_X = colnames(A1_Input_BCP_Saisi[,! colnames(A1_Input_BCP_Saisi) %in% Liste_ret] ) 
Liste_Y= c("idunique","Coa_garantie", "Coa_lib_garantie","Coa_capital_garantie" ,"Coa_smp_garantie", "Coa_pnette_garantie")
Info_Sans_cap_prim =Brute_IT_Inc_DAB_cor[Brute_IT_Inc_DAB_cor$idunique %in% A1_Liste_saisi_ressaisie,Liste_X]  
Info_Sans_cap_prim = Info_Sans_cap_prim[!duplicated(Info_Sans_cap_prim),]
A1_Input_BCP_ReSaisi = merge(Info_Sans_cap_prim, Data_collecte_garantie[Data_collecte_garantie$idunique %in% A1_Liste_saisi_ressaisie ,Liste_Y], by = "idunique",all.x =FALSE , all.y = TRUE )
#colnames(A1_Input_BCP_ReSaisi)[17:21]<-c("Coa_garantie","Coa_lib_garantie","Coa_capital_garantie","Coa_smp_garantie","Coa_pnette_garantie")
### Avec Les données saisies

### Traitement BCP
Vec_Colonne_utile = c("Coa_branche", "Coa_garantie" , "Coa_lib_garantie" , "Coa_client" , "Coa_ancien_pol", "Coa_police","Coa_date_effet","Coa_date_fin","Coa_capital_garantie" , "Coa_smp_garantie" ,"Coa_pnette_garantie",  "Coa_taux_garantie"  )
A1_info_brute_police_ = A1_Input_BCP_Saisi[ ,Vec_Colonne_utile ]
colnames(A1_info_brute_police_saisi) =  c("Coa_branche", "Coa_garantie" , "Coa_lib_garantie", "assure" , "ancien_pol", "nouveau_pol","date_effet","date_fin","capital_100" , "smp_100" ,"pnette_100",  "taux_net_coass"  )

A1_Table_Pret_BCP_saisi=Traitement_Preparatoire(A1_info_brute_police_saisi,liste_activite,bd_garanties_IT,bd_garanties_traites,reduction_traites_prop, paste0(chemin_base_res_analyse,"A1/Saisi/A1_saisi_"))
A1_Res_saisi= Traitement_BCP(A1_Table_Pret_BCP_saisi,bd_traite_edp,paste0(chemin_base_res_analyse,"A1/Saisi/A1_saisi_"))



### Traitement BCP   ### Avec Les données resaisies

Vec_Colonne_utile = c("Coa_branche", "Coa_garantie" , "Coa_lib_garantie" , "Coa_client" , "Coa_ancien_pol", "Coa_police","Coa_date_effet","Coa_date_fin","Coa_capital_garantie" , "Coa_smp_garantie" ,"Coa_pnette_garantie",  "Coa_taux_garantie"  )
A1_info_brute_police_Resaisi = A1_Input_BCP_ReSaisi[ ,Vec_Colonne_utile ]
colnames(A1_info_brute_police_Resaisi) =  c("Coa_branche", "Coa_garantie" , "Coa_lib_garantie", "assure" , "ancien_pol", "nouveau_pol","date_effet","date_fin","capital_100" , "smp_100" ,"pnette_100",  "taux_net_coass"  )

A1_Table_Pret_BCP_Resaisi=Traitement_Preparatoire(A1_info_brute_police_Resaisi,liste_activite,bd_garanties_IT,bd_garanties_traites,reduction_traites_prop, paste0(chemin_base_res_analyse,"A1/ReSaisi/A1_Resaisi_"))
A1_Res_Resaisi= Traitement_BCP(A1_Table_Pret_BCP_Resaisi,bd_traite_edp,paste0(chemin_base_res_analyse,"A1/ReSaisi/A1_Resaisi_"))


