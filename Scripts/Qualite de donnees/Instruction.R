require(RMySQL)
require(compare)
require(stringr)
require(stringdist)
require(plyr)
require(reshape2)
# 

###  Preparer les principales tables
##code des clients.
  ### 
  

mydb = dbConnect(MySQL(), user=Sys.getenv('MYSQL_USER'), password=Sys.getenv('MYSQL_PASSWORD'), dbname='Suivi_Prod', host='127.0.0.1')
dbListTables(mydb)

Production_2016 <- read.csv2("<CHEMIN_LOCAL>/Production_2016.csv", stringsAsFactors=FALSE)
 View(Production_2016)
Chemin = "<CHEMIN_LOCAL>/"
Production_2016 =  read.csv2(paste0(Chemin, "Production_2016.csv"), stringsAsFactors=FALSE)
#RegPron = "Le|La|L|l|le|la|les|-|\\.|\\s|/"

######  Connexion a la BD #####

  ### 1 Ajout de la table Branche
Table_branche =  read.csv2(paste0(Chemin, "Table_bd/tbBranche.csv"), stringsAsFactors=FALSE)
colnames(Table_branche)
colnames(Table_branche)= c("Code_branche", "Libelle_Branche")
 
#Table_branche$Libelle_Branche <- iconv(Table_branche$Libelle_Branche, from="UTF-8", to="LATIN1")

#RegBranche = "\\216|La|L|l|le|la|les|-|\\.|\\s|/"
Libelle= gsub(RegPron,"",Libelle)

#gsub("\216", "é", Table_branche$Libelle_Branche) 
dbWriteTable(mydb, "Branche", Table_branche,append=TRUE, row.names=FALSE, overwrite=FALSE)


### 2 Ajout Table Client ####

Client_init = BaseFusionPrime$client
#Lib_Client = str_trim(Production_2016$client)
#Lib_client_retravaille = gsub(RegPron,"",Lib_Client)

Codification_client =Codification(Client_init)
#Code_client = Codification_client$Code

Lib_client_retravaille =  Codification_client$Lib_travaille
Code_client = Codification_client$Code

#Production_2016$Code_client = Code_client

#length(unique(Lib_Client))

length(unique(Lib_client_retravaille))

length(unique(Code_client))
  
  ### Check sur l origine des doublons dans les listes de 
  Liste_client_analyse_doublon=Comparaison_vecteur(Codification_client$Lib_init,Codification_client$Lib_travaille)
  length(Resultat$Liste_cas_unique)
  writeLines( Liste_client_analyse_doublon,paste0(Chemin_resultat,"Liste_client_doublon.csv"), sep =";", dec = ".")
  
  capture.output(Liste_client_analyse_doublon$Liste_cas_unique, file = paste0(Chemin_resultat,"Liste_client_doublon.csv"))
  
  # Recodification de Lib_client
    Lib_client_final = Resultat$Vec_Recodifie

    TT= cbind( Client_init, Lib_client_retravaille, Lib_client_final,Code_client )
    Table_client = data.frame(TT, row.names = NULL)

    Table_client_unique =Table_client[!duplicated(Lib_client_final),c("Code_client", "Lib_client_final")]

  colnames(Table_client_unique)
  colnames(Table_client_unique)=c( "Code_client","Lib_Client")

# Produciton de la table client
  write.table(Table_client_unique,paste0(Chemin_resultat,"tbClient.csv"), sep =";", dec = ".")
  dbWriteTable(mydb, "Clients", Table_client_unique,append=TRUE, row.names=FALSE, overwrite=FALSE)

  
#dbWriteTable(mydb, "Clients", Table_client_unique,append=TRUE, row.names=FALSE, overwrite=FALSE)

## 


###### 3 Ajout Table Police #####

    
 
          
     
    ### Formaliser la procedure qui permet d ajouter des polices a la table polices.
          #  Les outputs, les polices ajoutees
          # les polices non ajoutes.
         Res_fiabilisation_police= Fiabiliser_police(Production_2016$ancienpol)
         
         Table_branche$Libelle_Branche=gsub("  "," ",Table_branche$Libelle_Branche)
         # Ajout des libelle de branche dans la table production 2016
         
         Production_2016$Code_police_travaille =Res_fiabilisation_police$Vec_police_travaille
         Prod_police=  merge(Production_2016, Table_branche, by.x ="branche",by.y="Libelle_Branche")
         
        # e 
       
         Table_police = merge(Prod_police[,c("Code_police_travaille","police", "Code_branche", "Code_client","date_effet","fin_garantie")], Res_fiabilisation_police$Table_police, by.x ="Code_police_travaille", by.y="Liste_Police_OK")
         colnames(Table_police)
         length(unique(Table_police$Code_police_travaille))
         length(Res_fiabilisation_police$Liste_Police_OK)
         
         Table_Police_Unique = Table_police[!duplicated(Table_police$Code_police_travaille),c("Code_police_travaille", "Liste_Pol_Index_Garantie", "Code_branche" ,"Liste_Pol_numeric" )]
         colnames(Table_Police_Unique) = c("Ancien_Police", "Code_Produit", "Code_branche","numero_police") 
         
         
         write.table(Table_Police_Unique,paste0(Chemin,"Table_bd/tbPolice.csv"), sep =";", dec = ".")
         dbWriteTable(mydb, "Police", Table_Police_Unique,append=TRUE, row.names=FALSE, overwrite=FALSE)
         
      
         
          
         
          
         ###  Table sur les intermediaires ####
         ### Les codes intermediaires utilises en interne doivent etre revus
         Production_2016_3 =  read.csv2(paste0(Chemin, "Production_2016_3.csv"), stringsAsFactors=FALSE)
       #  Production_2016_Transpo = Production_2016_Transpo[Production_2016_Transpo$branche =="Transport",]
         
          
      
        Codification_interm =Codification(BaseFusionPrime$intermediaire)

        
        #length(unique(Lib_Client))
        
        length(unique(Codification_interm$Lib_travaille))
        length(unique(Codification_interm$Code))
        length(unique(BaseFusionPrime$codeinterm))
        
          #  ==> notre codificqtion est de meilleure qualite que celle du systeme
        
        ### Check sur l origine des doublons dans les listes de 
       Resultat_interm2=Comparaison_vecteur(BaseFusionPrime$codeinterm,Codification_interm$Code)
       length(unique( Resultat_interm2$Vec_Recodifie))
       
       ex=runif(1,1,7)
      unique( BaseFusionPrime$codeinterm[Resultat_interm2$Liste_cas_position[[ex]]])
       unique( BaseFusionPrime$intermediaire[Resultat_interm2$Liste_cas_position[[ex]]])
       unique( Codification_interm$Code[Resultat_interm2$Liste_cas_position[[ex]]])
       
       Resultat_interm2$Liste_cas_unique
           ## Check de similitude sur les noms des intermediaires.
        
       
       BaseFusionPrime$New_Code_interm =Codification_interm$Code
       BaseFusionPrime$New_LibInter =Resultat_interm2$Vec_Recodifie
       BaseFusionPrime$id_typeInter = as.factor(BaseFusionPrime$typeinter)
       
      
       
        Table_interm = BaseFusionPrime[!duplicated(BaseFusionPrime$New_Code_interm),c("New_LibInter","New_Code_interm", "typeinter")]
        
         
        
            ### 4 Table sur les types d'intermediaires ####
            Table_typeIntermediaire =cbind(unique(Table_interm$typeinter), c(1,2,3))
            Table_typeIntermediaire=data.frame(Table_typeIntermediaire)
            colnames(Table_typeIntermediaire)= c("Lib_type_intermediaire","id_Type_Intermediaire")
            
            write.table(Table_typeIntermediaire,paste0(Chemin,"Table_bd/tbtypeIntermediaire.csv"), sep =";", dec = ".")
            dbWriteTable(mydb, "Type_Intermediaire", Table_typeIntermediaire,append=TRUE, row.names=FALSE, overwrite=FALSE)
            
          #####  5 Finalisation insertion table intermediaire ###
            Table_interm_2 = merge(Table_interm,Table_typeIntermediaire, by.x = "typeinter" , by.y = "Lib_type_intermediaire")
            Table_interm_unique = Table_interm_2[,c("New_LibInter", "New_Code_interm","id_Type_Intermediaire")]
            colnames(Table_interm_unique)
            colnames(dbReadTable(mydb,"Intermediaires"))
           
           colnames(Table_interm_unique) =colnames(dbReadTable(mydb,"Intermediaires")) [c(2:4)]
            write.table(Table_interm_unique,paste0(Chemin,"Table_bd/tbIntermediaires.csv"), sep =";", dec = ".")
            dbWriteTable(mydb, "Intermediaires", Table_interm_unique,append=TRUE, row.names=FALSE, overwrite=FALSE)
    
      
              
      #### 7.Insertion de la production ####
            #client ok
            # code inter 
              mean(Production_2016$intermediaire %in% Table_interm_unique$lib_intermediaire)
              liste_Inter_noninclus=unique(Production_2016$intermediaire[!Production_2016$intermediaire %in% Table_interm_unique$lib_intermediaire])
              
              Production_2016$code_intermediaire=  Table_interm_unique$code_intermediaire[match(Production_2016$intermediaire,Table_interm_unique$lib_intermediaire,nomatch = NA_integer_)]
  
                #166 cas de intermediares non retrouves reprennsant 13 cas unique
            #Date_emission  
              Production_2016$annee_emission = Production_2016$exercice
            #date effet
              Production_2016$Date_effet = as.Date(Production_2016$date_effet, "%d/%m/%y")
              Production_2016$Date_fin = as.Date(Production_2016$fin_garantie, "%d/%m/%y")
              Production_2016$DureeJour =  as.numeric(Production_2016$Date_fin -Production_2016$Date_effet)
              
              
              Table_Production = Production_2016[c()]
            colnames(dbReadTable(mydb,"Production"))
            colnames(Production_2016)
            
            write.table(Production_2016,paste0(Chemin,"Table_bd/tbProduction.csv"), sep =";", dec = ".")
            
            Ex_Prod= dbReadTable(mydb,"Production")
            
            
    #### J ai besoin de la liste des polices 2016 et 30/06 2017 qui sont valides cad:
              # les num polices identifiables
              # les date d effet < date d echeance
              # les codes polices dans le bon format
              # les intermediaires et clients sont identifialbles.
            
        # sur cette base je ferai l analyse de la structure des polices. Celles qui se repete plusieurs foirs
        # je ferai le calcul des precs pour voir si c est normal que la prec a baisse ou pas.
            
        # je 
            