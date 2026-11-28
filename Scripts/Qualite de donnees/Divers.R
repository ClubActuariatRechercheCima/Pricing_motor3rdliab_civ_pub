Liste_code_client[c(runif(10,1,17570))]
Liste_client[c(runif(10,1,17570))]
nb_cod_client =sapply(Liste_code_client, FUN = nchar)

## retrouver les clients qui generent des codes non uniques

RegPron = "Le|La|L|l|le|la|les|-|\\.|\\s|/"

duplic_Indices = duplicated(Liste_code_client)
Dup_Client  = Liste_client[duplic_Indices]
Dup_code_Client  = Liste_code_client[duplic_Indices]


Indice_dup=match(Liste_code_client,Dup_code_Client)

length(unique(Vec_code_Client)) 

Position_dup_code= Liste_code_client %in% Dup_code_Client

## exemple
Position_dup_code =Liste_code_client %in% Dup_code_Client[runif(1, min=1, max = length(Dup_code_Client))] 

Liste_client[which(Position_dup_code)]
Liste_code_client[Position_dup_code]  
Liste_client[Position_dup_code ]

people = c("rori", "emilia", "matteo", "mehmet", "filipe", "anna", "tyler",
           "rasmus", "jacob", "youna", "flora", "adi")

grep(pattern = "t{2}", people, value = TRUE) 

strsplit(people,split="t{2}")

flavors = c("chocolate", "vanilla", "cinnamon", "mint", "lemon")
str_split(flavors, "[aeiou]")
strsplit(flavors,"[aeiou]" )




T1=system.time()


nDup_lib_client=lapply(Lib_Client, FUN = function(x)(return(sum( Lib_Client %in% x))))
nDup_lib_client2 =lapply(Lib_Client, FUN = function(x)(return(sum( Lib_Client== x))))

system.time(lapply(Lib_Client, FUN = function(x)(return(sum( Lib_Client== x)))))
nDup_lib_client_retravaille =lapply(Lib_client_retravaille, FUN = function(x)(return(sum( Lib_client_retravaille %in% x))))


cas_suivi2 = duplicated(Lib_client_retravaille)& (!duplicated(Lib_Client))
Cas2=which(cas_suivi2)[runif(1, min=1, max = sum(cas_suivi2))]
Lib_client_retravaille[Cas2]
Lib_Client[(Lib_client_retravaille %in% Lib_client_retravaille[Cas2])]


Cas_suivi = (nDup_lib_client==1)&(nDup_lib_client_retravaille>1)
Lib_client_retravaille[which(Cas_suivi)[runif(1, min=1, max = 154)]]

Cas=which(Cas_suivi)[runif(1, min=1, max = 154)]
Cas=9186
Lib_client_retravaille[Cas]
Lib_Client[(Lib_client_retravaille %in% Lib_client_retravaille[Cas])]


which((Lib_client_retravaille == Lib_client_retravaille[9186]))

Vec_CodePolAnalyse2 = grepl(pattern = "^[A-Za-z]{3}[0-9]{6,6}$",Code_police_travaille)

grepl(pattern = "^[A-Z]{3}[0-9]{6}$","MRD2703320")
"MRD2703320"
summary(Code_police_travaille[ Vec_Police_6chiffre] %in% Liste_Pol_numeric)


Liste_client = unique(Lib_Client)
length(unique(Lib_Client))

length(Codification(unique(Lib_Client)))
length(Codification(unique(Lib_client_retravaille)))
length(unique(Codification(Lib_Client)))
length(unique(Code_client))

length(Codification(unique(Lib_Client)))


nb_client = sapply(Liste_client, FUN = nchar)





length(unique(Lib_client_retravaille))
length(unique(Codification(Lib_client_retravaille)))

Liste_code_client = Codification(Liste_client)


length(unique(Liste_code_client))
length(unique(Liste_client))

nb_client = sapply(Liste_client, FUN = nchar)


Table_client=data.frame(cbind(Liste_client,Liste_code_client), row.names = NULL)
Table_client=data.frame(cbind(Liste_client,Liste_code_client), row.names = NULL)



colnames(Table_client)=c("Lib_Client", "Code_client")

## Mettre la table de client dans la BD ##
dbWriteTable(mydb, "Clients", Table_client[c("Lib_Client", "Code_client"),],append=TRUE, row.names=FALSE, over)

colnames(Table_client)=c("Lib_Client", "Code_client")


## Quelques exemples de polices dupliques dans la base
vec_dup_pol = duplicated(Table_police_hs_Transport$Code_police_travaille)


test=   Table_police_hs_Transport[Table_police_hs_Transport$Code_police_travaille== Table_police_hs_Transport$Code_police_travaille[    which(vec_dup_pol)[runif(1,min = 1, max = sum(vec_dup_pol)) ]],]


##### HORS POLICE TRANSPORT

## Construction des polices transports
Production_2016_Transpo =  read.csv2(paste0(Chemin, "Production_2016_3.csv"), stringsAsFactors=FALSE)
Production_2016_Transpo = Production_2016_Transpo[Production_2016_Transpo$branche =="Transport",]

## Traitement applicquee aux police

Code_police_initT = Production_2016_Transpo$
  
  #Codification_Pol =Codification(Code_police_init, bPolice = TRUE)
  Code_police_travailleT =  gsub("-|\\.|\\s|/","",Code_police_initT)

Vec_CodePolAnalyseT = grepl(pattern = "^[A-Za-z]{3}[0-9]{6,6}$",Code_police_travailleT)

Production_2016$Code_police_travaille = Code_police_travaille
## Liste des polices a corriger
Liste_Police_a_corriger =   unique(Code_police_travaille[!Vec_CodePolAnalyse2])
Base_Police_A_corriger = Production_2016[!Vec_CodePolAnalyse2 ,]
Liste_Police_corriger = unique(Code_police_travaille[Vec_CodePolAnalyse2])


table(Base_Police_A_corriger$intermediaire)
### Extraire la  liste des polices numerique de 6 qui pourra completer les autres.

Vec_Police_6_chiffres = grepl(pattern = "^[0-9]{6,6}$",Code_police_travaille)

Liste_Pol_Index_Garantie = str_sub(Liste_Police_corriger,1,3 )
Liste_Pol_numeric =  str_sub(Liste_Police_corriger,4,9 )


### Liste 002 correspond a la liste des polices qui ont 6 chiffres mais qui sont inclus dans la liste des polices correctes
Base_Police_A_corriger = Base_Police_A_corriger[!Base_Police_A_corriger$Code_police_travaille %in% Liste_Pol_numeric,] 


## ##
## ##
## ##

## selection

Table_branche$Libelle_Branche
Code_branche = Production_2016$branche
table(Production_2016$branche)

grepl(pattern = "[A-Z]{3}[0-9]{6}","MRDFFG2703320")

Code_police_travaille[which(Vec_CodePolAnalyse)[runif(5,1,3468)]]

sum(Production_2016$pnette[ Vec_CodePolAnalyse],na.rm = TRUE)/sum(as.double(Production_2016$pnette),na.rm = TRUE)

Table_Police_A_traiter = Production_2016[Vec_CodePolAnalyse,]
table(Table_Police_A_traiter$intermediaire)

## 
sum(Production_2016$pnette[ Vec_CodePolAnalyse],na.rm = TRUE)/sum(as.double(Production_2016$pnette),na.rm = TRUE)

summary(Ta)
## Epuration et creation des codes Clients. 
## Construire la structure des codes clients.


#Code_police_travaille= gsub(RegPron,"",Production_2016$ancienpol)
Code_police_init = Production_2016$ancienpol

#Codification_Pol =Codification(Code_police_init, bPolice = TRUE)
Code_police_travaille =  gsub("-|\\.|\\s|/","",Code_police_init)


# Verification format 

summary(nchar(Production_2016$ancienpol))

summary(nchar(Code_police_travaille))

Vec_CodePolAnalyse=  nchar(Code_police_travaille)==9
Vec_CodePolAnalyse2 = grepl(pattern = "^[A-Za-z]{3}[0-9]{6,6}$",Code_police_travaille)


### Lister certaines polices invalides 
summary(Vec_CodePolAnalyse & (!Vec_CodePolAnalyse2))
Code_police_init[Vec_CodePolAnalyse & (!Vec_CodePolAnalyse2)]
Code_police_travaille[Vec_CodePolAnalyse & (!Vec_CodePolAnalyse2)]

Production_2016$Code_police_travaille = Code_police_travaille
## Liste des polices a corriger
Liste_Police_a_corriger =   unique(Code_police_travaille[!Vec_CodePolAnalyse2])
Base_Police_A_corriger = Production_2016[!Vec_CodePolAnalyse2 ,]
Liste_Police_corriger = unique(Code_police_travaille[Vec_CodePolAnalyse2])


table(Base_Police_A_corriger$intermediaire)
### Extraire la  liste des polices numerique de 6 qui pourra completer les autres.

Vec_Police_6_chiffres = grepl(pattern = "^[0-9]{6,6}$",Code_police_travaille)

Liste_Pol_Index_Garantie = str_sub(Liste_Police_corriger,1,3 )
Liste_Pol_numeric =  str_sub(Liste_Police_corriger,4,9 )


### Liste 002 correspond a la liste des polices qui ont 6 chiffres mais qui sont inclus dans la liste des polices correctes
Base_Police_A_corriger = Base_Police_A_corriger[!Base_Police_A_corriger$Code_police_travaille %in% Liste_Pol_numeric,] 


## Construction de la table de police ###
Table_police_coeur = cbind(Liste_Police_corriger,Liste_Pol_Index_Garantie,Liste_Pol_numeric)
Table_branche$Libelle_Branche=gsub("  "," ",Table_branche$Libelle_Branche)
Prod_police=  merge(Production_2016, Table_branche, by.x ="branche",by.y="Libelle_Branche")
# Construction  des polices hors transports 

Table_police_hs_Transport = merge(Prod_police[,c("Code_police_travaille","police", "Code_branche", "Code_client","date_effet","fin_garantie")], Table_police_coeur, by.x ="Code_police_travaille", by.y="Liste_Police_corriger")

colnames(Table_police_hs_Transport)
length(unique(Table_police_hs_Transport$Code_police_travaille))

Table_Police_Unique = Table_police_hs_Transport[!duplicated(Table_police_hs_Transport$Code_police_travaille),c("Code_police_travaille", "Liste_Pol_Index_Garantie", "Code_branche" ,"Liste_Pol_numeric" )]
colnames(Table_Police_Unique) = c("Ancien_Police", "Code_Produit", "Code_branche","numero_police") 

write.table(Table_Police_Unique,paste0(Chemin,"Table_bd/tbPolice.csv"), sep =";", dec = ".")
dbWriteTable(mydb, "Police", Table_Police_Unique,append=TRUE, row.names=FALSE, overwrite=FALSE)


### Table intermediaire
  

### Importation production

Table_production = Production_2016[,c("Code_client",)]
colnames(Table_production)

Production_2016$intermediaire[Resultat_interm$Liste_cas_position[[2]]]

unique(Codification( unique(Table_interm$typeinter))$Code)

rm(objects(touslesobjets[!grepl( pattern= "Table|Production",touslesobjets)]))

