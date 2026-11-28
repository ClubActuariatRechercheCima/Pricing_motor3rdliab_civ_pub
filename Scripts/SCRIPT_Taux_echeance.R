install.packages("stringr", dependencies=TRUE)


#chemin_base= "<CHEMIN_LOCAL>/"
#chemin_base= "<CHEMIN_LOCAL>/"





write.table(BaseFusionPrime,file = paste0(Chemin_resultat,"BaseFusionPrimebon.csv"),sep=";",dec=",", row.names = FALSE)


#BaseFusionPrime$IdUnique= as.numeric(row.names(BaseFusionPrime))


### Cloisement des listes de clients et de leur différents types dans la BaseFusion ###


#BasePropre$Identifiant=paste0(BasePropre$police,as.numeric(BasePropre$date_effet),as.numeric(BasePropre$fin_garantie))
BasePropre$Identifiant_avenant=paste0(BasePropre$ancienpol,as.numeric(BasePropre$fin_garantie))

#summary(duplicated(BasePropre$Identifiant))
summary(duplicated(BasePropre$Identifiant_avenant))

###Recoder variable d'une table#
  
#### Création de la base des données des identifiants_court unique avec chacun leur date d'effet##
by_variable = c("exercice","branche"  , "typeinter", "codepartenaire","Identifiant_avenant","fin_garantie","client","intermediaire","codeinterm","ancienpol")
by_variable_sans_ex = c("branche"  , "typeinter", "codepartenaire","Identifiant_avenant","fin_garantie","client","intermediaire","codeinterm","ancienpol")

Vec_Variable = c("date_effet" )

Base_Police_Avenant= aggregate(BasePropre[,Vec_Variable],by=BasePropre[,by_variable], FUN = min, na.rm=TRUE)
Base_Police_Avenant_sans_ex= aggregate(BasePropre[,Vec_Variable],by=BasePropre[,by_variable_sans_ex], FUN = min, na.rm=TRUE)
colnames(Base_Police_Avenant)[11]="date_effet"
colnames(Base_Police_Avenant_sans_ex)[10]="date_effet"

#summary(duplicated(BasePropre$Identifiant_avenant))
#summary(duplicated(Base_Police_Avenant$Identifiant_avenant))


Base_Police_Avenant$Duree=DureePolice(Base_Police_Avenant$date_effet,Base_Police_Avenant$fin_garantie)
Base_Police_Avenant$AnneePolice=Exposition(Base_Police_Avenant$date_effet,Base_Police_Avenant$fin_garantie)


Base_Police_Avenant_sans_ex$Duree=DureePolice(Base_Police_Avenant_sans_ex$date_effet,Base_Police_Avenant_sans_ex$fin_garantie)
Base_Police_Avenant_sans_ex$AnneePolice=Exposition(Base_Police_Avenant_sans_ex$date_effet,Base_Police_Avenant_sans_ex$fin_garantie)

####
Vec_Variable = c("pnette","coass","taxe", "cp" )
#by_variable = c("branche" , "CodeIntermediaire", "codepartenaire","Identifiant","date_effet","fin_garantie","client","police")
BasePropre_Avenant <-aggregate(BasePropre[,Vec_Variable],by=BasePropre[,by_variable], FUN = SumDouble, na.rm=TRUE)
BasePropre_Avenant_sans_ex <-aggregate(BasePropre[,Vec_Variable],by=BasePropre[,by_variable_sans_ex], FUN = SumDouble, na.rm=TRUE)


###### Fusionner indicateur chiffrees et Date d'effet ###

BaseFin= merge(BasePropre_Avenant,Base_Police_Avenant,by=by_variable,all.x = TRUE,all.y = FALSE)   
BaseFin_sans_ex=merge(BasePropre_Avenant_sans_ex,Base_Police_Avenant_sans_ex,by=by_variable_sans_ex,all.x = TRUE,all.y = FALSE)   

## comprendre ###
Liste_doublon=Base_Police_Avenant$Identifiant_avenant[duplicated(Base_Police_Avenant$Identifiant_avenant)]
BaseDoublon_analyse=BaseFin[BaseFin$Identifiant_avenant%in% Liste_doublon,]


Liste_doublon_sans_ex=Base_Police_Avenant_sans_ex$Identifiant_avenant[duplicated(Base_Police_Avenant_sans_ex$Identifiant_avenant)]
BaseDoublon_analyse_sans_ex=BaseFin_sans_ex[BaseFin_sans_ex$Identifiant_avenant%in% Liste_doublon_sans_ex,]

###### Enregistrements #####
write.table(BaseDoublon_analyse_sans_ex,file = paste0(Chemin_resultat,"BaseDoublon_analyse_sans_ex.csv"),sep=";",dec=",", row.names = FALSE)
write.table(BaseDoublon_analyse,file = paste0(Chemin_resultat,"BaseDoublon_analyse.csv"),sep=";",dec=",", row.names = FALSE)
write.table(BaseFin,file = paste0(Chemin_resultat,"BaseFin.csv"),sep=";",dec=",", row.names = FALSE)
write.table(BaseFin_sans_ex,file = paste0(Chemin_resultat,"BaseFin_sans_ex.csv"),sep=";",dec=",", row.names = FALSE)

#Conserv_1=BaseFin
#Conserv_2= BaseFin_sans_ex
BaseFin_sans_ex=Conserv_2
#######Savoir si la police a été renouvelée et si oui mettre la durée entre ############
#### Pour chaque police 
  #### Indiquer si c'est une an ou un renouvellement par rapport à lapériode d'effet.
length(unique(BaseFin_sans_ex$ancienpol))
BaseFin_sans_ex$AN=TRUE
BaseFin_sans_ex$Resilie=FALSE
BaseFin_sans_ex$Renouveller=FALSE

BaseFin_sans_ex$Identifiant_avenant[runif(1,1,180925)]

Liste_Police= unique(BaseFin_sans_ex$ancienpol)
length(Liste_Police)

# T1<-Sys.time()
# for (i in 1:100)
# {
#   test=Liste_Police[i]
#   Btest=BaseFin_sans_ex[(BaseFin_sans_ex$ancienpol==test),]
#   ## AN
#   BaseFin_sans_ex[BaseFin_sans_ex$ancienpol==test,]$AN[Btest$date_effet==min(Btest$date_effet)]=TRUE
#   ### Renouvellement
#   BaseFin_sans_ex[BaseFin_sans_ex$ancienpol==test,]$Renouveller[Btest$fin_garantie==max(Btest$fin_garantie)]=FALSE
#   
#   
# }
# 
# T2<-Sys.time()
# Tdiff= difftime(T2, T1)

T1<-Sys.time()
for (i in 1:length(Liste_Police))
{
  test=Liste_Police[i]
  selection=(BaseFin_sans_ex$ancienpol==test)
  if (sum(selection)>1)
  {
   # BaseFin_sans_ex[selection,]$AN=TRUE
    #BaseFin_sans_ex[selection,]$Renouveller=FALSE
#  }else
 # {
    
    Btest=BaseFin_sans_ex[selection,]
    MinE=min(Btest$date_effet)
    MaxF=max(Btest$fin_garantie)
    ## AN
    #BaseFin_sans_ex[selection,]$AN[Btest$date_effet!=MinE]=FALSE
    BaseFin_sans_ex[selection,"AN"][Btest$date_effet!=MinE]=FALSE
    
     ### Renouvellement
   # BaseFin_sans_ex[selection,]$Renouveller[Btest$fin_garantie!=MaxF]=TRUE
    BaseFin_sans_ex[selection,"Renouveller"][Btest$fin_garantie!=MaxF]=TRUE
    
  }
  
   #BaseFin_sans_ex[BaseFin_sans_ex$ancienpol==test,]$Renouveller[Btest$fin_garantie==max(Btest$fin_garantie)]=FALSE
  
  
}

T2<-Sys.time()
Tdiff= difftime(T2, T1)


###Rappatrier les statuts dans Base_fin

Base_fin_test = merge(BaseFin,BaseFin_sans_ex[,c(by_variable_sans_ex,"AN","Renouveller" )],by=by_variable_sans_ex,all.x = TRUE,all.y = FALSE)   


Base_fin_test$AnneeEffet=format(Base_fin_test$date_effet,"%Y")
Base_fin_test$MoisEffet=months.Date(Base_fin_test$date_effet)


Base_fin_test$AnneeRen=format(Base_fin_test$fin_garantie+1,"%Y")
Base_fin_test$MoisRen=months.Date(Base_fin_test$fin_garantie+1)
Base_fin_test$Unite=1

write.table(Base_fin_test,file = paste0(Chemin_resultat,"Resultat_total.csv"),sep=";",dec=",", row.names = FALSE)

#####





by_variable_agg = c("exercice","branche"  , "typeinter", "codepartenaire","AnneeEffet","MoisEffet","AnneeRen","MoisRen")
by_variable_sans_ex = c("branche"  , "typeinter", "codepartenaire","Identifiant_avenant","fin_garantie","client","intermediaire","codeinterm","ancienpol")

Vec_Variable = c("pnette","cp","Duree","AnneePolice","Unite" )


Base_fin_test_fiable=Base_fin_test[Base_fin_test$exercice!=2010 & Base_fin_test$exercice!=2018,]
Base_fin_test_fiable$AN=as.numeric(Base_fin_test_fiable$AN)
Base_fin_test_fiable$Renouveller=as.numeric(Base_fin_test_fiable$Renouveller)

Res_Renouvellement =glm(Renouveller~branche+typeinter+codepartenaire+AnneeRen+MoisRen ,family =binomial(link = "logit") ,data=Base_fin_test_fiable[Base_fin_test_fiable$typeinter!="autres",])

Res_Duree =glm(Duree~branche+typeinter+codepartenaire+MoisRen+AN ,family =gaussian() ,data=Base_fin_test_fiable[Base_fin_test_fiable$typeinter!="autres",])

Res_AN = glm(AN~branche+typeinter+codepartenaire+AnneeEffet+MoisEffet ,family =binomial(link = "logit") ,data=Base_fin_test_fiable[Base_fin_test_fiable$typeinter!="autres",])


Vec_Variable = c("pnette","prime","compteur", "boolRenouveller" )
by_variable = c("branche" , "CodeIntermediaire","Mois")


Resultat_Renouvellement <-aggregate(BaseTravail[,Vec_Variable],by=BaseTravail[,by_variable], FUN = SumDouble, na.rm=TRUE)
Resultat_Renouvellement$taux_ref=Resultat_Renouvellement$boolRenouveller/Resultat_Renouvellement$compteur
res=glm(taux_ref~branche+CodeIntermediaire+codepartenaire+Annee+Mois ,family =binomial(link = "logit") ,data=Resultat_Renouvellement)
Resultat_R=aggregate(BaseTravail[,Vec_Variable],by=BaseTravail[,by_variable], FUN = SumDouble, na.rm=TRUE)
res2=glm(boolRenouveller~branche+CodeIntermediaire+codepartenaire+Annee+Mois ,family =binomial(link = "logit") ,data=BaseTravail[BaseTravail$codepartenaire!="autres",])





summary(Resultat_Renouvellement)

write.table(BaseTravail,file = paste0(Chemin_resultat,"Base_taux_echeance.csv"),sep=";",dec=",", row.names = FALSE)
write.table(BaseFusionPrime,file="output.csv", sep = ',', row.names = FALSE, append = T, fileEncoding = 'UTF-8')
sum(BaseFusionPrime$RefRenouvellement)
BaseFusion=BaseFusionPrime
BaseFusionPrime$RefRenouvellement=as.numeric(BaseFusionPrime$RefRenouvellement)
BaseFusionPrime$boolRenouveller=as.character(BaseFusionPrime$boolRenouveller)
BaseFusionPrime$res=as.character(BaseFusionPrime$res)
BaseFusionPrime$prime=as.numeric(BaseFusionPrime$prime)


##### 
table(nchar(BaseFusionPrime$ancienpol))




###### Base Echeance 2018 non emis ######

BaseEcheance2018 = BaseTravail[BaseTravail$Annee2==2018,]
write.table(BaseEcheance2018,file = paste0(Chemin_resultat,"Base_Ecehance2018.csv"),sep=";",dec=",", row.names = FALSE)




############## traitement Base Agreg #####
length(unique(BaseAggreg$Identifiant))

tt=duplicated(BaseAggreg$Identifiant)
summary(tt)

View(BaseAggreg[tt,])

View(t(as.data.frame(TT)))


Existence_renouvellement_2(141813)
Existence_renouvellement_2(14511)

Existence_renouvellement_2(32896)

View(BaseFusionPrime[testt,])

testt=BaseFusionPrime$police== "<NUMERO_POLICE>"
sum(testt)


Tdiff= difftime(T2, T1)
Extrait=BaseFusionPrime[1:30000,]


Existence_renouvellement_2(15268)

Existence_renouvellement_2(152)
