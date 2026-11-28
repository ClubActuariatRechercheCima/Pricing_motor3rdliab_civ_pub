
ExtractionAnnee<-function(ChaineDate)
{
  as.numeric(format(as.Date(ChaineDate, "%d/%m/%Y"),"%Y"))
  
}

DureePolice<-function(DateEffet, DateFin)
{
  ## Cette fonction compre 1 pour une emissions sur 1 an, et 1/12 pour une police mensuelle
  as.numeric(as.Date(DateFin, "%d/%m/%Y")- as.Date(DateEffet, "%d/%m/%Y")+1) 
  
}
ConversionDate<-function(DateNum)
{
  as.Date(as.character(DateNum),"%Y%m%d")
  
}
Exposition<-function(DateEffet, DateFin,BaseAnnuelle)
{
  ## Base_annuelle correpond au nombre de jours de référence dans une année
  ## Cette fonction compre 1 pour une emissions sur 1 an, et 1/12 pour une police mensuelle
  as.numeric(as.Date(DateFin, "%d/%m/%Y")- as.Date(DateEffet, "%d/%m/%Y")+1)/365.25  
  
}

AnneePoliceAcquisition<-function(DateEffet,DateFin,Annee)
{
  # Renvoie l'ann�e police pour une annee donnees
  
  DebutAnnee = as.Date(paste("01/01/",Annee,sep = ""), "%d/%m/%Y")
  FinAnnee= as.Date(paste("31/12/",Annee,sep = ""), "%d/%m/%Y")
  
  DateEffet =as.Date(DateEffet, "%d/%m/%Y")
  DateFin =as.Date(DateFin, "%d/%m/%Y")
  
  AnneePoliceAcquisition=(DateEffet<FinAnnee & DateFin>DebutAnnee)*((sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})+   sapply(as.numeric(DateEffet-DebutAnnee),FUN=function(x){min(x,0)}) + sapply(as.numeric(DateFin-FinAnnee),FUN=function(x){min(x,0)})+1)/365.25)
  
  
  #  sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})
  
  # min(as.numeric(DateEffet-DebutAnnee),0)
}

SumDouble <-function(X,na.rm)
{
  sum(as.double(X),na.rm = TRUE)
}

SumDoubleAcquisition <-function(X,Acquisition)
{
  sum(as.double(X*Acquisition),na.rm = TRUE)
}

RatioSP<-function(X)
{
  sum(X[,1],na.rm=TRUE)/sum(X[,2],na.rm=TRUE)
}

ratioSP<-function(x,y)
{
  sum(x,na.rm=TRUE)/sum(y,na.rm=TRUE)
}
AnalyseGraphique<-function(list_Variable, Variable1, RatioSup, RatioInf)
{
  
  
  # Thu Dec 08 11:59:31 2016 ------------------------------
  Table_aggregate<-aggregate(Sinistre_Analyse[,Vec_Variable], list(Surv=Sinistre_Analyse$Annee_surv,Segment=Sinistre_Analyse$Segment,Canal=Sinistre_Analyse$Canal,Branche=Sinistre_Analyse$branche),  FUN = SumDouble, na.rm = TRUE)
  
  
}

Retraitement_police<-function(VecPolice)
{
  
  FormatPattern = "-|\\.|\\s|/"
  return(gsub(FormatPattern, "",VecPolice))
  
  
}

AnneePoliceProvisionPREC<-function(DateEffet,DateFin,DebutPrec)
{
  # Renvoie l'année police pour une annee donnees
  
  #DebutAnnee = as.Date(paste("01/01/",Annee,sep = ""), "%d/%m/%Y")
  #FinAnnee= as.Date(paste("31/12/",Annee,sep = ""), "%d/%m/%Y")
  
  DebutPrec = as.Date(DebutPrec, "%d/%m/%Y")
  #  FinAnnee= as.Date(FinAnnee, "%d/%m/%Y")
  
  DateEffet =as.Date(DateEffet, "%d/%m/%Y")
  DateFin =as.Date(DateFin, "%d/%m/%Y")
  
  # AnneePoliceAcquisition=(DateFin>DebutPrec)*((sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})+   sapply(as.numeric(DateEffet-DebutAnnee),FUN=function(x){min(x,0)}) + sapply(as.numeric(DateFin-FinAnnee),FUN=function(x){min(x,0)})+1)/365.25)
  
  AnneePoliceProvision = (DateFin>DebutPrec)*((sapply(as.numeric(DateFin- DebutPrec),FUN=function(x){max(x,0)})+   sapply(as.numeric(DebutPrec-DateEffet),FUN=function(x){min(x,0)}) )/365.25)
  
  
  #  sapply(as.numeric(FinAnnee-DateEffet),FUN=function(x){max(x,0)})
  
  # min(as.numeric(DateEffet-DebutAnnee),0)
}


Codification <- function(Libelle, bPolice=FALSE )
{
  #bPolice = TRUE indique que la recodification est reaisee pour les polices
  
  Resultat = list()
  ## Cette fonction retraite le texte initial avnat de recodifier
  
  if (bPolice==TRUE)
  {
    
    RegPron = "-|\\.|\\s|/" 
  }else
  {
    RegPron = "Le|La|L|l|le|la|les|-|\\.|\\s|/"
  }
  Resultat$Lib_init = Libelle
  
  
  Libelle= gsub(RegPron,"",Libelle)
  Resultat$Code  =abbreviate(Libelle,method="both.sides" ,minlength = 6)
  
  
  
  Resultat$Lib_travaille =Libelle
  return(Resultat)
}

Choisir_Version_Texte<-function(Liste_texte)
{
  return(Liste_texte[1])
  
}


Comparaison_vecteur <-function(Vecteur_Init, Vecteur_travaille)
{
  ## Cette fonction prend en input une liste de libelle ou de codee et une liste de libelle ou de code travaillee
  ##  elle construi pour chaque code travaille, la liste des elemnts initiaux qui ont ce meme code travaille comme outpuot
  ### enfin elle permet pour chacune des vecteurs initiaux de choisir la version qui sera retenue comme correspondant du code
  Res=list()
  
  
  Vec_Recodifie = Vecteur_Init
  if (length(unique(Vecteur_travaille))>length(unique(Vecteur_Init)))
  {
    Res$Debug = "Revoir Vecteurs d entrees"
    existsFunction()
    
    
  }else
  {
    
    ## identifie tous les cas ou un elt est en doublon dans vecteur_travaille mais n est 
    ## pas en doublon dans Vecteur_init
    cas_suivi = duplicated(Vecteur_travaille)& (!duplicated(Vecteur_Init))
    
    
    Liste_cas = list()
    Liste_cas_position = list()
    for (i in 1:sum(cas_suivi))
    {
      
      cas=which(cas_suivi)[i]
      
      Liste_cas[[i]]=Vecteur_Init[(Vecteur_travaille == Vecteur_travaille[cas])]
      Liste_cas_position[[i]]=which(Vecteur_travaille == Vecteur_travaille[cas])
      
      Vec_Recodifie[ c(Liste_cas_position[[i]])] = Choisir_Version_Texte(Liste_cas[[i]])
    }
    
    Res$Liste_cas <- Liste_cas
    Res$Liste_cas_position = Liste_cas_position
    Res$Vec_Recodifie=Vec_Recodifie
    
  }
  
  Res$Liste_cas_unique= lapply(Liste_cas, FUN=unique)
  Res$cas_suivi=cas_suivi
  return(Res)
}


Fiabiliser_police <- function(Vec_police_initial)
{
  
  ### Ajouter du code pour controle les prefixes de branche.
  Res = list()
  Vec_police_travaille =  gsub("-|\\.|\\s|/","",Vec_police_initial)
  
  Vec_CodePolOK = grepl(pattern = "^[A-Za-z]{3}[0-9]{6,6}$",Vec_police_travaille)
  
  Liste_Police_a_corriger =   unique(Vec_police_travaille[!Vec_CodePolOK])
  Liste_Police_OK = unique(Vec_police_travaille[Vec_CodePolOK])
  
  Liste_Pol_Index_Garantie = str_sub(Liste_Police_OK,1,3 )
  Liste_Pol_numeric =  str_sub(Liste_Police_OK,4,9 )
  
  Table_police_coeur = cbind(Liste_Police_OK,Liste_Pol_Index_Garantie,Liste_Pol_numeric)
  
  Res$Vec_police_travaille= Vec_police_travaille  
  Res$Vec_CodePolOK=Vec_CodePolOK
  Res$Liste_Police_a_corriger= Liste_Police_a_corriger
  Res$Liste_Police_OK=Liste_Police_OK
  Res$Table_police =Table_police_coeur
  Res$Liste_Pol_Index_Garantie = Liste_Pol_Index_Garantie
  Res$Liste_Pol_numeric=Liste_Pol_numeric
  
  return(Res)
  
}
