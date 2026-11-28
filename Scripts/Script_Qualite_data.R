#Chemin_source="<CHEMIN_LOCAL>/"
#Chemin_source="<CHEMIN_LOCAL>/"
Chemin_source=paste0(Chemin_initial,"Collecte donnees/")

#####LECTURE DE TOUTES LES TABLES DE COLLECTE DE DONNEES ET RETRAITEMENT DES NUMERO DE POLICE

Fichiers=list.files(paste0(Chemin_source,"BD_Collecte/"))
n=length(Fichiers)
data=list()
for(i in 1:n)
{
  data[[i]]=read.csv2(paste(paste0(Chemin_source,"BD_Collecte/"),Fichiers[i],sep=""),sep=";")
  data[[i]]$Anc_Police=Retraitement_police(data[[i]]$Anc_Police)
  data[[i]]$idunique=paste(data[[i]]$Anc_Police,data[[i]]$Effet,data[[i]]$Code_garantie,sep="_")
  data[[i]]$Name=Fichiers[i]
  }

######## REGROUPEMENT DES TABLES DE COLLECTE DE DONNEES
res=data[[1]]
for(i in 2:n)
{
  Ajout=data[[i]][!(data[[i]]$idunique %in% res$idunique),]
  ##Doublons=data[[2]][data[[2]]$idunique %in% res$idunique,]
  res=rbind(res,Ajout)
}
#colnames(res)

#res=res[res$Prime!=0,]
write.table(res[,c(1:8,10)],paste(Chemin_source,"COLLECTE_FINALE_AGGREGEE.csv",sep=""),sep=";",dec=",",row.names=FALSE)
#write.table(res[,c(1:8)],paste(Chemin_source,"COLLECTE_FINALE_AGGREGEE.csv",sep=""),sep=";",dec=",",row.names=FALSE)

