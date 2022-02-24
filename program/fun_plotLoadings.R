plotLoadings <- function(O2PLS_fit=fit,
                         LoadingPC=1,
                         nTopLoadings=5,
                         nBottomLoadings=5,
                         typeL="Joint",
                         justReturnLoadings=F){
  Loadings=list()
  print("Loadings Till here?1")
  if(typeL=="Joint"){
    Loadings[[1]]=as.data.frame(O2PLS_fit$W.)
    Loadings[[2]]=as.data.frame(O2PLS_fit$C.)
    ## 2 Plots will be returned
  }else{
    if(typeL=="RNA"){
      Loadings[[1]] = as.data.frame(O2PLS_fit$P_Yosc.) #RNA
    }
    
    if(typeL=="Lipid"){
      Loadings[[1]] = as.data.frame(O2PLS_fit$P_Xosc.) #Lipid
    }
  }
  if(justReturnLoadings==TRUE){
    return(Loadings)
  }
  print("Loadings Till here?2")
  plotOutputList=lapply(Loadings,function(listElem){
    listElem=as.data.frame(listElem)
    NoTotalCmp=ncol(listElem)
    origData=listElem
    colnames(listElem)=1:ncol(listElem)
    listElem$Name=rownames(listElem)
    IDX_toPlot=order(listElem[,LoadingPC],decreasing = T)
    listElem$Name=factor(listElem$Name,levels = listElem$Name[IDX_toPlot])
    listElem=listElem[IDX_toPlot[c(1:nTopLoadings,(nrow(listElem)-(nBottomLoadings-1)):nrow(listElem))],]
    if(any(grepl("ENSMUSG",listElem$Name))){
      # replace ID names with real Gene Names 
      # Potentially TOGGLE
      listElem$Symbol="NA"
      for(i in listElem$Name){
        idx_match=which(grepl(i,savedData$RNA$annotation_rows$RNA$GENEID))
        listElem[i,"Symbol"]=savedData$RNA$annotation_rows$RNA$SYMBOL[idx_match]
      }
      # order after size
      IDX_toPlot=order(listElem[,LoadingPC],decreasing = F)
      listElem$Symbol=factor(listElem$Symbol,levels = listElem$Symbol[IDX_toPlot])
    }
    
    # Color according to # occurence in top L loadings
    #Prep Orig data
    for(i in 1:NoTotalCmp){
      idxOrder=order(origData[,i],decreasing = T)
      origData[idxOrder,paste0("Order_",i)]=1:nrow(origData)
      origData[,paste0("inSel_",i)]=origData[,paste0("Order_",i)]%in%c(1:nTopLoadings,(nrow(origData)-(nBottomLoadings-1)):nrow(origData))
    }
    listElem$TotalSelected=0
    for(i in 1:nrow(listElem)){
      #identify for each selected gene it's position among all other cmp
      inSelCols=grepl("inSel",colnames(origData))
      No_inSelection=origData[as.character(listElem$Name[i]),inSelCols]
      listElem[i,"TotalSelected"]=length(which(t(No_inSelection)[,1]==T))
    }
    if("Symbol"%in%colnames(listElem)){
      p1=ggplot(listElem,aes(y=Symbol,x=listElem[,LoadingPC]))
    }else{
      p1=ggplot(listElem,aes(y=Name,x=listElem[,LoadingPC]))
    }
    plotOut=p1+
      geom_col(aes(fill=TotalSelected),col="black")+
      scale_fill_gradient(low="#277d6a",high="grey")+
      ylab("")+
      xlab(paste0("Loadings PC ",LoadingPC))+
      theme_bw(base_size = 20)
    plotOut
  })
  
  
  
  plotOutputList
}