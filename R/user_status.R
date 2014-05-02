.statuses<-function(p){
  .extract<-function(x){
    a<-unlist(xmlToList(x))
    created_at<-a['div.div.div.span..attrs.title']
    saying<-a['div.div.blockquote.p']
    saying<-ifelse(is.na(saying),a['div.div.div.blockquote.p'],saying)
    reply<-gsub('[^0-9]','',a[grep('回应',a)][1])
    out<-c(created_at,saying,reply)
    names(out)<-c('created_at','saying','reply')
    return(out)   
  }
  
  n1<-getNodeSet(p,'//div[@class="status-item"]')
  data_sid<-sapply(n1,function(x) xmlGetAttr(x, "data-sid"))
  data_target_type<-sapply(n1,function(x) xmlGetAttr(x, "data-target-type"))
  data_object_kind<-sapply(n1,function(x) xmlGetAttr(x, "data-object-kind"))
  
  n2<-getNodeSet(p,'//div[@class="status-item"]//div[@class="text"]')
  txt<-gsub('\n|   ','',sapply(n2,xmlValue))
  n3<-getNodeSet(p,'//div[@class="status-item"]//div[@class="block block-subject"]')
  
  txt<-gsub('\n|   |> 我来回应','',sapply(n1,xmlValue))
  txt<-gsub('          ','',txt)
  tmp0<-t(sapply(n1,.extract))
  tmp<-cbind(data_sid,data_object_kind,data_target_type,txt,tmp0)
  tmp<-as.data.frame(tmp,stringsAsFactors=F)
  return(tmp)
}
################################
user_status<-function(userid,results=200){
  pages<-results/20
  df<-c()
  for(pg in 1:pages){
    u<-paste0('http://www.douban.com/people/',userid,'/statuses?p=',pg)
    cat(pg,' Getting saying from:',u,'...\n')
    p<-.refreshURL(u)
    if(!is.null(p)){
      df0<-.statuses(p)
      df<-rbind(df,df0)
    }
  }
  return(df)
}
##qxde<-user_status(userid='qxde01',results=200)
