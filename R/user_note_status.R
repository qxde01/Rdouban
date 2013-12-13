##################
#' @param u:日记地址
#'
.get_user_note<-function(u){
  p<-.refreshURL(u)
  title<-sapply(getNodeSet(p, '//div[@class="note-header"]//h1'),xmlValue)
  published<-sapply(getNodeSet(p, '//div[@class="note-header"]//span'),xmlValue)
  liked<-sapply(getNodeSet(p, '//span[@class="fav-num"]//a'),xmlValue)
  liked<-gsub('[^0-9]','',liked)
  recommend<-sapply(getNodeSet(p, '//span[@class="rec-num"]'),xmlValue)
  recommend<-gsub('[^0-9]','',recommend)
  if(length(recommend)==0)recommend<-NA
  if(length(liked)==0)liked<-NA
  note<-sapply(getNodeSet(p, '//div[@class="note"]'),xmlValue)
  note<-note[nchar(note)>0]
  out<-c(title,published,note,liked,recommend)
  return(out)
}
#############################################################
## 获取用户的日记
#'@param userid
#'@param count
#'@param verbose

user_note_status<-function(userid,count=10,verbose=TRUE){
  u<-paste0("http://www.douban.com/people/",userid,"/notes")
  p<-.refreshURL(u)
  pages<-sapply(getNodeSet(p, '//body//div[@class="paginator"]//a'),xmlValue)
  if(length(pages)==0){
    pages=1
  }
  else{
    pages<-as.integer(pages[length(pages)-1])
  }
  cat("\n--------There is a total of ",pages," pages about notes.--------\n")
  href<-c()
  for(pg in 1:pages){
    u = paste0("http://www.douban.com/people/", userid, 
               "/notes?start=", (pg - 1) * count)
    cat("Getting note URLs from page ", pg, ": \n  ", u, " ...\n")
    p<-.refreshURL(u)
    href0<-sapply(getNodeSet(p, '//body//div//span[@class="wrap"]//a'),function(x) xmlGetAttr(x, "href"))
    href0<-unique(href0[grep('/note/',href0)])
    href<-c(href,href0)
  }
  total<-length(href)
  cat("\n--------There is a total of ",total," notes.--------\n")
  df<-data.frame(matrix(nrow=total,ncol=5),stringsAsFactors=F)
  colnames(df)<-c("title","published","note","liked","recommend")
  if(total>0){
    for(i in 1:total){
      u=href[i]
      if(verbose==TRUE){cat("  Getting note from URL: ",i,',', u, " ...\n")}
      df[i,]<-.get_user_note(u)  
    }
  }
  return(df)
}


