get_online_tags<-function(results=1000,count=200){
  pages<-ceiling(results/count)
  tags<-c()
  for(pg in 1:pages){
    u=paste0("http://www.douban.com/online/tag/?start=",(pg-1)*count)
    cat(pg,", Getting tags from u:",u,"......\n")
    p=.refreshURL(u)
    n1<-getNodeSet(p,'//div[@class="article"]//div[@class="indent"]//span')
    tag<-sapply(n1,xmlValue)
    tag<-gsub("[\n ]|　　","",tag)
    #cat(tag,"\n")
    tag<-unlist(strsplit(tag,"\\(|\\)"))
    word<-tag[seq(1,length(tag),2)]
    freq<-tag[seq(1,length(tag),2)+1]
    url=sapply(getNodeSet(p,'//div[@class="article"]//div[@class="indent"]//a'),
               function(x) xmlGetAttr(x, "href"))
    #cat(length(word),"  ",length(freq),"\n")   
    tags0<-cbind(tag=word,url=url,freq=freq)
    tags<-rbind(tags,tags0)
  }
  row.names(tags)<-NULL
  return(as.data.frame(tags,stringsAsFactors=F))
}