get.music.info<-function(musicid){
  u=paste0("https://api.douban.com/v2/music/",musicid)
  #p=getURL(u,ssl.verifypeer = FALSE)
  p<-.refreshURL(u,ssl.verifypeer = FALSE)
  reslist <- fromJSON(p)
  title<-reslist[["title"]]
  author<-unlist(reslist[["author"]]);names(author)<-NULL
  rating<-unlist(reslist[["rating"]])
  summary<-reslist[["summary"]]
  tags<-reslist[["tags"]]
  
  tags<-data.frame(tag_label=sapply(tags,function(x)x[["name"]]),
                   tag_freq=sapply(tags,function(x)x[["count"]]),
                   stringsAsFactors=F)
  image<-reslist[['image']]
  href<-reslist[["alt"]]
  reslist$title<-NULL;reslist$author<-NULL;
  reslist$rating<-NULL;reslist$tags<-NULL;
  reslist$summary<-NULL;reslist$alt<-NULL
  reslist$image<-NULL
  attribute=reslist$attrs
  songs<-t(sapply(attribute$songs,
                  function(x)c(x[["index"]],x[["title"]],x[["name"]])))
  attribute$songs<-NULL
  colnames(songs)<-c("index","title","name")
  list(title=title,
       author=author,
       rating=as.double(rating),
       summary=summary,
       tags=tags,
       songs=as.data.frame(songs,stringsAsFactors=F),
       href=href,
       image=image,
       attribute= attribute)
}
