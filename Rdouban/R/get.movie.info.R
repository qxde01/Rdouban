get.movie.info<-function(movieid){
  u=paste0("https://api.douban.com/v2/movie/",movieid)
  #p=getURL(u,ssl.verifypeer = FALSE)
  p<-.refreshURL(u,ssl.verifypeer = FALSE)
  reslist <- fromJSON(p)
  title<-reslist[["title"]]
  author<-unlist(reslist[["author"]]);names(author)<-NULL
  rating<-unlist(reslist[["rating"]])
  tags<-reslist[["tags"]]
  summary<-reslist[["summary"]]
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
  
  list(title=title,
       author=author,
       rating=as.double(rating),
       summary=summary,
       tags=tags,
       href=href,
       image=image,
       attribute= attribute)
}