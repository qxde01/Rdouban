##douban api of book information
##@bookid
##@version
get.book.info<-function(bookid,version=2){
  if(version==1){
    u<-paste0("http://api.douban.com/book/subject/",bookid,"?alt=json")
    p=getURL(u)
    reslist <- fromJSON(p)
    title<-reslist[["title"]][[1]]
    author<-unlist(reslist[["author"]]);names(author)<-NULL
    rating<-unlist(reslist[["gd:rating"]])
    summary<-reslist[["summary"]];names(summary)<-NULL
    tags=reslist[["db:tag"]]
    tags<-data.frame(tag_label=sapply(tags,function(x)x[["@name"]]),
                    tag_freq=sapply(tags,function(x)x[["@count"]]),
                    stringsAsFactors=F)
    attribute<-t(sapply(reslist[["db:attribute"]],function(x)c(x[2],x[1])))
    attribute<-as.data.frame(attribute,stringsAsFactors=F)
    link<-sapply(reslist[["link"]],function(x)x[[2]])
    href<-link[2];image<-link[3]
  }
if(version==2){
  u=paste0("https://api.douban.com/v2/book/",bookid)
  p=getURL(u,ssl.verifypeer = FALSE)
  reslist <- fromJSON(p)
  title<-reslist[["title"]]
  author<-reslist[["author"]]
  rating<-unlist(reslist[["rating"]])
  tags<-reslist[["tags"]]
  summary<-reslist[["summary"]]
  tags<-data.frame(tag_label=sapply(tags,function(x)x[["name"]]),
                   tag_freq=sapply(tags,function(x)x[["count"]]),
                   stringsAsFactors=F)
  image<-reslist[['images']]
  href<-reslist[["alt"]]
  reslist$title<-NULL;reslist$author<-NULL;
  reslist$rating<-NULL;reslist$tags<-NULL;
  reslist$summary<-NULL;reslist$alt<-NULL
  reslist$images<-NULL
  attribute=reslist
}
  
  list(title=title,
       author=author,
       rating=as.double(rating),
       summary=summary,
       tags=tags,
       href=href,
       image=image,
       attribute= attribute )
}


