\encoding{UTF-8}
\name{douban_user_statuses}
\alias{douban_user_statuses}

\title{
获取豆瓣用户的评论、日记等信息
}
\description{
获取豆瓣用户的评论、日记、读书标签等信息.
}
\usage{
douban_user_statuses(nickname,getNote=TRUE,getReview=TRUE,verbose=TRUE,...)
}

\arguments{
  \item{nickname}{豆瓣用户的昵称}
  \item{getNote}{如果为\code{TRUE},将获取用户的日记，否则不获取，默认获取}
  \item{getReview}{如果为\code{TRUE},将获取用户的评论，否则不获取，默认获取}
  \item{verbose}{如果为\code{TRUE}，将显示过程信息，否则不显示，默认显示}
}
\details{
}
\value{一个列表(list)，包含:
  \item{book }{用户所读书籍的数量，\code{doing}指正在阅读的书籍数量,\code{collection}指已读的书籍数量,\code{wish}指想读的书籍数量}
  \item{magazine}{用户所读杂志的数量}
  \item{movie }{用户所看电影的数量，\code{collection}指观看的电影数量,\code{wish}指想观看的电影数量}
  \item{music}{用户所听音乐的数量，\code{doing}指正在收听的音乐数量,\code{collection}指已听过的音乐数量,\code{wish}指想听的音乐数量}
  \item{book_do_labels}{一个data.frame，正在阅读的书籍标签信息,\code{label_name}标签名称及对应的使用频率\code{label_freq}}
  \item{book_wish_labels}{一个data.frame，想要阅读的书籍标签信息,\code{label_name}标签名称及对应的使用频率\code{label_freq}}
  \item{book_collect_labels}{一个data.frame，已经阅读过的书籍标签信息,\code{label_name}标签名称及对应的使用频率\code{label_freq}}
  \item{notes}{一个data.frame，用户发表的日记信息，包括：\code{title}日记标题，\code{time}日记发表时间，\code{note}日记内容，\code{rec_num}获得推荐的次数,\code{fac_num}获得喜欢的次数，\code{url}日记主页网址}
  \item{reviews}{一个data.frame，用户发表的评论信息，包括:\code{target_title}所评论的书籍或电影的名称，\code{title}评论文章标题，\code{time}评论文章发表时间，\code{review}评论文章内容，\code{rating}评分信息，\code{useful}获取的赞的次数，\code{unuseful}被批评为无用的次数，\code{url}评论文章主页网址，\code{type=book}为对书籍的评论，\code{type=movie}为对电影的评论}
}

\author{<\email{qxde01@@gmail.com}>
}
\note{

}

\seealso{

}
\examples{
\dontrun{douban_user_statuses(nickname='qxde01',verbose=TRUE,getNote=F)}
}

\keyword{douban}
\keyword{comment}
