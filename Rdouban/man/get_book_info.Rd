\name{get_book_info}
\alias{get_book_info}
\title{获取豆瓣图书基本信息.}
\usage{
  get_book_info(bookid,...)
}
\arguments{
  \item{bookid}{豆瓣书籍的id号,一串数字,如20326626}
}
\value{
  一个列表(list), 包括:
  \item{book_info}{书籍的基本信息,如作者、出版社等} 
  \item{votes_info}{用户评分信息,score为总分,votes指参与的评分人数,starsx指对应评分用户的所占的比例}
  \item{content_intro}{书籍内容简介}
\item{author_intro}{作者简介}
\item{label_all_number}{用户常用标签数量}
\item{labels}{一个data.frame,\code{label_name}标签名称及对应的使用频率\code{label_freqreq}}
\item{reader_info}{doings指正在阅读的用户数量,collections指已读的用户数量,wishes指想读的用户数量}
}

\description{ 获取豆瓣图书基本信息.}
\note{\code{bookid}必须是豆瓣图书有效的id.}


\author{
   <\email{qxde01@@gmail.com}>
}
\examples{
\dontrun{get_book_info(bookid='2120492')}
}
\keyword{book}
\keyword{ douban }