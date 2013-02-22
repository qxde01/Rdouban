\name{get_book_info}
\alias{get_book_info}
\title{获取豆瓣图书基本信息.}
\description{ 获取豆瓣图书基本信息,包括作者、简介、豆瓣评分等.}

\usage{
  get_book_info(bookid,...)
}
\arguments{
  \item{bookid}{豆瓣书籍的id号,一串数字,如20326626，可通过图书的豆瓣主页查询}
}
\value{
  一个列表(list), 包括:
  \item{book_base_info}{一个字符串，书籍的基本信息,如作者、出版社等} 
  \item{votes_info}{长度为7的向量,用户评分信息,\code{score}为总分,\code{votes}指参与的评分人数,\code{starsx}指对应评分用户的所占的比例}
  \item{content_intro}{一个字符串，书籍内容简介}
  \item{author_intro}{一个字符串，作者简介}
  \item{label_amount}{一个整数，用户常用标签数量}
  \item{labels}{一个data.frame,\code{label_name}标签名称及对应的使用频率\code{label_freq}}
  \item{comments_notes_amount}{长度为2的向量，其中\code{comments_amount}评论数量,\code{notes_amount}为笔记数量}
  \item{reader_info}{长度为3的向量，\code{doings}指正在阅读的用户数量,\code{collections}指已读的用户数量,\code{wishes}指想读的用户数量}
}


\note{\code{bookid}必须是豆瓣图书有效的id.}

\author{
   <\email{qxde01@@gmail.com}>
}
\examples{
\dontrun{get_book_info(bookid='2120492')}
}
\keyword{book}
\keyword{douban}
