\encoding{UTF-8}
\name{get_book_info}
\alias{get_book_info} 
\title{获取豆瓣图书基本信息}
\description{ 获取豆瓣图书基本信息,包括作者、简介、豆瓣评分等.}
\usage{get_book_info(bookid,...)
}
\arguments{
  \item{bookid}{豆瓣书籍的id号,一串数字,如20326626,可通过图书的豆瓣主页查询}}
\value{一个列表list,包括:
  \item{title}{书名}
  \item{author}{作者}
  \item{rating}{长度为7的向量,用户评分信息,\code{avergae}为总分,\code{votes}指参与的评分人数,\code{starsx}指对应评分用户的所占的比例}
  \item{href}{书籍主页URL}
  \item{image}{书籍封面图片URL}
  \item{summary}{书籍内容简介}
  \item{author_intro}{作者简介}
  \item{tags}{\code{tag_label}标签名称及对应的使用频率\code{tag_freq}}
  \item{comments_total}{评论数量}
  \item{notes_total}{为笔记数量}
  \item{readers}{\code{doings}指正在阅读的用户数量,\code{collections}指已读的用户数量,\code{wishes}指想读的用户数量}
  \item{attribute}{书籍的基本信息,如作者、出版社、ISBN等} 
}

\author{
   <\email{qxde01@@gmail.com}>
}
\examples{
## http://book.douban.com/subject/1291204/
\dontrun{book<-get_book_info(bookid='1291204')}
}
\keyword{book}
\keyword{douban}
