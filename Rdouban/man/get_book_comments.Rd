\name{get_book_comments}
\alias{get_book_comments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
获取豆瓣图书的评论文章.
}
\description{
获取豆瓣图书的评论文章.
}
\usage{
get_book_comments(bookid,n=50,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bookid}{ 豆瓣书籍的id号,一串数字,如20326626}
  \item{n}{获取评论的数量，默认n=50}
}
  
\value{
  一个列表(list), 包括:
  \item{book_title}{书籍名称} 
  \item{comments_amount}{评论文章总数}
  \item{comment_info}{一个data.frame,包含8列:
  \code{titles}评论文章名称,    
  \code{comments}评论文章内容,  
  \code{authors}作者昵称,     
  \code{rating}作者评分,     
  \code{useful}该评论被赞为有用的数量,    
  \code{unuseful}该评论被批评为无用的数量,
  \code{author_url}作者主页, 
  \code{commen_turl}该评论文章的主页}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
<\email{qxde01@@gmail.com}>
}

\seealso{
 \code{\link{get_book_info}}
}
\examples{
\dontrun{get_book_comments(bookid='2120492',n=50)}
}

\keyword{ book }
\keyword{ douban }
\keyword{ comments }
