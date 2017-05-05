##| Compare books to Mike
source('common.R')

##|---------------
##| Explore  data
##|---------------
friend <- 'mike_soltys'
friend <- 'jad_taylor'

df <- read.delim(paste0('data/book_compare_',friend,'.tsv'), 
                  stringsAsFactors = F)

colnames(df) <- c("title", "friend_rating", "my_rating")

df2 <- df %>%
  mutate(
    friend_rating = suppressWarnings(as.numeric(substring(friend_rating, 1, 1))),
    my_rating = suppressWarnings(as.numeric(substring(my_rating, 1, 1))),
    friend_rating_jitter = jitter(friend_rating),
    my_rating_jitter = jitter(my_rating),
    point_size = friend_rating + my_rating
  )

##| Chart stars scatter
p1 <- nPlot(my_rating_jitter ~ friend_rating_jitter, data = df2, type = 'scatterChart')
p1$xAxis(axisLabel = 'Friend')
p1$yAxis(axisLabel = 'Me')
p1$chart(forceY = c(0, 5))
p1$chart(forceX = c(0, 5))
p1$chart(size = '#! function(d){return d.point_size} !#')
p1$chart(tooltipContent = "#!
          function(key, x, y, d){ 
            return '<h3>' + d.point.title + '</h3>' +
            '<p> Me: ' + d.point.my_rating + '</p>' +
            '<p> Friend: ' + d.point.friend_rating + '</p>'
            
          }
          !#")
p1

## TODO
## - Write a quick book report.
## - Show the scatter of books 
## - Show a table of star diff where mike is higher in order of grestest to least diff
## - Show a table of star diff for me vs mike
## - Show the process of obtaining and munging the data to create these insights
## - Attempt to explain why I like books more and the kinds of books we like more
## - Describe 
## - 

