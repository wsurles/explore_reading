##| Explore Reading
source('common.R')

##|---------------
##| Explore  data
##|---------------
df <- read.csv('data/goodreads_export.csv', stringsAsFactors = F)

str(df)

## Calculate cummulative pages read by year
df2 <- df %>%
  filter(
    Exclusive.Shelf == 'read',
    Date.Read != ""
  ) %>%
  mutate(
    date = as.Date(Date.Read, format = "%Y/%m/%d"),
    yday = yday(date),
    year = year(date),
    pages = Number.of.Pages
  ) %>%
  filter(date >= "2010-01-01") %>%
  select(year, yday, pages, Title)
  
## Expand Dates
grid <- expand.grid(year = unique(df2$year), yday = seq(1:365))
df3 <- merge(grid, df2, by=c("year","yday"), all.x = T)
str(df3)

## Crunch by year and month
df4 <- df3 %>%
  mutate(
    date = as.Date(paste0(year,'-',yday), format = "%Y-%j"),
    date_str = as.character(date),
    month = month(date),
    pages = ifelse(is.na(pages), 0, pages),
    title = ifelse(is.na(Title), "", Title)
  ) %>%
  group_by(year) %>%
  mutate(
    cum_year_pages = cumsum(pages)
  ) %>%
  filter(date <= today()) %>%
  arrange(date) %>%
  data.frame()

str(df4)
df5 <- left_join(df4,df)

##| YOY cumulative by month line
p <- nPlot(cum_year_pages ~ yday, data = df4, group = "year", type = 'lineChart')
p$xAxis(axisLabel = 'Day of Year')
p$chart(tooltipContent = "#!
        function(key, x, y, d){ 
          return '<h3>' + d.point.year + '</h3>' +
          '<p><b>' + d.point.date_str + '</b></p>' + 
          '<p><b>'  + 'Total Pages: ' + d.point.cum_year_pages + '</b></p>' +
          '<p><b>'  + d.point.title + '</b></p>' +
          '<p><b>'  + 'Pages: ' + d.point.pages + '</b></p>'
        }
        !#")
p$xAxis(
  # Set the tick labels to month 
  tickFormat="#!function(d) {return d3.time.format('%b')(new Date( d * 86400000 ));}!#"
  # Put the ticks at the first of each month  
  ,tickValues = "#!data[0].values
      .map(function(v){
        return v[opts.x]
      })
      .filter(function(v){
        return d3.time.format('%d')(new Date(v*60*60*24*1000)) == '01'
      })!#"
)
p
p$save('reading_william.html', standalone = TRUE)



