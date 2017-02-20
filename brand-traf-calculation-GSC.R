#устанавливаем библиотеки для работы с Search Console и авторизации. 
#Нужно только при первом запуске
install.packages("googleAuthR")
install.packages("searchConsoleR")
install.packages("ggplot2")

##добавляем библиотеки при каждом новом запуске скрипта
#библиотека для авторизации в сервисах Google
library(googleAuthR)
#библиотека для запросов к API Search Console
library(searchConsoleR)
#библиотека для визуализации в R
library(ggplot2)

setwd("C:/Users/User/Documents/R/search-console")

### 1. Выгрузка данных.

## Пишем домен сайта из Search Console, для которого нужно выгрузить данные.
## Обязательно указывать протокол как в интерфейсе Search Console.
## Ниже пример для сайта olx.ua.
website <- "https://www.olx.ua/"

## Eстанавливаем нужные даты. End (конец периода) не менее 3 дней до текущей даты.
start <- Sys.Date() - 3
end <- Sys.Date()-3

## Авторизация в Search Console.  
## В первый раз нужно будет разрешить доступ к данным, но дальше происходит автоматическое 
## обновления токена.
gar_auth()

## Перечислите все возможные вариации брендовых фраз для вашего сайта.
## Проще всего взять их из Search Console и постоянно дополнять.
brand_queries <- c('алх','емаркет','оглх','одх','одч','оеликс','оелікс','оил','ойл',
                   'олдх','олеикс','олекс','олеікс','олз','оликс','олихкз','олих','олкс',
                   'оллх','олнх','олх','олч','олъ','олыкс','олікс','олx','оох','оэликс',
                   'оілх','оілікс','оlх','оlx','сладно','сланда','сландо','щдл','щдх',
                   'щдч','щшдч','ыдфтвщ','ідфтвщ','0лх','cландо','ckfylj','ckfyuj',
                   'clando','emarket','jk[','jkbrc','jkrc','jk{','oil','oix','oixkz',
                   'ojx','olх','ol.x','ol[.kz','olix','ollx','olx','oxl','slando','slondo')

## Создаем пустой data frame для записи результата.
data <- data.frame()

## Выгрузка данных по брендовым фразам. Для каждого варианта бренда делается запрос к API.
for (bq in brand_queries){
  data_n <- search_analytics(siteURL = website, 
                         startDate = start, 
                         endDate = end, 
                         dimensions = c('device'), 
                         dimensionFilterExp = c(paste("query~~", bq, sep = "")),
                         searchType = c('web'),
                         rowLimit = 10000,
                         walk_data = "none")
  data <- rbind(data, data_n)
}
## Удаляем пустые значения из результата.
data <- na.exclude(data)

## Суммируем значения по каждому типу устройств.
data <- aggregate(. ~ device, data=data, FUN=sum)

## Оставляем только клики.
data <- data[,1:2]

## Добавляем столбец "Тип запроса".
data$query_type <- "brand"


## Выгружаем все клики по устройствам.
data_all <- search_analytics(siteURL = website,
                             startDate = start,
                             endDate = end,
                             dimensions = c('device'),
                             searchType = c('web'),
                             rowLimit = 10000,
                             walk_data = "none")

## Оставляем только нужные столбцы.
data_all <- data_all[,1:2]

## Создаем датафрейм с результатом.
result <- data

## Считаем количество кликов для nonbrand.
result <- rbind(result, data.frame(device = 'DESKTOP',
                     clicks = data_all$clicks[data_all$device=='DESKTOP']-
                       data$clicks[data$device=='DESKTOP'],
                     query_type = 'nonbrand'))
result <- rbind(result, data.frame(device = 'MOBILE',
                     clicks = data_all$clicks[data_all$device=='MOBILE']-
                       data$clicks[data$device=='MOBILE'],
                     query_type = 'nonbrand'))
result <- rbind(result, data.frame(device = 'TABLET',
                     clicks = data_all$clicks[data_all$device=='TABLET']-
                       data$clicks[data$device=='TABLET'],
                     query_type = 'nonbrand'))


## создаем выборки по типу запроса чтобы просуммировать для устройств.
brand <- subset(result, query_type=='brand')
nonbrand <- subset(result, query_type=='nonbrand')


## Суммируем данные по устройствам и вставляем в общий data frame.
result <- rbind(result, data.frame(device = 'ALL', 
                                       clicks = sum(brand$clicks), 
                                       query_type = 'brand'))

result <- rbind(result, data.frame(device = 'ALL', 
                                       clicks = sum(nonbrand$clicks),
                                       query_type = 'nonbrand'))


## Записываем данные в csv файл.
filename <- paste("brand_search_analytics",
                  Sys.Date(),
                  'device',
                  'web',
                  ".csv",sep="-")

write.csv(data_all, filename)



### 2. Визуализация.

## Функция для визуализации со статьи
## http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization
visual <- function(x, type){
  
  library(scales)
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=10, face="bold")
    )
  
  ggplot(x, aes(x="", y=clicks, fill=query_type))+
    geom_bar(width = 1, stat = "identity")+
    coord_polar("y", start=0)+
    scale_fill_brewer("Query type") + 
    blank_theme +
    theme(axis.text.x=element_blank())+
    geom_text(aes(y = clicks/2 + 
                    c(0, cumsum(clicks)[-length(clicks)]), 
                  label = percent(clicks/sum(clicks))), 
              size=4)+
    ggtitle(sprintf("Clicks from %s devices",c(type)))
}

## Функция для свода нескольких графиков на одном.
## Тут ее взял http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## создаем выборки по устройствам.
desktop <- subset(result,device=='DESKTOP')
mobile <- subset(result,device=='MOBILE')
tablet <- subset(result,device=='TABLET')
all_devices <- subset(result, device=='ALL')

## Создаем графики для каждого типа устройств. 
## Не забываем перед этим запустить функцию 'visual'                 
p_desktop <- visual(desktop, 'Desktop')
p_mobile <- visual(mobile,'Mobile')
p_tablet <- visual(tablet,'Tablet')
p_all_devices <- visual(all_devices, 'All')

## Сводим все на один график. Перед этим запускаем функцию multiplot.
multiplot(p_desktop, p_mobile, p_tablet, p_all_devices, cols=3)
