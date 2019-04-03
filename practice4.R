
# загрузка пакетов

library('R.utils')               # gunzip() для распаковки архивов 
library('sp')                    # функция spplot()

library('ggplot2')               # функция ggplot()
library('RColorBrewer')          # цветовые палитры
require('rgdal')                 # функция readOGR()
library('broom')                 # функция tidy()
require('dplyr')                 # функция join()
library('scales')                # функция pretty_breaks()

## установка и сборка пакета «gpclib»
## установить RTools (recommended) отсюда:
## http://cran.r-project.org/bin/windows/Rtools/
install.packages('gpclib', type = 'source')
library('gpclib')
library('maptools')

gpclibPermit()



# ссылка на файл

ShapeFileURL <- 'https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_RUS_shp.zip'
# создаём директорию 'data' и скачиваем
if (!file.exists('./data')) dir.create('./data')
if (!file.exists('./data/36_RUS_shp.zip')) {
  download.file(ShapeFileURL,
                destfile = './data/36_RUS_shp.zip')}
#Файл, который мы скачали – это архив формата «.zip». Распакуем его.
# распаковать архив
unzip('./data/36_RUS_shp.zip', exdir = './data/36_RUS_shp')
# посмотреть список файлов распакованного архива
dir('./data/36_RUS_shp')



# прочитать данные уровней 0, 1, 2
Regions0 <- readOGR("./data/36_RUS_shp/gadm36_RUS_0.shp")
Regions1 <- readOGR("./data/36_RUS_shp/gadm36_RUS_1.shp")
Regions2 <- readOGR("./data/36_RUS_shp/gadm36_RUS_2.shp")
Regions3 <- readOGR("./data/36_RUS_shp/gadm36_RUS_3.shp")

par(mfrow = c(1, 1))



plot(Regions0, main = 'adm0')
plot(Regions1, main = 'adm1')
plot(Regions2, main = 'adm2')
plot(Regions3, main = 'adm2')

rm(Regions0, Regions2, Regions3)

# имена слотов
slotNames(Regions1)

# слот "данные"
Regions1@data

# делаем фактор из имён областей (т.е. нумеруем их)
Regions1@data$NAME_1 <- as.factor(Regions1@data$NAME_1)

# результат
Regions1@data$NAME_1

# строим картограмму
spplot(Regions1, # объект 'SpatialPolygonsDataFrame'
       'NAME_1', # показанная цветом переменная
       scales = list(draw = T), # рисовать координатную сетку
       col.regions = rainbow(n = 83) # цвета для заливки
       
) 



reg <- read.csv('region.csv', stringsAsFactors = F, sep = ';')
#month_pay <- reg$month_pay

Regions1@data <- merge(Regions1@data, reg) 
head(Regions1@data$month_pay)
# задаём палитру
mypalette <- colorRampPalette(c('whitesmoke', 'blue'))
spplot(Regions1@data, 'month_pay',
       col.regions = mypalette(40), # цветовая шкала
       # (20 градаций)
       col = 'coral4', # цвет контурных линий
       par.settings = list(axis.line = list(col = NA)) # без
       # осей
       
)
tab <- select(Regions1@data, NAME_1,month_pay)
