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

plot(Regions0, main = 'adm0', asp = 1.8)
plot(Regions1, main = 'adm1', asp = 1.8)
plot(Regions2, main = 'adm2', asp = 1.8)
plot(Regions3, main = 'adm2', asp = 1.8)

rm(Regions0, Regions2, Regions3)

# имена слотов
slotNames(Regions1)

# слот "данные"
head(Regions1@data)
dat1 <- Regions1@data
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
month_pay <- reg$month_pay

Regions1@data <- merge(reg, Regions1@data, by.x='reg', by.y='NAME_1')

# задаём палитру
mypalette <- colorRampPalette(c('whitesmoke', 'blue'))
spplot(Regions1, 'month_pay',
       col.regions = mypalette(20), # цветовая шкала
       # (20 градаций)
       col = 'coral4', # цвет контурных линий
       par.settings = list(axis.line = list(col = NA)) # без
       # осей
)


#2 часть постройка карты с помощью ggplot()======
 rm(Regions1)

Regions <- readOGR("./data/36_RUS_shp/gadm36_RUS_2.shp")
Regions@data$id <- Regions@data$NAME_2

# преобразовываем SpatialPolygonsDataFrame в data.frame
Regions.points <- tidy(Regions, region = 'id')

# добавляем к координатам сведения о регионах
Regions.df <- merge(Regions.points, Regions@data, by = 'id')

Regions_1.1$HASC_2 <- as.character(Regions_1.1$HASC_2)

Regions_1 <- Regions.df[grepl('^RU[.]KK', Regions.df$HASC_2),]
popul <- read.csv('popul.csv',stringsAsFactors = F, sep = ';')
Regions_1.1 <- merge(popul, Regions_1, by.x='name_of_reg', by.y='NAME_2')
# создаём график

gp <- ggplot() +
  geom_polygon(data = Regions_1.1,
               aes(long, lat, group = group,
                   fill = population)) +
  geom_path(data = Regions_1.1,
            aes(long, lat, group = group),
            color = 'coral4') +
  coord_map(projection = 'mercator') +
  scale_fill_distiller(palette = 'OrRd',
                       direction = 1,
                       breaks = pretty_breaks(n = 5)) +
  labs(x = 'Долгота', y = 'Широта',
       title = "Население") 

# выводим график

gp 

