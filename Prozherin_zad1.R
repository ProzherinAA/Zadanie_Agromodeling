library(tidyverse)
library(lubridate)
library(rnoaa)

# скачиваем список всех метеостанций мира, процесс может занять до нескольких минут
# station_data = ghcnd_stations()

# сохраняем скачанные список метеостанций в отдельный файл, чтобы больше не обращаться к нему через интернет
# write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")

# для региона 25 создаем таблицу с коррдинатами столицы Владивосток
vladivostok = data.frame(id = "VLADIVOSTOK",
                  latitude = 43.1056,
                  longitude = 131.874)

# обращаемся к ближайшим метеостанциям в локации Владивостока,
# лимит подбираем, чтобы 11 станций было дальше 60 км [1993-2005 гг]
vladivostok_around = meteo_nearby_stations(lat_lon_df = vladivostok,
                                    station_data = station_data,
                                    limit = 13,
                                    var = c("PRCP", "TAVG"),
                                    year_min = 1993, year_max = 2005)

# проверяем тип данных (получили list из одной строки с вложеной таблийцей)
class(vladivostok_around)

# чтобы далее работать с данными надо перевести строку в формат таблицы, для этого просто обращаемся к этой строке с таблицей в текстовом формате
vladivostok_around = vladivostok_around[[1]]

# убираем из таблицы данные с метеостанций ближе 60 км от Владивостока
vladivostok_around = vladivostok_around |> filter(distance >= 60)

# создаем вектор с id номерами выбранных метеостанций 
meteo_id_11 = vladivostok_around[,1]

# получаем все данные со всех выбранных метеостанций
all_meteo_data = meteo_tidy_ghcnd(stationid = meteo_id_11)

all_meteo_data$prcp # Precipitation - осадки
all_meteo_data$snwd # Snow - высота зимних осадков
all_meteo_data$tavg # Temoerature air averaged - средняя темп воздуха за день
all_meteo_data$tmax # Temoerature air max - максимальная темп воздуха за день
all_meteo_data$tmin # Temoerature air min - минимальная темп воздуха за день

# добавляем в таблицу колонки с разделенной датой (год, месяц, день)
all_meteo_data = all_meteo_data |> mutate(
  year = year(date), month = month(date), doy =yday(date))

# фильтруем по годам так, чтобы выбрать период 13 лет, предшествующий 2006 году
all_meteo_data = all_meteo_data |> filter(year > 1992 & year < 2006)

# переводим размерности значений в привычную для пользователей форму (меторологи не любят точки)
all_meteo_data = all_meteo_data |> mutate(
  prcp = prcp /10, snwd = snwd /10, tavg = tavg /10, tmax =tmax/10, tmin = tmin/10)

# в векторе можно подменить все значения меньше 5 на 0 (делаем это через substitute)
all_meteo_data$tavg[all_meteo_data$tavg < 5] = 0

# группируем по месяцам, годам и id 
# и сводим в таблицу с помесячными суммами активных температур на все станции и годы
sum_monht_tavg = all_meteo_data |> group_by(month, id, year) |> 
  summarise(sum_tavg = sum(tavg, na.rm = TRUE)) |> print(n=500) 

# сбрасываем группировку
sum_monht_tavg = ungroup(sum_monht_tavg)

# группируем по месяцам и сводим в таблицу со средними помесячными активными температурами 
mean_month_tavg = sum_monht_tavg |> group_by(month) |>
  summarise(mean_tavg = mean(sum_tavg, na.rm=T))

# считаем среднюю сумму активных температур (>5) за год
sum_year_tavg = sum(mean_month_tavg$mean_tavg) #получили 2965 градусов 

# по данным в интернете средняя сумма активных температур (>10) по Приморью 2400-2600 градусов

# добавляем в сводную таблицу переменные из табл 1 методички https://ecologymodeling.github.io/Tests.html
mean_month_tavg = mean_month_tavg |> mutate(
  afi = c(0,0,0,32.11,26.31,25.64,23.2,18.73,16.3,13.83,0,0),
  bfi = c(0,0,0,11.3,9.26,9.03,8.16,6.59,5.73,4.87,0,0),
  di = c(0,0,0,0.33,1,1,1,0.32,0,0,0,0))

# добавляем в сводную таблицу переменную Fi, расчитанную по формуле из методички
mean_month_tavg = mean_month_tavg |> mutate(Fi = afi+bfi*mean_tavg)

# рассчитываем урожайность пшеницы по формуле из методички с учетом заданных констант
Yj = 10^6 * sum(mean_month_tavg$Fi * mean_month_tavg$di * 300 / (
  1600 * 2.2 * (100 - 25)))
Yj = Yj / 1000 / 100

# Итого, урожайность пшеницы в Приморье (25) в 2006 году 
# составила 16817595 г/га = 168 ц/га ЧТО-ТО МНОГОВАТО!
# По данным сайта www.agrodv.ru в 2021 г урожайность в Приморье составила 24.8 ц/га
