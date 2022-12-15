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

#active_temp - summarise, не активные температуры стали равны 0

# колонку tavg вытаскиваем из таблицы и переводим в формат вектора
active_temp = as.vector(all_meteo_data$tavg)

# в векторе можно подменить все значения меньше 5 на 0 (делаем это через substitute)
active_temp[active_temp < 5] = 0

# заменяем колонку tavg в таблице метеоданных на колонку-вектор с активными температурами (выше 5 градусов, если ниже то 0)
all_meteo_data = all_meteo_data |> mutate(tavg = active_temp)

# группируем по месяцам в сводную таблицу средние данные активных температур по всем годам и всем метеостанциям
sum_tavg_month = all_meteo_data |> group_by(month) |> summarise(
  sum_tavg = sum(tavg, na.rm = TRUE)/13/11) |> print(n=12)

sum(sum_tavg_month$sum_tavg) # средняя сумма активных температур по Приморью 2322 гр (>5)

# добавляем в сводную таблицу переменные из табл 1 методички https://ecologymodeling.github.io/Tests.html
sum_tavg_month = sum_tavg_month |> mutate(
  afi = c(0,0,0,32.11,26.31,25.64,23.2,18.73,16.3,13.83,0,0),
  bfi = c(0,0,0,11.3,9.26,9.03,8.16,6.59,5.73,4.87,0,0),
  di = c(0,0,0,0.33,1,1,1,0.32,0,0,0,0))

# добавляем в сводную таблицу переменную Fi, расчитанную по формуле из методички
sum_tavg_month = sum_tavg_month |> mutate(Fi = afi+bfi*sum_tavg)

# рассчитываем урожайность пшеницы по формуле из методички с учетом заданных констант

Yj = 10^6 * sum(sum_tavg_month$Fi * sum_tavg_month$di * 300 / (
  1600 * 2.2 * (100 - 25)))
Yj = Yj / 1000 / 100

# Итого, урожайность пшеницы в Приморье (25) в 2006 году 
# составила 13241853 г/га = 132.4 ц/га ЧТО-ТО МНОГОВАТО!
# По данным сайта www.agrodv.ru в 2021 г урожайность в Приморье составила 24.8 ц/га
