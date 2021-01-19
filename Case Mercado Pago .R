#INSTALACION DE PAQUETERIAS NECESARIAS
install.packages("gsubfn")
install.packages("proto")
install.packages("RSQLite")
install.packages("sqldf")
install.packages("readr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("hrbrthemes")


#LLAMADO DE PAQUETERIAS
library("proto")
library("gsubfn")
library("RSQLite")
library("sqldf")
library("readr")
library("lubridate")
library("ggplot2")
library("readxl")
library("dplyr")
library("hrbrthemes")



#Cargar tablas
devices<-read_excel("~/DOWNLOADS/Work Sample - Point.xlsx",sheet = 1)
tpv_mensual<-read_excel("~/DOWNLOADS/Work Sample - Point.xlsx",sheet = 2)

#Visualizacion primeras lineas de datos
head(devices)
head(tpv_mensual)

#Visualizacion de tabla de datos
summary(devices)
summary(tpv_mensual)

#Agregar weekday a tabla devices
devices$weekday<-as.character(devices$FECHA_VENTA,format='%a')

#Cambiar formato fecha_venta de datetime a chr manejo en sqldf
devices$FECHA_VENTA<-as.character(devices$FECHA_VENTA,format='%Y-%m-%d')

#Soluciones a las preguntas

#Question 1
#¿Cuánto crecieron las ventas en Hot Sale con la Promoción? 

#Solucion:

#Tabla auxiliar para hacer gráfica de comportamiento de ventas contando los distintos dispositivos vendidos por día
Question_1<-sqldf('SELECT fecha_venta as sales_date, weekday,
                  case 
                  when fecha_venta between "2019-05-20" and "2019-05-24" then "the_week_before"
                  when fecha_venta between "2019-05-25" and "2019-05-26" then "weekend"
                  when fecha_venta between "2019-05-27" and "2019-05-31" then "hotsale"
                  else  "NA"
                  end as date_label,
                  COUNT(distinct device_id) as devices_sold
                  FROM devices 
                  GROUP BY 1,2,3')

#Visualizar resultado de la consulta
head(Question_1)

# Incrementar margen de gráficas
par(mar=c(8,4,4,4))

#Gráfica para visualizar el comportamiento de ventas
barplot1<-barplot(Question_1$devices_sold,main='Devices sold',names.arg=paste(Question_1$sales_date,Question_1$weekday),ylim=c(0,600),las=2,
                  col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6),rgb(0.3,0.9,0.4,0.6)))
text(barplot1, Question_1$devices_sold+30 ,  Question_1$devices_sold ,cex=1) 
abline(v=c(6.1 , 8.5) , col="grey")
legend("topleft", legend = c("The week before","Weekend","Hotsale" ) , 
       col=c(rgb(0.3,0.1,0.4,0.6),rgb(0.3,0.5,0.4,0.6),rgb(0.3,0.9,0.4,0.6)), 
       bty = "n", pch=20 , pt.cex = 2, cex = 1, horiz = FALSE, inset = c(0.05, -0.05))

#Obtencion de dispotivos vendidos por cada periodo de tiempo
Question_1_2<-sqldf('
                  SELECT 
                  date_label,
                  sum(devices_sold)  as devices_sold
                  FROM Question_1
                  group by 1')

#Visualizar consulta 
head(Question_1_2)

Question_1_answer<-sqldf('
                  SELECT 
                  (sum(devices_sold) filter (where date_label=="hotsale")*1.00/sum(devices_sold) filter(where date_label=="the_week_before")-1)*100.00 as "%growth"
                  FROM Question_1_2
                    ')

#Respuesta a pregunta 1: crecimiento de ventas
print(Question_1_answer)

#Question 2
#¿Cuál fue la tasa de activación de los dispositivos y cómo se compara contra la semana anterior?

#Solucion:

#Cambiar formato fecha_venta y mes de datetime a chr para manejo en sqldf
tpv_mensual$FECHA_ACTIVACION<-as.character(tpv_mensual$FECHA_ACTIVACION,format='%Y-%m-%d')
tpv_mensual$MES<-as.character(tpv_mensual$MES,format='%Y-%m-%d')

#Se crea consulta que relacione tabla devices y tpv mensual y nos de por DEVICE_ID su fecha de venta,
#fecha de activación, etiqueta de periodo y dias de conversión (resta entre fecha de activación y venta)

Question_2<-sqldf('
                  SELECT 
                  dev.device_id,
                  dev.fecha_venta as sales_date,
                  tpv.fecha_activacion as activation_date,
                  case 
                  when dev.fecha_venta between "2019-05-20" and "2019-05-24" then "the_week_before"
                  when dev.fecha_venta between "2019-05-25" and "2019-05-26" then "weekend"
                  when dev.fecha_venta between "2019-05-27" and "2019-05-31" then "hotsale"
                  else  "NA"
                  end as date_label,
                  (strftime("%s",tpv.fecha_activacion)-strftime("%s",dev.fecha_venta))/60/60/24 as conversion_days
                  FROM devices dev
                  LEFT JOIN  tpv_mensual tpv on tpv.device_id=dev.device_id
                  group by 1,2,3,4
                    ')
#Imprimir primeras lineas
head(Question_2)

#Se crea consulta para obtener el conteo de dispotivos activados y vendidos por periodo de tiempo
#así como la tasa de activación y estadísticas de tendencia central
Question_2_1<-sqldf('
                  SELECT 
                  date_label,
                  count(device_id) filter (where activation_date is not null) as activated_devices,
                  count(device_id) as sold_devices,
                  count(device_id) filter (where activation_date is not null)*100.00/count(device_id) as activation_rate,
                  AVG(conversion_days)  filter (where activation_date is not null) as avg_conversion_days ,
                  median(conversion_days)  filter (where activation_date is not null) as median_conversion_days ,
                  mode(conversion_days)  filter (where activation_date is not null) as mode_conversion_days 
                  FROM Question_2 q2
                  group by 1
                  ')

#Visualización de respuesta 
print(Question_2_1)

#Adicional
#Gráfica histograma espejo de los días de conversión
q2 <-ggplot(Question_2, aes(x = conversion_days )) +
  geom_histogram(aes(y = ..count..),  fill=rgb(0.3,0.1,0.4,0.6),breaks=seq(0, 50, by = 5),
                 data = ~ subset(., date_label == "the_week_before"  & !(is.na(conversion_days)) & conversion_days>=0 )) +
  geom_label( aes(x=20, y=100, label="The week before"), color=rgb(0.3,0.1,0.4,0.6)) +
  geom_histogram(aes(y = -..count..), fill= rgb(0.3,0.9,0.4,0.6),breaks=seq(0, 50, by = 5),
                 data = ~ subset(., date_label == "hotsale" & !(is.na(conversion_days)) & conversion_days>=0)) +
  geom_label( aes(x=20, y=-200, label="Hotsale"), color=rgb(0.3,0.5,0.4,0.6)) +
  geom_hline(yintercept = 0)+
  ggtitle("Conversion days density")+
  theme_ipsum() +
  xlab("Conversion days")

#Visualización de gráfico
print(q2)

#Question 3
#	¿Cuánto creció el TPV por dispositivo?

#Solucion:

#Consulta auxiliar obtener el TPV por mes total y el promedio 
Question_3<-sqldf('
                  SELECT 
                  strftime("%Y-%m",tpv.mes)  as month,
                  "total" as date_label,
                  sum(tpv) as total_tpv,
                  count(distinct (tpv.device_id)) as devices,
                  sum(tpv)/count(distinct (tpv.device_id)) as avg_tpv
                  FROM tpv_mensual tpv
                  left JOIN  devices dev on dev.device_id=tpv.device_id
                  where mes>="2019-05-01"
                  group by 1,2
                    ')

#Consulta para obtener las variaciones de avg_tpv por mes
Question_3_1<-sqldf('
                  SELECT 
                   month,
                  date_label,
                  round(total_tpv/1000,1) as total_tpv_k,
                  devices,
                  avg_tpv,
                  "Δ"||cast(round((avg_tpv/(LAG( avg_tpv) over (order by month)) -1)*100,1) as char)||"%"  as delta
                  FROM Question_3 tpv
                  group by 1,2,3,4,5
                    ')

#Imprimir respuesta 
print(Question_3_1)

#Grafica de TPV por mes y AVG TPV
q3<-ggplot(Question_3_1, aes(x=month, y=avg_tpv, group=date_label)) +
  geom_line(aes(y=avg_tpv,fill=rgb(0, 0, 1, 0.5),colour = "Avg_TPV"))+
  geom_point(aes(y=avg_tpv,colour = "Avg_TPV"))+
  geom_text( aes(y=avg_tpv+400,label=delta), color='black') +
  geom_bar( aes(y=total_tpv_k/1),stat="identity", size=0.1, fill='red', alpha=0.6) +
  scale_y_continuous(
    # Features of the first axis
    name = "Average TPV",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1000.0, name="Total TPV (M)",labels = scales::comma),  labels = scales::comma

  )+
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "TPV by device",
     x = "month",
     colour = "")+
  ggtitle("Average & total TPV by month")

#Imprimir Gráfico q3
print(q3)

#Consulta para sacar TPV agrupado por periodo de venta de los dispositivos
Question_3_2<-sqldf('
                  SELECT 
                    strftime("%Y-%m",tpv.mes)  as month,
                  case 
                  when dev.fecha_venta between "2019-05-20" and "2019-05-24" then "the_week_before"
                  when dev.fecha_venta between "2019-05-25" and "2019-05-26" then "weekend"
                  when dev.fecha_venta between "2019-05-27" and "2019-05-31" then "hotsale"
                  else  "NA"
                  end as date_label,                  
                  sum(tpv) as total_tpv,
                  count(distinct (tpv.device_id)) as devices,
                  sum(tpv)/count(distinct (tpv.device_id)) as avg_tpv
                  FROM tpv_mensual tpv
                  left JOIN  devices dev on dev.device_id=tpv.device_id
                  where mes>="2019-05-01"
                  group by 1,2
                    ')

#Imprimir primeras lineas consulta
head(Question_3_2)

#Grafico de lineas para graficar TPV promedio por periodo de compra de los dispositivos
q3_1<-ggplot(Question_3_2, aes(x=month, y=avg_tpv, group=date_label)) +
  geom_line(aes(y=avg_tpv,fill=rgb(0, 0, 1, 0.5),colour = date_label))+
  geom_point(aes(y=avg_tpv,colour = date_label))+
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = "Avg TPV",
       x = "month",
       colour = "Devices sales period")+
  scale_y_continuous(name="Avg TPV", labels = scales::comma)+
  ggtitle("Average TPV by month")

#Imprimir gráfico
print(q3_1)



#Question 4
#	¿Cuál fue el ROI de la promoción?

#Solucion:

#Consulta obtencion total TPV de los dispotivos vendidos en el hotsale
Question_4<-sqldf('
                  SELECT 
                  "hotsale" as date_label,
                  sum(tpv) filter (where dev.fecha_venta between "2019-05-27" and "2019-05-31") as total_tpv
                  FROM tpv_mensual tpv
                  left JOIN  devices dev on dev.device_id=tpv.device_id
                  where mes>="2019-05-01"
                  group by 1
                    ')

#Imprimir consulta
print(Question_4)

#Consulta para obtener el ROI de la promoción
Question_4_answer<-sqldf('
                  SELECT 
                  q4.date_label,
                  q4.total_tpv,
                  q4.total_tpv*0.035 as comission,
                  sum(q1.devices_sold) as devices_sold,
                  sum(q1.devices_sold)*(299-99) as investment,
                  (q4.total_tpv*0.035-(sum(q1.devices_sold)*(299-99)))/(sum(q1.devices_sold)*(299-99))*100 as roi,
                  (q4.total_tpv*0.035-(sum(q1.devices_sold)*(299-99)))/sum(q1.devices_sold) as benefit_per_unit
                  FROM Question_4 q4
                   join Question_1 q1 on q1.date_label=q4.date_label
                  group by 1
                    ')

#Impresion de respuesta
print(Question_4_answer)
