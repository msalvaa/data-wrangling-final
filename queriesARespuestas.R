library(dplyr)
library(ggplot2)
library(scales)

# --------------------------------------------------- Pregunta UNO ---------------------------------------------------------------
#  Pregunta: En total según los datos que recibieron, ¿cuál es el acabado más popular?

prod_ventas = select(table,DESCRIPCION,UNI,VENTA,ACABADO)
prod_ventas <- mutate(prod_ventas, 
                      PrecioUnitario = VENTA/UNI)                     
prod_ventas = group_by(prod_ventas, ACABADO)

Resumen <- summarize(prod_ventas, PRODUCTO=length(ACABADO),
                     TOTAL_VENTA = sum(VENTA, na.rm = TRUE))
Resumen = arrange(Resumen,desc(TOTAL_VENTA))

# grafica de resultados

ggplot(data = head(Resumen,10),
       aes(x = ACABADO, y= TOTAL_VENTA, TOTAL_VENTA))+ coord_flip() +
  geom_bar(aes(colour = TOTAL_VENTA), stat = "identity")  +
  scale_fill_gradient(low = "mediumblue", high = "aquamarine2")

View(Resumen) # respuesta a la primera pregunta del correo 

# -------------------------------------------------- Pregunta 2 -------------------------------------------------------------------
# Pregunta: ¿Porqué los días 7, 14, 21 y 28 no tienen información en la base de datos?
table <- table %>%
  mutate(dia = wday(table$FECHA, label = TRUE))

View(table) # se observó que cada siete días no se registraron ventas, y estos díaa caen DOMINGO.

# ------------------------------------------------ Pregunta 3 ---------------------------------------------------------------------
# Pregunta: ¿Qué día del mes es el que presenta más demanda el producto con el código 01502?

diaMasDemanda <- table %>%
  select(FECHA, UNI, CODPROD)%>%
  filter(CODPROD == "01502") %>%
  filter(UNI == max(UNI))

View(diaMasDemanda)
