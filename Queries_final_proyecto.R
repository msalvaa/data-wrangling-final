productos_unicos <- unique(tabla$DESCRIPCION)

df <- tabla

###venta por producto
VentaXproducto <- df %>%
  select(VENTA, DESCRIPCION) %>%
  group_by(DESCRIPCION) %>%
  summarise(TOTAL_VENTA = sum(VENTA))
  
#View(VentaXproducto)         

productos_unicos <- unique(df$DESCRIPCION)

#View(productos_unicos)

###venta por fecha
VentaXfecha <- df %>%
  select(VENTA, FECHA) %>%
  group_by(FECHA) %>%
  summarise(TOTAL_VENTA = sum(VENTA)) 

#View(VentaXfecha) 

###venta por fecha (Ordenado por la mayor fecha de venta)
VentaXfechaOrdenado <- df %>%
  select(VENTA, FECHA) %>%
  group_by(FECHA) %>%
  summarise(TOTAL_VENTA = sum(VENTA)) %>%
  arrange(desc(TOTAL_VENTA))

#View(VentaXfechaOrdenado) 

###venta por acabado
VentaXacabado <- df %>%
  select(VENTA, ACABADO) %>%
  group_by(ACABADO) %>%
  summarise(TOTAL_VENTA = sum(VENTA)) %>%
  arrange(desc(TOTAL_VENTA))

#View(VentaXacabado) 

# ###venta por tienda
# VentaXtienda <- df %>%
#   select(VENTA, TIENDA) %>%
#   group_by(TIENDA) %>%
#   summarise(TOTAL_VENTA = sum(VENTA)) %>%
#   arrange(desc(TOTAL_VENTA))
# 
# View(VentaXtienda) 


###Venta por codigo
VentaXcodigo <- df %>%
  select(VENTA, CODPROD) %>%
  group_by(CODPROD) %>%
  summarise(TOTAL_VENTA = sum(VENTA)) %>%
  arrange(desc(TOTAL_VENTA))

#View(VentaXcodigo) 

###venta total de unidades por codigo
UniXcodigo <- df %>%
  select(CODPROD, UNI) %>%
  group_by(CODPROD) %>%
  summarise(TOTAL_UNI = sum(UNI)) %>%
  arrange(desc(TOTAL_UNI))

#View(UniXcodigo) 

###venta total de unidades dependiendo del acabado
UniXacabado <- df %>%
  select(ACABADO, UNI) %>%
  group_by(ACABADO) %>%
  summarise(TOTAL_UNI = sum(UNI)) %>%
  arrange(desc(TOTAL_UNI))

#View(UniXacabado) 

###Ventas de acuerdo a un rango de fechas
Datos_fechas <- df[df$FECHA >= "2018-01-02" & df$FECHA <= "2018-01-10",] 


####Ya a partir de esto se puede hacer cualquier querie que se quiera saber entre esas fechas.
#  select(VENTA, DESCRIPCION) %>%
#  group_by(DESCRIPCION) %>%
#  summarise(TOTAL_VENTA = sum(VENTA))

#View(Datos_fechas)
