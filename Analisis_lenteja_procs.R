---
  title: ""
author: ""
format:
  html:
  toc: true
toc-expand: true
toc-depth: 4
toc-location: left
number-sections: true
self-contained: true
code-fold: true
output-file: "ESM"
editor_options: 
  chunk_output_type: console
execute: 
  warning: false
message: false
echo: true
---
  
  # Project Setup
  
  ```{r}
#| label:  setup


source('https://inkaverse.com/setup.r')
```

# Data import

```{r}
gs <- "https://docs.google.com/spreadsheets/d/1orK27clI5Y0CTXDxhTnAd9EN5feucpNRoRr6Cl1JWSs/edit?gid=1366689696#gid=1366689696" %>% 
  as_sheets_id()


dat <- gs %>% 
  range_read(ss = .,sheet = "dat")
```
#modelo estadistico 

"$$alt_plant = u + block+T +d+T*d e$$"

  
  ```{r}
#Análisis de varianza

str(dat)

modelo <- aov(alt_plant ~ block + tipo_fertilizante + dia + tipo_fertilizante*dia, data = dat)
anova(modelo)
```


```{r}
dat
```

#comparación de medias




```{r}
library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)

# 1. Calcular medias y extraer las letras de significancia
mc <- emmeans(modelo, ~ tipo_fertilizante)
medias_letras <- cld(mc, adjust = "tukey", Letters = letters)

# Convertir a data.frame y limpiar los espacios extra en las letras
medias_letras <- as.data.frame(medias_letras)
medias_letras$.group <- trimws(medias_letras$.group)

# 2. Construir el gráfico
ggplot(medias_letras, aes(x = tipo_fertilizante, y = emmean)) +
  
  # Barras
  geom_col(
    width = 0.40,
    fill = "#9ACD32",
    color = "black",
    linewidth = 0.4
  ) +
  
  # Barras de error (Intervalos de confianza)
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.08,
    linewidth = 0.8
  ) +
  
  # Línea conectora pulida (estilo spline)
  geom_smooth(
    aes(group = 1), 
    method = "loess", 
    se = FALSE, 
    color = "#9ACD32", 
    linetype = "dashed", 
    linewidth = 1,
    alpha = 0.6
  ) +
  
  # Puntos de anclaje para la línea
  geom_point(
    color = "#556B2F", 
    size = 2.5
  ) +
  
  # Etiquetas de las medias (los números)
  geom_label(
    aes(label = sprintf("%.2f", emmean)),
    fill = "white",
    color = "black",
    linewidth = 0.25,
    size = 4,
    fontface = "bold",
    vjust = 1.2 # Ajustado hacia abajo para dar espacio a las letras arriba
  ) +
  
  # *** NUEVO: Letras de Significancia Estadísticas ***
  geom_text(
    aes(y = upper.CL + 0.3, label = .group), # Ubicadas justo encima de la barra de error
    size = 6,
    fontface = "bold",
    color = "black" # Puedes cambiarlo a "red" si quieres que resalten más
  ) +
  
  # Etiquetas de los ejes
  labs(
    x = "Tratamientos (Fertilizantes)",
    y = "Altura promedio de la planta (cm)"
  ) +
  
  # Expandir el límite superior para que las letras no se corten
  expand_limits(y = max(medias_letras$upper.CL) + 1.5) +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text = element_text(
      size = 12,
      color = "black"
    ),
    axis.line = element_line(
      linewidth = 0.8
    ),
    axis.ticks = element_line(
      linewidth = 0.8
    ),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

# 3. Guardar el gráfico
ggsave(
  "altura_lentejas.jpg",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 300
)


#Modelo7

# 1. Calcular medias y extraer las letras de significancia
mc <- emmeans(modelo7, ~ tipo_fertilizante)
medias_letras <- cld(mc, adjust = "tukey", Letters = letters)

# Convertir a data.frame y limpiar los espacios extra en las letras
medias_letras <- as.data.frame(medias_letras)
medias_letras$.group <- trimws(medias_letras$.group)

# 2. Construir el gráfico
ggplot(medias_letras, aes(x = tipo_fertilizante, y = emmean)) +
  
  # Barras
  geom_col(
    width = 0.40,
    fill = "#9ACD32",
    color = "black",
    linewidth = 0.4
  ) +
  
  # Barras de error (Intervalos de confianza)
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.08,
    linewidth = 0.8
  ) +
  
  # Línea conectora pulida (estilo spline)
  geom_smooth(
    aes(group = 1), 
    method = "loess", 
    se = FALSE, 
    color = "#9ACD32", 
    linetype = "dashed", 
    linewidth = 1,
    alpha = 0.6
  ) +
  
  # Puntos de anclaje para la línea
  geom_point(
    color = "#556B2F", 
    size = 2.5
  ) +
  
  # Etiquetas de las medias (los números)
  geom_label(
    aes(label = sprintf("%.2f", emmean)),
    fill = "white",
    color = "black",
    linewidth = 0.25,
    size = 4,
    fontface = "bold",
    vjust = 1.2 # Ajustado hacia abajo para dar espacio a las letras arriba
  ) +
  
  # *** NUEVO: Letras de Significancia Estadísticas ***
  geom_text(
    aes(y = upper.CL + 0.3, label = .group), # Ubicadas justo encima de la barra de error
    size = 6,
    fontface = "bold",
    color = "black" # Puedes cambiarlo a "red" si quieres que resalten más
  ) +
  
  # Etiquetas de los ejes
  labs(
    x = "Tratamientos (Fertilizantes)",
    y = "Altura promedio de la planta (cm)"
  ) +
  
  # Expandir el límite superior para que las letras no se corten
  expand_limits(y = max(medias_letras$upper.CL) + 1.5) +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text = element_text(
      size = 12,
      color = "black"
    ),
    axis.line = element_line(
      linewidth = 0.8
    ),
    axis.ticks = element_line(
      linewidth = 0.8
    ),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

# 3. Guardar el gráfico
ggsave(
  "altura_lentejas.jpg",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 300
)



```{r}
#Análisis de varianza7

library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)

# 1. Calcular medias y letras SEPARADAS POR DÍA
mc_dias <- emmeans(modelo, ~ tipo_fertilizante | dia)
medias_dias_letras <- cld(mc_dias, adjust = "tukey", Letters = letters) # R ajustará a Sidak automáticamente

# Convertir a data.frame y limpiar espacios
medias_dias_letras <- as.data.frame(medias_dias_letras)
medias_dias_letras$.group <- trimws(medias_dias_letras$.group)

# 2. Construir el gráfico con paneles por día
ggplot(medias_dias_letras, aes(x = tipo_fertilizante, y = emmean)) +
  
  # Barras
  geom_col(
    width = 0.50,
    fill = "purple",
    color = "black",
    linewidth = 0.4
  ) +
  
  # Barras de error
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.8
  ) +
  
  # Etiquetas de las medias (los números)
  geom_label(
    aes(label = sprintf("%.2f", emmean)),
    fill = "white",
    color = "black",
    linewidth = 0.25,
    size = 3.5,
    fontface = "bold",
    vjust = 1.2
  ) +
  
  # Letras de Significancia
  geom_text(
    aes(y = upper.CL + 0.5, label = .group), 
    size = 5,
    fontface = "bold",
    color = "black" 
  ) +
  
  # *** LA MAGIA: Dividir el gráfico por días ***
  facet_wrap(~ dia, labeller = label_both) +
  
  # Etiquetas
  labs(
    x = "Tratamiento (Fertilizantes)",
    y = "Altura promedio de la planta (cm)"
  ) +
  
  # Ajustar el límite superior
  expand_limits(y = max(medias_dias_letras$upper.CL) + 2) +
  
  theme_classic(base_size = 14) +
  
  # Ajustes de diseño
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8),
    
    # Diseño de las cajas de los títulos de cada día (Día 7, Día 14, etc.)
    strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 12),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# 3. Guardar el gráfico
ggsave(
  "altura_lentejas_po_dia.jpg",
  width = 24, # Lo hice un poco más ancho (24cm) para que quepan bien los 3 gráficos
  height = 12,
  units = "cm",
  dpi = 300
)
#por dia
library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)

# --- PASO PREVIO REQUERIDO ---
dat$block <- as.factor(dat$block)
dat$dia <- as.factor(dat$dia) # Asegura que R separe los días 7, 14 y 21

# Volver a correr el modelo con los factores corregidos
modelo <- aov(alt_plant ~ block + tipo_fertilizante * dia, data = dat)

# 1. Calcular medias y letras SEPARADAS POR DÍA
mc_dias <- emmeans(modelo, ~ tipo_fertilizante | dia)
medias_dias_letras <- cld(mc_dias, adjust = "tukey", Letters = letters)

# Convertir a data.frame y limpiar espacios
medias_dias_letras <- as.data.frame(medias_dias_letras)
medias_dias_letras$.group <- trimws(medias_dias_letras$.group)


# 2. Construir el gráfico con paneles por día
ggplot(medias_dias_letras, aes(x = tipo_fertilizante, y = emmean)) +
  
  # Barras
  geom_col(
    width = 0.50,
    fill = "purple",
    color = "black",
    linewidth = 0.4
  ) +
  
  # Barras de error
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.8
  ) +
  
  # Etiquetas de las medias (los números)
  geom_label(
    aes(label = sprintf("%.2f", emmean)),
    fill = "white",
    color = "black",
    size = 3.5,
    fontface = "bold",
    vjust = 1.2
  ) +
  
  # Letras de Significancia (dinámicas para que no se corten arriba)
  geom_text(
    aes(y = upper.CL + (max(upper.CL) * 0.05), label = .group), 
    size = 5,
    fontface = "bold",
    color = "black" 
  ) +
  
  # *** LA MAGIA: Dividir por días liberando las escalas del eje Y si es necesario ***
  # Nota: Si quieres que cada día tenga su propia escala de altura, usa scales = "free_y"
  facet_wrap(~ dia, labeller = label_both, scales = "free_y") +
  
  # Etiquetas
  labs(
    x = "Tipo de Fertilizante)",
    y = "Altura promedio de la planta (cm)"
  ) +
  
  theme_classic(base_size = 14) +
  
  # Ajustes de diseño
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8),
    
    # Diseño de las cajas de los títulos de cada día
    strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 12),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# 3. Guardar el gráfico
ggsave(
  "altura_lentejas_por_dia.jpg",
  width = 24, 
  height = 12,
  units = "cm",
  dpi = 300
)


#numero de hojas



library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)

# --- PASO PREVIO REQUERIDO ---
dat$block <- as.factor(dat$block)
dat$dia <- as.factor(dat$dia) # Asegura que R separe los días 7, 14 y 21

# Volver a correr el modelo con los factores corregidos
modelo <- aov(num_hojas ~ block + tipo_fertilizante * dia, data = dat)

# 1. Calcular medias y letras SEPARADAS POR DÍA
mc_dias <- emmeans(modelo, ~ tipo_fertilizante | dia)
medias_dias_letras <- cld(mc_dias, adjust = "tukey", Letters = letters)

# Convertir a data.frame y limpiar espacios
medias_dias_letras <- as.data.frame(medias_dias_letras)
medias_dias_letras$.group <- trimws(medias_dias_letras$.group)


# 2. Construir el gráfico con paneles por día
ggplot(medias_dias_letras, aes(x = tipo_fertilizante, y = emmean)) +
  
  # Barras
  geom_col(
    width = 0.50,
    fill = "orange",
    color = "black",
    linewidth = 0.4
  ) +
  
  # Barras de error
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.8
  ) +
  
  # Etiquetas de las medias (los números)
  geom_label(
    aes(label = sprintf("%.2f", emmean)),
    fill = "white",
    color = "black",
    size = 3.5,
    fontface = "bold",
    vjust = 1.2
  ) +
  
  # Letras de Significancia (dinámicas para que no se corten arriba)
  geom_text(
    aes(y = upper.CL + (max(upper.CL) * 0.05), label = .group), 
    size = 5,
    fontface = "bold",
    color = "black" 
  ) +
  
  # *** LA MAGIA: Dividir por días liberando las escalas del eje Y si es necesario ***
  # Nota: Si quieres que cada día tenga su propia escala de altura, usa scales = "free_y"
  facet_wrap(~ dia, labeller = label_both, scales = "free_y") +
  
  # Etiquetas
  labs(
    x = "Tratamiento (Fertilizantes)",
    y = "Numero promedio de hojas de la planta (num)"
  ) +
  
  theme_classic(base_size = 14) +
  
  # Ajustes de diseño
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11, color = "black"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8),
    
    # Diseño de las cajas de los títulos de cada día
    strip.background = element_rect(fill = "lightgray", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 12),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# 3. Guardar el gráfico
ggsave(
  "numero de hojas.jpg",
  width = 24, 
  height = 12,
  units = "cm",
  dpi = 300
)
