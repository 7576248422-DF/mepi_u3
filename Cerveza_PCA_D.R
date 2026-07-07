{r}
#| label: setup

source('https://inkaverse.com/setup.r')


# Data import

{r}
gs <- "https://docs.google.com/spreadsheets/d/1QtEcMseC-JX_yFDp21c0pRmPcRQOlQeUO9Y6TB2XXSU/edit?gid=0#gid=0" %>% 
  as_sheets_id()

fb <- gs %>% 
  range_read(ss = .,sheet = "fb")


# Research Objectives

> https://docs.google.com/spreadsheets/d/1u6xgTT8jDFkHSMkK1iRrNVR-1iOINQidbEiaDCu2Nqw/edit?gid=504037395#gid=504037395

## RO 1

{r}



## RO 2

{r}



# Multivariate

```{r}
#| eval: false

library(FactoMineR)
library(factoextra)

# str(dtx)
dtx <- fb %>%
  dplyr::select(!c("repeticiones")) %>%
  # Desempaquetar y forzar a factor las variables suplementarias
  dplyr::mutate(dplyr::across(1, ~ as.factor(unlist(.)))) %>%
  # Desempaquetar y forzar a numérico el resto de variables
  dplyr::mutate(dplyr::across(2:ncol(.), ~ as.numeric(unlist(.))))

# str(dtx) <- (Opcional) Puedes descomentar esta línea para verificar que ya no haya "List"

mv <- dtx %>%
  as.data.frame() %>%
  PCA(scale.unit = T, graph = F, quali.sup = 1)

var <- mv %>% 
  plot.PCA(choix = c("var"), cex=0.7)

cos <- mv %>% 
  fviz_pca_var(col.var = "cos2"
               , gradient.cols = c("black", "purple", "green")
               , repel = TRUE
               , labelsize = 4)

ind <- mv %>% 
  plot.PCA(choix = c("ind")
           , cex = 0.7
           , label = "ind"
           , invisible = "quali"
           , habillage="1"
           , col.hab = rainbow(18)
  )
