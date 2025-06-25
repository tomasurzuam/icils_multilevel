# ---- Procesamiento base ----

rm(list=ls())     
options(scipen=999)

# 1. Cargar librerías y bbdd 

pacman::p_load(lme4, dplyr, readr, haven, sjlabelled, psych, purrr, tidyr,
               haven, stargazer, texreg, dplyr, car, tibble, readxl)

archivos_bsg <- list.files(
  path = "input/raw_data", # Directorio actual (cambia si es necesario)
  pattern = "^BSG.*\\.Rdata$",
  full.names = TRUE
)

# Cargar todos los archivos encontrados
for (archivo in archivos_bsg) {
  load(archivo)
  cat("Cargado:", archivo, "\n")
}

# así hasta cargar todas

# Unir bases de datos

icils_2023 <- rbind(BSGAUTI3, BSGAZEI3, BSGBFLI3, BSGBIHI3, BSGCHLI3, BSGCYPI3, BSGCZEI3, BSGDEUI3, 
                    BSGDNKI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3,
                    BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
                    BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
                    BSGURYI3, BSGUSAI3, BSGXKXI3)

rm(BSGAUTI3, BSGAZEI3, BSGBFLI3, BSGBIHI3, BSGCHLI3, BSGCYPI3, BSGCZEI3, BSGDEUI3, 
   BSGDNKI3, BSGESPI3, BSGFINI3, BSGFRAI3, BSGGRCI3, BSGHRVI3, BSGHUNI3, BSGDNWI3,
   BSGITAI3, BSGKAZI3, BSGKORI3, BSGLUXI3, BSGLVAI3, BSGMLTI3, BSGNLDI3, BSGNORI3, 
   BSGOMNI3, BSGPRTI3, BSGROUI3, BSGSRBI3, BSGSVKI3, BSGSVNI3, BSGSWEI3, BSGTWNI3,
   BSGURYI3, BSGUSAI3, BSGXKXI3)

# 2. Seleccionar variables nivel 1

icils_23_proc <- icils_2023 %>%
  select( CNTRY, IDSCHOOL, IISB, S_SEX, S_LRNINTS, S_GENCLASS, PV1CIL, 
          S_IMMBGR, IS3G09, IS3G13, IS3G14)

# 3. Procesamiento de variables 

# 3.1. Recodificación de casos perdidos

icils_23_proc$S_SEX <- recode(icils_23_proc$S_SEX, "c(8, 9)=NA")
icils_23_proc$S_GENCLASS <- recode(icils_23_proc$S_GENCLASS, "c(998, 999)=NA")
icils_23_proc$PV1CIL <- recode(icils_23_proc$PV1CIL, "c(998, 999)=NA")
icils_23_proc$S_LRNINTS <- recode(icils_23_proc$S_LRNINTS, "c(998, 999)=NA")
icils_23_proc$IISB <- recode(icils_23_proc$IISB, "c(998, 999)=NA")
icils_23_proc$S_IMMBGR <- recode(icils_23_proc$S_IMMBGR, "c(8, 9)=NA")
icils_23_proc$IS3G09 <- recode(icils_23_proc$IS3G09, "c(1)=1; c(2, 3, 4, 5)=2; c(8, 9)=NA")
icils_23_proc$IS3G13 <- recode(icils_23_proc$IS3G13, "c(1)=1; c(2, 3, 4, 5)=2; c(8, 9)=NA")
icils_23_proc$IS3G14 <- recode(icils_23_proc$IS3G14, "c(8, 9)=NA")

# 3.2. Renombramiento de variables 

icils_23_proc <- icils_23_proc %>% rename("sexo"= S_SEX,
                                          "aprendizaje_escuela"= S_LRNINTS,
                                          "alf_digital"= PV1CIL,
                                          "autoeffgen"= S_GENCLASS,
                                          "iiseb"= IISB,
                                          "inmigrante"= S_IMMBGR, 
                                          "educ_p1"= IS3G09,
                                          "educ_p2"= IS3G13,
                                          "libros_hogar"= IS3G14
)

# ---- Procesamiento bases nivel 2 ----

pib_df <- read_csv(
  file = "input/raw_data/pib.csv",
  skip = 4,               # Saltar metadatos
  col_names = TRUE,       # Usar encabezados
  quote = "\"",
  na = c("", "NA")        # Valores faltantes
)

pib_education <- read_csv(
  file = "input/raw_data/pib_education.csv",
  skip = 4,               # Saltar metadatos
  col_names = TRUE,       # Usar encabezados
  quote = "\"",
  na = c("", "NA")        # Valores faltantes
)

gender <- read_excel("input/raw_data/gender_index.xlsx")

# 1. Selección de países 

#Cambiamos el ID de Bélgica para que calce con ICILS

pib_df$`Country Code`[pib_df$`Country Code` == "BEL"] <- "BFL"
pib_education$`Country Code`[pib_education$`Country Code` == "BEL"] <- "BFL"
gender$countryIsoCode[gender$countryIsoCode == "BEL"] <- "BFL"

# lista de países para filtrar por los países que necesitamos
countries <- c("AUT", "AZE", "BFL", "BIH",
                   "CHL", "CYP", "CZE", "DEU", "DNK", "ESP", "FIN",
                   "FRA", "GRC", "HRV", "HUN", "ITA", "KAZ", "KOR", "LUX", "LVA", 
                   "MLT", "NLD", "NOR", "OMN", "PRT", "ROU", "SRB", "SVK", "SVN",
                   "SWE", "TWN", "URY", "USA", "XKX")

pib_df <- filter(pib_df, `Country Code` %in% countries)
pib_education <- filter(pib_education, `Country Code` %in% countries)
gender <- filter(gender, countryIsoCode %in% countries)

# 2. FIltrar por año 

pib_df <- pib_df %>%
  select(`Country Code`, "2023")

pib_education_proc <- pib_education %>%
  select(`Country Code`, "2021")

# *Modificar valores de pib en educación a los más actuales en los 
# países disponibles

