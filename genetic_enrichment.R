
setwd("") #Establecer ruta de trabajo

lista_archivos <- c("Tobacco_mosaic_vires.csv","Verticillium_dahliae.csv","Bacterial_strain_SA188.csv",
                    "Blumeria_graminis.csv","Botryis_cinerea.csv","Colletotrichum_tofieldiae.csv",
                    "Fusarium_graminearum.csv","Heterodera_schachti_nematodes.csv","Heterodera_schachtii_parasitism.csv",
                    "Hyaloperonospora_arabidopsidis.csv","Microbacterium_sp.csv","Plutella_xylostella.csv",
                    "Pseudomonas_simiae.csv","Pseudomonas_syringae.csv","Rhizobium_sp.csv",
                    "Rhizoctonia_solani.csv")

lista_enriquecimiento <- list()

#Cargar archivos
for (archivo in lista_archivos) {
  lista_enriquecimiento[[archivo]] <- read.csv(archivo, header = TRUE, sep = ",", quote = "\"",
                                               dec = ".", fill = TRUE, comment.char = "")
}

#funcion
calcular_p_value <- function(df, n) {
  df$p_value <- dhyper(x = df$nGenes, m = df$Pathway.Genes, n = 27413 - df$Pathway.Genes, k = n)
  return(df)
}

valores_n <- c(55, 50, 50, 51, 50, 51, 52, 50, 52, 53, 51, 50, 50, 50, 50, 51)
resultados <- list()

#calcular el valor p para cada dataframe
for (nombre_archivo in names(lista_enriquecimiento)) {
  df <- lista_enriquecimiento[[nombre_archivo]]
  n <- valores_n[names(lista_enriquecimiento) == nombre_archivo]
  df <- calcular_p_value(df, n)
  resultados[[nombre_archivo]] <- df
}

#Decir si es significativo
for (nombre_archivo in names(resultados)) {
  df <- resultados[[nombre_archivo]]
  df$Significance <- ifelse(df$p_value < 0.05, "yes", " ")
  resultados[[nombre_archivo]] <- df
}

#Nueva ubicacion para guardar los resultados
setwd("") 

#Guardar archivos en .csv
for (nombre_archivo in names(resultados)) {
  df <- resultados[[nombre_archivo]]
  write.csv(df, paste0(nombre_archivo, ".csv"), row.names = FALSE)
}

