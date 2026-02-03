install.packages("tidyverse")
install.packages("lme4")
install.packages("lmerTest")
install.packages("sjPlot")
install.packages("MASS")
install.packages("car")
install.packages("MuMIn")
install.packages("janitor")

library(MuMIn)
library(car)
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(MASS)
library(readr)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
se <- function(x) #función para error standard
{
  y <- x[!is.na(x)]
  sqrt(var(as.vector(y))/length(y))
}

#Preparo i dati

library(readr)
df_comparato <- read_delim("DATI_CONFRONTO_PULITI.csv", delim = ";", col_names = TRUE)


#========Conversione dei tipi di colonna========================================

df_comparato <- df_comparato %>%
  mutate(
    # Converti in numerico le colonne dei tempi di reazione (RT) e l'età
    rt_determinante = as.numeric(rt_determinante),
    rt_nome = as.numeric(rt_nome),
    rt_verbo1 = as.numeric(rt_verbo1),
    rt_pronome = as.numeric(rt_pronome),
    rt_verbo2 = as.numeric(rt_verbo2),
    eta = as.numeric(eta),
    
    #Converto in fattori le variabili categoriche e gli identificatori
    Gruppo = as.factor(Gruppo),
    ID = as.factor(ID), # ID del partecipante
    `ID frase` = as.factor(`ID frase`), # ID della frase (item)
    Cond = as.factor(Cond),
    `Bias di genere` = as.factor(`Bias di genere`)
  )

#per i dislessici: Yes = corretto
#per i controlli: No = no errori, quindi corretto

df_comparato$Corretto_finale <- NA

df_comparato$Corretto_finale[df_comparato$Gruppo == "CG" & df_comparato$Corretto == "no"]  <- 1
df_comparato$Corretto_finale[df_comparato$Gruppo == "CG" & df_comparato$Corretto == "si"] <- 0

df_comparato$Corretto_finale[df_comparato$Gruppo == "DG" & df_comparato$Corretto == "yes"] <- 1
df_comparato$Corretto_finale[df_comparato$Gruppo == "DG" & df_comparato$Corretto == "no"]  <- 0
table(df_comparato$Gruppo, df_comparato$Corretto, df_comparato$Corretto_finale)

#=====================ANALISI DEMOGRAFICA=======================================

# Creo un dataframe con solo i partecipanti unici per le statistiche demografiche

partecipanti <- df_comparato %>%
  distinct(Gruppo, part, eta, ID,  id_genero, educazione, lingua, nazionalita, residenza, tiempo_total, lista) # Mantiene solo la prima riga per ogni partecipante

# Calcolo e stampo le statistiche sull'età

print("--- Statistiche Demografiche (Età) ---")
print(paste("Età media:", round(mean(partecipanti$eta, na.rm = TRUE), 2))) #età media: 24.81
print(paste("Deviazione Standard Età:", round(sd(partecipanti$eta, na.rm = TRUE), 2))) #sd: 3.73
print(paste("Età massima:", max(partecipanti$eta, na.rm = TRUE))) # max: 33
print(paste("Età minima:", min(partecipanti$eta, na.rm = TRUE)))  # min: 19

# Standardizzo la variabile 'id_genero' 

df_comparato <- df_comparato %>%
  mutate(id_genero = case_when(
    id_genero %in% c("F", "Donna", "femmina", "f", "donna cis", "donna", "Femmina",
                     "DONNA", "Femminile", "FEMMINA") ~ "Donna",
    id_genero %in% c("MASCHIO", "maschio", "Maschio", "uomo", "M", "MASCHILE", "UOMO",
                     "Maschile", "Uomo") ~ "Uomo",
    TRUE ~ "Altro" # Mette 'Altro' per ogni valore non specificato
  ))
partecipanti <- partecipanti %>%
  mutate(id_genero = case_when(
    id_genero %in% c("F", "Donna", "femmina", "f", "donna cis", "donna", "Femmina",
                     "DONNA", "Femminile", "FEMMINA") ~ "Donna",
    id_genero %in% c("MASCHIO", "maschio", "Maschio", "uomo", "M", "MASCHILE", "UOMO",
                     "Maschile", "Uomo") ~ "Uomo",
    TRUE ~ "Altro" # Mette 'Altro' per ogni valore non specificato
  ))

#Conto i partecipanti per genere
print("--- Conteggio Partecipanti per Genere ---")
print(
  df_comparato %>%
    distinct(ID, Gruppo, .keep_all = TRUE) %>%
    count(id_genero)
)

# Calcolo statistiche demografiche separate per ogni gruppo

statistiche_per_gruppo <- partecipanti %>%
  distinct(ID, Gruppo, .keep_all = TRUE) %>%
  group_by(Gruppo) %>%
  summarise(
    Numero_Partecipanti = n(),
    Eta_Media = mean(eta, na.rm = TRUE),
    Eta_DS = sd(eta, na.rm = TRUE)
  )

statistiche_per_gruppo

#Conto il genere separatamente per ogni gruppo

conteggio_genere_per_gruppo <- partecipanti %>%
  distinct(ID, Gruppo, .keep_all = TRUE) %>%
  count(Gruppo, id_genero)

conteggio_genere_per_gruppo

#T-test: per un confronto statistico sull'età

t.test(eta ~ Gruppo, data = partecipanti)

#======================PULIZIA DATI=============================================

#Salvo il dataframe originale per calcolare le percentuali

df_intero <- df_comparato
print(paste("Numero totale di prove iniziali:", nrow(df_intero))) #Prove iniziali: 176

# Rimozione delle risposte errate

df_pulito_errori <- df_intero %>%
  filter(Corretto_finale == 1) #1 lascia solo le risposte corrette

#Calcolo percentuale dati rimossi per errori

percentuale_errori <- ((nrow(df_intero) - nrow(df_pulito_errori)) / nrow(df_intero)) * 100
print(paste0("Dati rimossi per errori: ", round(percentuale_errori, 2), "%")) #32.39%


#Calcolo errori per gruppo

df_intero %>%
  group_by(Gruppo) %>%
  summarise(
    Accuratezza_Media = mean(Corretto_finale),
    Percentuale_Errore = (1 - Accuratezza_Media) * 100 
  )
#Controlli: 90.9% accuracy e 9.09% errori 
#Dislessici: 44% Accuracy e 55.7% errori

#===============================================================================

#         PULIZIA DATI (ERRORI E OUTLIERS), con metodo SD adattivo

#===============================================================================

#Prendo il dataframe con solo le risposte corrette

df_pulito_errori <- df_comparato %>%
  filter(Corretto_finale == 1)

print(paste("Numero di prove dopo la rimozione degli errori:", nrow(df_pulito_errori)))
#119 prove rimaste dopo la pulizia
#Rimozione degli outliers con il metodo della Deviazione Standard (SD)
#Per ogni partecipante (identificato da ID e Gruppo), calcolo i suoi limiti personali.

# Calcola media e sd per gruppo
library(dplyr)

# 1️⃣ Calcolo delle medie e SD per gruppo (CG / DG)
stats_group <- df_pulito_errori %>%
  group_by(Gruppo) %>%
  summarise(
    mean_rt_det_g = mean(rt_determinante, na.rm = TRUE),
    sd_rt_det_g   = sd(rt_determinante, na.rm = TRUE),
    mean_rt_nome_g = mean(rt_nome, na.rm = TRUE),
    sd_rt_nome_g   = sd(rt_nome, na.rm = TRUE),
    mean_rt_v1_g = mean(rt_verbo1, na.rm = TRUE),
    sd_rt_v1_g   = sd(rt_verbo1, na.rm = TRUE),
    mean_rt_pronome_g = mean(rt_pronome, na.rm = TRUE),
    sd_rt_pronome_g   = sd(rt_pronome, na.rm = TRUE),
    mean_rt_v2_g = mean(rt_verbo2, na.rm = TRUE),
    sd_rt_v2_g   = sd(rt_verbo2, na.rm = TRUE)
  )

# 2️⃣ Filtro adattivo
df_finale_analisi_sd <- df_pulito_errori %>%
  group_by(ID, Gruppo) %>%
  mutate(n = n()) %>%
  left_join(stats_group, by = "Gruppo") %>%
  mutate(
    mean_rt_det = if_else(n >= 8, mean(rt_determinante, na.rm = TRUE), mean_rt_det_g),
    sd_rt_det   = if_else(n >= 8, sd(rt_determinante, na.rm = TRUE), sd_rt_det_g),
    
    mean_rt_nome = if_else(n >= 8, mean(rt_nome, na.rm = TRUE), mean_rt_nome_g),
    sd_rt_nome   = if_else(n >= 8, sd(rt_nome, na.rm = TRUE), sd_rt_nome_g),
    
    mean_rt_v1 = if_else(n >= 8, mean(rt_verbo1, na.rm = TRUE), mean_rt_v1_g),
    sd_rt_v1   = if_else(n >= 8, sd(rt_verbo1, na.rm = TRUE), sd_rt_v1_g),
    
    mean_rt_pronome = if_else(n >= 8, mean(rt_pronome, na.rm = TRUE), mean_rt_pronome_g),
    sd_rt_pronome   = if_else(n >= 8, sd(rt_pronome, na.rm = TRUE), sd_rt_pronome_g),
    
    mean_rt_v2 = if_else(n >= 8, mean(rt_verbo2, na.rm = TRUE), mean_rt_v2_g),
    sd_rt_v2   = if_else(n >= 8, sd(rt_verbo2, na.rm = TRUE), sd_rt_v2_g)
  ) %>%
  mutate(
    limite_sup_rt_det = mean_rt_det + 2.5 * sd_rt_det,
    limite_inf_rt_det = mean_rt_det - 2.5 * sd_rt_det,
    limite_sup_rt_nome = mean_rt_nome + 2.5 * sd_rt_nome,
    limite_inf_rt_nome = mean_rt_nome - 2.5 * sd_rt_nome,
    limite_sup_rt_v1 = mean_rt_v1 + 2.5 * sd_rt_v1,
    limite_inf_rt_v1 = mean_rt_v1 - 2.5 * sd_rt_v1,
    limite_sup_rt_pronome = mean_rt_pronome + 2.5 * sd_rt_pronome,
    limite_inf_rt_pronome = mean_rt_pronome - 2.5 * sd_rt_pronome,
    limite_sup_rt_v2 = mean_rt_v2 + 2.5 * sd_rt_v2,
    limite_inf_rt_v2 = mean_rt_v2 - 2.5 * sd_rt_v2
  ) %>%
  ungroup() %>%
  filter(
    rt_determinante > limite_inf_rt_det & rt_determinante < limite_sup_rt_det,
    rt_nome > limite_inf_rt_nome & rt_nome < limite_sup_rt_nome,
    rt_verbo1 > limite_inf_rt_v1 & rt_verbo1 < limite_sup_rt_v1,
    rt_pronome > limite_inf_rt_pronome & rt_pronome < limite_sup_rt_pronome,
    rt_verbo2 > limite_inf_rt_v2 & rt_verbo2 < limite_sup_rt_v2
  )

#================================================
qqp(df_finale_analisi_sd$rt_nome, "norm", main = "Q-Q Plot di rt_nome (Originale)")

#Grafico 3: Densità di rt_nome (Log-Trasformato)


plot(density(log(df_finale_analisi_sd$rt_nome)), main = "Densità di rt_nome (Log)")

#Grafico 4: Q-Q Plot di rt_nome (Log-Trasformato)

qqplot_residui_nome= qqp(log(df_finale_analisi_sd$rt_nome), "norm", main = "Q-Q Plot di rt_nome (Log)")
png("qqplot_residui_nome.png")
library(car) # se usi qqp

# Apri il device PNG
png("qqplot_residui_nome.png", width = 800, height = 600)

# Crea il Q-Q plot
qqp(log(df_finale_analisi_sd$rt_nome), "norm", main = "Q-Q Plot di rt_nome (Log)")

# Chiudi il device (salva il file)
dev.off()

#Calcolo la percentuale di dati persi 
citation()
percentuale_outliers_sd <- ((nrow(df_pulito_errori) - nrow(df_finale_analisi_sd)) / nrow(df_pulito_errori)) * 100
print(paste0("Dati rimossi come outliers (metodo SD adattivo): ", round(percentuale_outliers_sd, 2), "%"))
#11.76% di dati persi

#Percentuale totale di dati rimossi dall'inizio

percentuale_totale_sd <- ((nrow(df_comparato) - nrow(df_finale_analisi_sd)) / nrow(df_comparato)) * 100
print(paste0("Percentuale totale di dati rimossi: ", round(percentuale_totale_sd, 2), "%"))
print(paste("Numero finale di prove per l'analisi:", nrow(df_finale_analisi_sd)))
#Dati rimossi dall'inizio: 40.34%
df_pulito_errori %>%
  group_by(Gruppo) %>%
  summarise(ProvePrima = n()) %>%
  left_join(
    df_finale_analisi_sd %>% group_by(Gruppo) %>% summarise(ProveDopo = n()),
    by = "Gruppo"
  ) %>%
  mutate(
    Persi = ProvePrima - ProveDopo,
    PercentualePersi = (Persi / ProvePrima) * 100
  )
df_finale_analisi_sd %>%
  group_by(ID, Gruppo) %>%
  summarise(n_prove = n()) %>%
  arrange(n_prove)

#prove iniziali: 176
#prove rimosse per errori: 32.39 ---> rimangono 119 prove
#prove rimosse per outlier: 11.76% ---> rimangono 105 prove


#Da questo momento in poi, 'df_finale_analisi_sd' è il dataframe di riferimento.
summary(df_finale_analisi_sd)
df_comparato %>%
  group_by(Gruppo) %>%
  summarise(
    numero_iniziale = n()
  )
df_pulito_errori %>%
  group_by(Gruppo) %>%
  summarise(n_post_errori = n())
df_finale_analisi_sd %>%
  group_by(Gruppo) %>%
  summarise(n_finale = n())
df_pulito_errori %>%
  group_by(ID, Gruppo) %>%
  summarise(n = n()) %>%
  arrange(Gruppo, ID) -> counts_per_part

print(counts_per_part)

#===============================================================================

#                 IMPOSTAZIONE DEI CONTRASTI

#===============================================================================
#Definisco come le variabili categoriche verranno confrontate nel modello.

# --- IMPOSTAZIONE ESPLICITA DEL GRUPPO DI RIFERIMENTO ---

contrasts(df_finale_analisi_sd$Cond)
contrasts(df_finale_analisi_sd$`Bias di genere`)
contrasts(df_finale_analisi_sd$Gruppo)
df_finale_analisi_sd$Gruppo <- relevel(factor(df_finale_analisi_sd$Gruppo), ref = "CG")
contrasts(df_finale_analisi_sd$Gruppo) <- contr.treatment(2)

#Imposto i "contrasti di somma scalati": confronta ogni livello con la media generale,
#trattando le due condizioni (es. g_fem vs g_masc) in modo paritario.

#contrasts(df_finale_analisi_sd$Gruppo) <- contr.sdif(2)
contrasts(df_finale_analisi_sd$Gruppo) <- contr.treatment(2)
contrasts(df_finale_analisi_sd$Cond) <- contr.sdif(2)
contrasts(df_finale_analisi_sd$`Bias di genere`) <- contr.sdif(2)


#Verifico che i nuovi contrasti siano stati applicati correttamente

contrasts(df_finale_analisi_sd$Cond)
contrasts(df_finale_analisi_sd$`Bias di genere`)
contrasts(df_finale_analisi_sd$Gruppo)


# GRAFICO 1: RT MEDI LUNGO LA FRASE

se <- function(x) {
  y <- x[!is.na(x)]
  sqrt(var(as.vector(y)) / length(y))
}


#CREAZIONE DELLA CONDIZIONE "INCROCIATA"

df_grafico1 <- df_finale_analisi_sd %>%
  mutate(
    condizione_incrociata = case_when(
      Cond == "g_fem" & `Bias di genere` == "masc" ~ "Bias Masc - Morf Fem",
      Cond == "g_masc" & `Bias di genere` == "fem" ~ "Bias Fem - Morf Masc",
      Cond == "g_fem" & `Bias di genere` == "fem" ~ "Bias Fem - Morf Fem",
      Cond == "g_masc" & `Bias di genere` == "masc"~ "Bias Masc - Morf Masc"
    )
  )

#Imposto l'ordine dei livelli per la legenda del grafico

df_grafico1$condizione_incrociata <- factor(df_grafico1$condizione_incrociata,
                                            levels = c(
                                              "Bias Masc - Morf Fem",
                                              "Bias Fem - Morf Masc",
                                              "Bias Fem - Morf Fem",
                                              "Bias Masc - Morf Masc"
                                            )
)


#CALCOLO DELLE MEDIE E DELL'ERRORE STANDARD (SE)

#Raggruppo PER GRUPPO e per la nuova condizione.

stat_mean <- df_grafico1 %>%
  group_by(Gruppo, condizione_incrociata) %>%
  summarize(
    determinante = mean(rt_determinante, na.rm = TRUE),
    nome = mean(rt_nome, na.rm = TRUE),
    verbo1 = mean(rt_verbo1, na.rm = TRUE),
    pronome = mean(rt_pronome, na.rm = TRUE),
    verbo2 = mean(rt_verbo2, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(determinante, nome, verbo1, pronome, verbo2), names_to = "Regione", values_to = "M")

stat_se <- df_grafico1 %>%
  group_by(Gruppo, condizione_incrociata) %>%
  summarize(
    determinante = se(rt_determinante),
    nome = se(rt_nome),
    verbo1 = se(rt_verbo1),
    pronome = se(rt_pronome),
    verbo2 = se(rt_verbo2)
  ) %>%
  pivot_longer(cols = c(determinante, nome, verbo1, pronome, verbo2), names_to = "Regione", values_to = "SE")

#Unisco medie e SE

stat_grafico_finale <- merge(stat_mean, stat_se)
stat_grafico_finale$Regione <- factor(stat_grafico_finale$Regione, levels = c("determinante", "nome", "verbo1", "pronome", "verbo2"))


#CREAZIONE DEL GRAFICO

ggplot(stat_grafico_finale) +
  aes(x = Regione, y = M, color = condizione_incrociata, group = condizione_incrociata) +
  geom_line(aes(linetype = condizione_incrociata), linewidth = 1) +
  geom_point(aes(shape = condizione_incrociata), size = 3) +
  geom_errorbar(aes(max = M + SE, min = M - SE), width = 0.1) +
  facet_wrap(~ Gruppo) + 
  labs(
    title = "RT Medi lungo le Regioni della Frase",
    subtitle = "Confronto tra CG e DG",
    x = "Regione della Frase",
    y = "Media RT (ms)",
    color = "Condizione",
    shape = "Condizione",
    linetype = "Condizione"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14)

#Salvo il grafico
ggsave("Figura_RT_per_Regione_e_Gruppo.png", width = 12, height = 7)


#CALCOLO DELLE MEDIE E DELL'ERRORE STANDARD (SE)
#Raggruppo PER GRUPPO e per condizione: focus solo su determinante e nome.

stat_mean_2 <- df_grafico1 %>% 
  group_by(Gruppo, condizione_incrociata) %>%
  summarize(
    determinante = mean(rt_determinante, na.rm = TRUE),
    nome = mean(rt_nome, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(determinante, nome), names_to = "Regione", values_to = "M")

stat_se_2 <- df_grafico1 %>%
  group_by(Gruppo, condizione_incrociata) %>%
  summarize(
    determinante = se(rt_determinante),
    nome = se(rt_nome)
  ) %>%
  pivot_longer(cols = c(determinante, nome), names_to = "Regione", values_to = "SE")

#Unisco medie e SE

stat_grafico_finale_2 <- merge(stat_mean_2, stat_se_2)
stat_grafico_finale_2$Regione <- factor(stat_grafico_finale_2$Regione, levels = c("determinante", "nome"))


#CREAZIONE DEL GRAFICO

ggplot(stat_grafico_finale_2) +
  aes(x = Regione, y = M, color = condizione_incrociata, group = condizione_incrociata) +
  geom_line(aes(linetype = condizione_incrociata), linewidth = 1) +
  geom_point(aes(shape = condizione_incrociata), size = 4) +
  geom_errorbar(aes(max = M + SE, min = M - SE), width = 0.1) +
  facet_wrap(~ Gruppo) + 
labs(
  title = "Focus su RT di Determinante e Nome",
  subtitle = "Confronto tra CG e DG",
  x = "",
  y = "Media RT (ms)",
  color = "Condizione",
  shape = "Condizione",
  linetype = "Condizione"
) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14)

# Salvo il grafico
ggsave("Figura_Focus_Determinante_Nome.png", width = 12, height = 7)



# Raggruppo PER GRUPPO e condizione: focus solo su nome e pronome

stat_mean_3 <- df_grafico1 %>%
  group_by(Gruppo, condizione_incrociata) %>%
  summarize(
    nome = mean(rt_nome, na.rm = TRUE),
    pronome = mean(rt_pronome, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(nome, pronome), names_to = "Regione", values_to = "M")

stat_se_3 <- df_grafico1 %>%
  group_by(Gruppo, condizione_incrociata) %>%
  summarize(
    nome = se(rt_nome),
    pronome = se(rt_pronome)
  ) %>%
  pivot_longer(cols = c(nome, pronome), names_to = "Regione", values_to = "SE")

#Unisco medie e SE
stat_grafico_finale_3 <- merge(stat_mean_3, stat_se_3)
stat_grafico_finale_3$Regione <- factor(stat_grafico_finale_3$Regione, levels = c("nome", "pronome"))


#CREAZIONE DEL GRAFICO

ggplot(stat_grafico_finale_3) +
  aes(x = Regione, y = M, color = condizione_incrociata, group = condizione_incrociata) +
  geom_line(aes(linetype = condizione_incrociata), linewidth = 1.2) + 
  geom_point(aes(shape = condizione_incrociata), size = 4) +
  geom_errorbar(aes(max = M + SE, min = M - SE), width = 0.1) +
  facet_wrap(~ Gruppo) + 
  # -------------------------
labs(
  title = "Costo di Integrazione: RT su Nome e Pronome",
  subtitle = "Confronto tra CG e DG",
  x = "Regione Critica",
  y = "Media RT (ms)",
  color = "Condizione",
  shape = "Condizione",
  linetype = "Condizione"
) +
  scale_x_discrete(labels = c("nome" = "Nome", "pronome" = "Pronome (tutti/tutte)")) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14)

#Salvo il grafico

ggsave("Figura_Costo_Integrazione_Nome_Pronome.png", width = 12, height = 7)


#Calcolo la MEDIA di RT_NOME PER ogni PAROLA TARGET, separatamente per Gruppo e per Condizione.

df_plot_parole <- df_finale_analisi_sd %>%
  group_by(Gruppo, `Parola target`, Cond) %>%
  summarise(rt_mean = mean(rt_nome, na.rm = TRUE), .groups = 'drop')

#CREAO IL GRAFICO A BARRE

ggplot(df_plot_parole, aes(x = `Parola target`, y = rt_mean, fill = Cond)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Gruppo, scales = "free_x") +
theme_minimal(base_size = 12) +
  labs(
    title = "RT del Nome per ogni Parola Target",
    subtitle = "Confronto tra CG e DG",
    x = "Parola Target (Ruolo)",
    y = "Media RT Nome (ms)",
    fill = "Genere Grammaticale"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 9))

#SalvO il grafico
ggsave("Figura_RT_per_Parola_Target.png", width = 16, height = 9)

#Calcolo la media di RT_NOME PER ogni combinazione di GRUPPO, BIAS, PAROLA TARGET e CONDIZIONE

df_plot_dettaglio <- df_finale_analisi_sd %>%
  group_by(Gruppo, `Bias di genere`, `Parola target`, Cond) %>%
  summarise(rt_mean = mean(rt_nome, na.rm = TRUE), .groups = 'drop')

#CREO IL GRAFICO A GRIGLIA

ggplot(df_plot_dettaglio, aes(x = `Parola target`, y = rt_mean, fill = Cond)) +
  geom_col(position = position_dodge()) +
  facet_grid(Gruppo ~ `Bias di genere`, scales = "free_x", space = "free_x") +
theme_minimal(base_size = 12) +
  labs(
    title = "RT del Nome per Parola Target, Bias e Genere Grammaticale",
    subtitle = "Dati suddivisi per Gruppo (righe) e Bias di Genere (colonne)",
    x = "Parola Target (Ruolo)",
    y = "Media RT Nome (ms)",
    fill = "Genere Grammaticale"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 9))

#Salvo il grafico
ggsave("Figura_RT_Analisi_Dettagliata_Griglia.png", width = 16, height = 10)


#===============================================================================

#             PREPARAZIONE FINALE PER L'ANALISI STATISTICA

#===============================================================================

library(tidyverse)
library(car)

#Grafico 1: Densità di rt_nome (Originale)

plot(density(df_finale_analisi_sd$rt_nome), main = "Densità di rt_nome (Originale)")

#Grafico 2: Q-Q Plot di rt_nome (Originale)

qqp(df_finale_analisi_sd$rt_nome, "norm", main = "Q-Q Plot di rt_nome (Originale)")

#Grafico 3: Densità di rt_nome (Log-Trasformato)

plot(density(log(df_finale_analisi_sd$rt_nome)), main = "Densità di rt_nome (Log)")

#Grafico 4: Q-Q Plot di rt_nome (Log-Trasformato)

qqplot_residui_nome= qqp(log(df_finale_analisi_sd$rt_nome), "norm", main = "Q-Q Plot di rt_nome (Log)")
png("qqplot_residui_nome.png")
library(car) # se usi qqp

# Apri il device PNG
png("qqplot_residui_nome.png", width = 800, height = 600)

# Crea il Q-Q plot
qqp(log(df_finale_analisi_sd$rt_nome), "norm", main = "Q-Q Plot di rt_nome (Log)")

# Chiudi il device (salva il file)
dev.off()
#Faccio lo stesso per il pronome

#Grafico 5: Densità di rt_pronome (Originale)

plot(density(df_finale_analisi_sd$rt_pronome), main = "Densità di rt_pronome (Originale)")

#Grafico 6: Q-Q Plot di rt_pronome (Originale)

qqp(df_finale_analisi_sd$rt_pronome, "norm", main = "Q-Q Plot di rt_pronome (Originale)")

#Grafico 7: Densità di rt_pronome (Log-Trasformato)

plot(density(log(df_finale_analisi_sd$rt_pronome)), main = "Densità di rt_pronome (Log)")

#Grafico 8: Q-Q Plot di rt_pronome (Log-Trasformato)

qqp(log(df_finale_analisi_sd$rt_pronome), "norm", main = "Q-Q Plot di rt_pronome (Log)")

#===============================================================================

#                           ANALISI DI RT_NOME 

#===============================================================================

library(lme4)
library(lmerTest)
library(sjPlot)
library(MuMIn)

se <- function(x) { y <- x[!is.na(x)]; sqrt(var(as.vector(y)) / length(y)) }

statistiche_finali <- df_finale_analisi_sd %>%
  group_by(Gruppo, Cond, `Bias di genere`) %>%
  summarize(M = mean(rt_nome, na.rm = TRUE), SE = se(rt_nome))

ggplot(statistiche_finali) +
  aes(x = `Bias di genere`, y = M, color = Cond, group = Cond) +
  geom_line(aes(linetype = Cond), linewidth = 1.2) +
  geom_point(aes(shape = Cond), size = 4) +
  geom_errorbar(aes(max = M + SE, min = M - SE), width = 0.1) +
  facet_wrap(~ Gruppo) +
  labs(title = "Interazione tra Bias e Genere Grammaticale su RT del Nome",
       subtitle = "Confronto tra CG e DG",
       x = "Bias di Genere", y = "Media RT Nome (ms)",
       color = "Genere Grammaticale", shape = "Genere Grammaticale", linetype = "Genere Grammaticale") +
  theme_light(base_size = 14) +
  scale_color_manual(values = c("g_fem" = "#E81B88", "g_masc" = "#56638A"))

ggsave("Figura_Interazione_Finale_Nome.png", width = 12, height = 7)


#COSTRUISCO I MODELLI CONCORRENTI PER IL NOME
# Tutti i modelli usano log(rt_nome) e solo (1|ID) come effetto casuale.
#ID frase aveva varianza 0


#Modello 0 (Nullo): Solo l'intercetta e l'effetto casuale del partecipante.

mod_nome_0 <- lmer(
  log(rt_nome) ~ 1 + (1 | part) + (1 | `ID frase`),
  data = df_finale_analisi_sd,
  REML = FALSE
)

summary(mod_nome_0)

#Modello 1 (Effetti Principali): Aggiungo gli effetti separati di Gruppo, Cond e Bias.

mod_nome_1 <- lmer(
  log(rt_nome) ~ Gruppo + Cond + `Bias di genere` + (1 | part)+ (1 | `ID frase`),
  data = df_finale_analisi_sd,
  REML = FALSE
)
summary(mod_nome_1)

#Modello 2 (Interazioni a 2 vie): Aggiungo le interazioni.

mod_nome_2 <- lmer(
  log(rt_nome) ~ (Gruppo + Cond + `Bias di genere`)^2 + (1 | part)+ (1 | `ID frase`),
  data = df_finale_analisi_sd,
  REML = FALSE
)

summary(mod_nome_2)

#Modello 3 (Interazione a 3 vie): modello più complesso.

mod_nome_3 <- lmer(
  log(rt_nome) ~ Gruppo * Cond * `Bias di genere` + (1 | part) + (1 | `ID frase`),
  data = df_finale_analisi_sd,
  REML = FALSE
)

summary(mod_nome_3)

#CONFRONTO DEI MODELLI
#Uso anova() per vedere se ogni modello più complesso è significativamente migliore del precedente.
#Uso AIC: il modello con il valore minore è il migliore

anova(mod_nome_0, mod_nome_1, mod_nome_2, mod_nome_3)
AIC(mod_nome_0, mod_nome_1, mod_nome_2, mod_nome_3)

summary(mod_nome_1)
install.packages("texreg")
library(texreg)
texreg(mod_nome_1, 
       file = "Tabella_Interazione_Nome.tex",
       custom.model.names = "Risultati LMM su log(rt_nome)",
       caption = "Risultati del modello lineare misto (LMM) per i RT log-trasformati sul Nome.",
       label = "tab:risultati_nome",
       digits = 3, # Imposta il numero di decimali
       stars = c(0.001, 0.01, 0.05) # Definisce le soglie per gli asterischi
)

tab_model(mod_nome_1, show.se = TRUE, show.stat = TRUE, title = "Risultati LMM su log(rt_nome)")

#===============================================================================

#                       ANALISI DI RT_PRONOME

#===============================================================================

#Controllo della Normalità
plot(density(df_finale_analisi_sd$rt_pronome), main = "Densità di rt_pronome (Originale)")
qqp(df_finale_analisi_sd$rt_pronome, "norm", main = "Q-Q Plot di rt_pronome (Originale)")
plot(density(log(df_finale_analisi_sd$rt_pronome)), main = "Densità di rt_pronome (Log)")
qqp(log(df_finale_analisi_sd$rt_pronome), "norm", main = "Q-Q Plot di rt_pronome (Log)")

#Grafico Riassuntivo dell'Interazione sul Pronome

statistiche_pronome <- df_finale_analisi_sd %>%
  group_by(Gruppo, Cond, `Bias di genere`) %>%
  summarize(
    M = mean(rt_pronome, na.rm = TRUE),
    SE = se(rt_pronome)
  )

ggplot(statistiche_pronome) +
  aes(x = `Bias di genere`, y = M, color = Cond, group = Cond) +
  geom_line(aes(linetype = Cond), linewidth = 1.2) +
  geom_point(aes(shape = Cond), size = 4) +
  geom_errorbar(aes(max = M + SE, min = M - SE), width = 0.1) +
  facet_wrap(~ Gruppo) +
  labs(
    title = "Interazione tra Bias e Genere Grammaticale su RT del Pronome",
    subtitle = "Confronto tra Gruppo Controllo e Dislessico",
    x = "Bias di Genere", y = "Media RT Pronome (ms)",
    color = "Genere Grammaticale", shape = "Genere Grammaticale", linetype = "Genere Grammaticale"
  ) +
  theme_light(base_size = 14) +
  scale_color_manual(values = c("g_fem" = "#E81B88", "g_masc" = "#56638A"))

ggsave("Figura_Interazione_Finale_Pronome.png", width = 12, height = 7)

#COSTRUISCO I MODELLI: anche qui ID frase aveva varianza = 0, quindi non usata

#Modello 0 (Nullo)

mod_pron_0 <- lmer(log(rt_pronome) ~ 1 + (1|part)+ (1 | `ID frase`),
                   data = df_finale_analisi_sd, REML = FALSE)

summary(mod_pron_0)

#Modello 1: effetti principali

mod_pron_1 <- lmer(log(rt_pronome) ~ Gruppo + Cond + `Bias di genere` + (1|part) + (1 | `ID frase`),
                   data = df_finale_analisi_sd, REML = FALSE)

summary(mod_pron_1)

#Modello 2: interazioni a 2 vie

mod_pron_2 <- lmer(log(rt_pronome) ~ (Gruppo + Cond + `Bias di genere`)^2 + (1|part) + (1 | `ID frase`),
                   data = df_finale_analisi_sd, REML = FALSE)

summary(mod_pron_2)

#Modello 3: interazione a 3 vie

mod_pron_3 <- lmer(log(rt_pronome) ~ Gruppo * Cond * `Bias di genere` + (1|part) + (1 | `ID frase`),
                   data = df_finale_analisi_sd, REML = FALSE)

summary(mod_pron_3)

#CONFRONTO DEI MODELLI

anova(mod_pron_0, mod_pron_1, mod_pron_2, mod_pron_3)

AIC(mod_pron_0, mod_pron_1, mod_pron_2, mod_pron_3)

summary(mod_pron_0)

tab_model(mod_pron_0, show.se = TRUE, show.stat = TRUE, title = "Risultati LMM (Interazioni a 2 vie) su log(rt_pronome)")

#===============================================================================

#                   ANALISI DELL'ACCURATEZZA

#===============================================================================

acc_summary <- df_comparato %>%
  group_by(Gruppo, Cond, `Bias di genere`) %>%
  summarise(
    accuratezza = mean(Corretto_finale),
    sd = sd(Corretto_finale),
    n = n(),
    se = sd / sqrt(n)
  )

#Grafico Accuracy

ggplot(acc_summary, aes(x = Cond, y = accuratezza, fill = `Bias di genere`)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = accuratezza - se, ymax = accuratezza + se),
                position = position_dodge(width = 0.8), width = 0.2) +
  facet_wrap(~ Gruppo) +
  labs(
    title = "Accuratezza per condizione e bias di genere",
    x = "Condizione",
    y = "Proporzione di risposte corrette"
  ) +
  theme_minimal(base_size = 14)

#Salvo il grafico
ggsave("Figura_Accuracy.png", width = 12, height = 7)

errori_per_part <- df_comparato %>%
  group_by(part, Gruppo) %>%  # puoi aggiungere anche Cond o Bias se ti interessa il dettaglio
  summarise(
    n_prove = n(),
    n_corrette = sum(Corretto_finale == 1),
    n_errori = sum(Corretto_finale == 0),
    accuratezza = mean(Corretto_finale) * 100
  ) %>%
  arrange(Gruppo, desc(n_errori))  # ordina per gruppo e numero di errori

# Visualizza la tabella
print(errori_per_part)

#Modelli per l'analisi

#Modello nullo

mod_acc_0 <- glmer(Corretto_finale ~ 1 + (1 | part),
                   data = df_comparato, family = binomial)

summary(mod_acc_0)

#Modello 1

mod_acc_1 <- glmer(Corretto_finale ~ Gruppo + (1 | part),
                   data = df_comparato, family = binomial)

summary(mod_acc_1)

#Modello 2: interazione a 2 vie

mod_acc_2 <- glmer(Corretto_finale ~ Gruppo * Cond + (1 | part),
                   data = df_comparato, family = binomial)

summary(mod_acc_2)

#Modello 3: interazioni a 3 vie

mod_acc_3 <- glmer(Corretto_finale ~ Gruppo * Cond * `Bias di genere` + (1 | part),
                   data = df_comparato, family = binomial)

summary(mod_acc_3)

#CONFRONTO TRA MODELLI

anova(mod_acc_0, mod_acc_1, mod_acc_2, mod_acc_3, test = "Chisq")
AIC(mod_acc_0, mod_acc_1, mod_acc_2, mod_acc_3)

summary(mod_acc_1)


#========================ANALISI CR_PR===================================

df_dislessici <- read_delim("DATI_dislessici_PULITI.csv", delim = ";", col_names = TRUE)

df_dislessici <- df_dislessici %>%
  mutate(
    # Converti in numerico le colonne dei tempi di reazione (RT) e l'età
    rt_determinante = as.numeric(rt_determinante),
    rt_nome = as.numeric(rt_nome),
    rt_verbo1 = as.numeric(rt_verbo1),
    rt_pronome = as.numeric(rt_pronome),
    rt_verbo2 = as.numeric(rt_verbo2),
    eta = as.numeric(eta),
    ID = as.factor(ID), # ID del partecipante
    `ID frase` = as.factor(`ID frase`), # ID della frase (item)
    Cond = as.factor(Cond),
    `Bias di genere` = as.factor(`Bias di genere`)
  )

df_dislessici <- df_dislessici %>%
  mutate(
    Cond = as.factor(Cond),
    Bias = as.factor(`Bias di genere`),
    `Condizione linguistica` = as.factor(`Condizione linguistica`)
  )

#Controllo contrasti attuali per variabili fattoriali
errores_ita_dis <- filter(df_dislessici, Corretto == "no") # rtas erróneas
colnames(df_dislessici)

 
errores_ita_dis %>%
  group_by(part) %>%
  summarise(cant_errores = sum(Corretto == "no", na.rm = TRUE)) %>%
  arrange(cant_errores) %>%
  print(n = 150)

df_dislessici$eta <- as.character(df_dislessici$eta)
df_dislessici$eta <- as.numeric(df_dislessici$eta)

df_partecipanti_dislessici <- df_dislessici %>% 
  distinct(part, eta, ID, id_genero, educazione, lingua, nazionalita, residenza, mail, tiempo_total, lista)
# 84 parts

mean(df_partecipanti_dislessici$eta) # 24.57
sd(df_partecipanti_dislessici$eta) # 4.35
max(df_partecipanti_dislessici$eta) # 33
min(df_partecipanti_dislessici$eta) # 19

unique(df_partecipanti_dislessici$id_genero)

df_dislessici <- df_dislessici %>%
  mutate(id_genero = case_when(
    id_genero %in% c("F", "Donna", "femmina", "f", "donna cis", "donna", "Femmina", 
                     "DONNA", "Femminile", "FEMMINA") ~ "F",
    id_genero %in% c("MASCHIO", "maschio", "Maschio", "uomo", "M", "MASCHILE", "UOMO",
                     "Maschile", "Uomo") ~ "M", )) %>%
  mutate(id_genero = factor(id_genero, levels = c("F", "M")))

df_partecipanti_dislessici <- df_dislessici %>% 
  distinct(part, eta, ID, id_genero, educazione, lingua, nazionalita, residenza, mail, tiempo_total, lista)
summary(df_partecipanti_dislessici) # 2 donne; 5 uomini


df_intero <- df_dislessici
errori <- filter(df_dislessici, Corretto == "no") # rtas erróneas
df_dislessici_corretto <- df_dislessici %>% 
  filter(Corretto == "yes") %>% # dejo las respuestas correctas
  droplevels()

# Calcolo la percentuali dei dati eliminati fin'ora
((dim(df_intero)[1] - dim(errori)[1])/ dim(df_intero)[1])*100
# 17.32% controlli
# 41.30% di risposte errate nei dislessici

# 1️⃣ Calcolo delle statistiche globali sui dislessici
# 1️⃣ Calcolo delle statistiche globali sui dislessici
stats_dislessici <- df_dislessici_corretto %>%
  summarise(
    mean_rt_det_g      = mean(rt_determinante, na.rm = TRUE),
    sd_rt_det_g        = sd(rt_determinante, na.rm = TRUE),
    mean_rt_nome_g     = mean(rt_nome, na.rm = TRUE),
    sd_rt_nome_g       = sd(rt_nome, na.rm = TRUE),
    mean_rt_v1_g       = mean(rt_verbo1, na.rm = TRUE),
    sd_rt_v1_g         = sd(rt_verbo1, na.rm = TRUE),
    mean_rt_pronome_g  = mean(rt_pronome, na.rm = TRUE),
    sd_rt_pronome_g    = sd(rt_pronome, na.rm = TRUE),
    mean_rt_v2_g       = mean(rt_verbo2, na.rm = TRUE),
    sd_rt_v2_g         = sd(rt_verbo2, na.rm = TRUE)
  )

# 2️⃣ Filtro adattivo
df_dislessici_sd <- df_dislessici_corretto %>%
  group_by(ID) %>%
  mutate(
    n = n(),
    
    mean_rt_det      = if_else(n >= 8, mean(rt_determinante, na.rm = TRUE), stats_dislessici$mean_rt_det_g),
    sd_rt_det        = if_else(n >= 8, sd(rt_determinante, na.rm = TRUE), stats_dislessici$sd_rt_det_g),
    
    mean_rt_nome     = if_else(n >= 8, mean(rt_nome, na.rm = TRUE), stats_dislessici$mean_rt_nome_g),
    sd_rt_nome       = if_else(n >= 8, sd(rt_nome, na.rm = TRUE), stats_dislessici$sd_rt_nome_g),
    
    mean_rt_v1       = if_else(n >= 8, mean(rt_verbo1, na.rm = TRUE), stats_dislessici$mean_rt_v1_g),
    sd_rt_v1         = if_else(n >= 8, sd(rt_verbo1, na.rm = TRUE), stats_dislessici$sd_rt_v1_g),
    
    mean_rt_pronome  = if_else(n >= 8, mean(rt_pronome, na.rm = TRUE), stats_dislessici$mean_rt_pronome_g),
    sd_rt_pronome    = if_else(n >= 8, sd(rt_pronome, na.rm = TRUE), stats_dislessici$sd_rt_pronome_g),
    
    mean_rt_v2       = if_else(n >= 8, mean(rt_verbo2, na.rm = TRUE), stats_dislessici$mean_rt_v2_g),
    sd_rt_v2         = if_else(n >= 8, sd(rt_verbo2, na.rm = TRUE), stats_dislessici$sd_rt_v2_g)
  ) %>%
  mutate(
    limite_sup_rt_det      = mean_rt_det + 2.5 * sd_rt_det,
    limite_inf_rt_det      = mean_rt_det - 2.5 * sd_rt_det,
    limite_sup_rt_nome     = mean_rt_nome + 2.5 * sd_rt_nome,
    limite_inf_rt_nome     = mean_rt_nome - 2.5 * sd_rt_nome,
    limite_sup_rt_v1       = mean_rt_v1 + 2.5 * sd_rt_v1,
    limite_inf_rt_v1       = mean_rt_v1 - 2.5 * sd_rt_v1,
    limite_sup_rt_pronome  = mean_rt_pronome + 2.5 * sd_rt_pronome,
    limite_inf_rt_pronome  = mean_rt_pronome - 2.5 * sd_rt_pronome,
    limite_sup_rt_v2       = mean_rt_v2 + 2.5 * sd_rt_v2,
    limite_inf_rt_v2       = mean_rt_v2 - 2.5 * sd_rt_v2
  ) %>%
  ungroup() %>%
  filter(
    rt_determinante > limite_inf_rt_det & rt_determinante < limite_sup_rt_det,
    rt_nome > limite_inf_rt_nome & rt_nome < limite_sup_rt_nome,
    rt_verbo1 > limite_inf_rt_v1 & rt_verbo1 < limite_sup_rt_v1,
    rt_pronome > limite_inf_rt_pronome & rt_pronome < limite_sup_rt_pronome,
    rt_verbo2 > limite_inf_rt_v2 & rt_verbo2 < limite_sup_rt_v2
  )


#calculo el porcentaje de datos eliminados totales: 75 datos
((dim(df_dislessici_corretto)[1] - dim(df_dislessici_sd)[1])/ dim(df_dislessici_corretto)[1])*100 #
# 6.28% controlli;
# 13.15% dislessici dei dati eliminati 

contrasts(df_dislessici_sd$Cond)
contrasts(df_dislessici_sd$`Bias di genere`)
contrasts(df_dislessici_sd$`Condizione linguistica`)

#Passo ai contrasti scalati

contrasts(df_dislessici_sd$Cond) <- contr.sdif(4)
contrasts(df_dislessici_sd$`Bias di genere`) <- contr.sdif(4)
contrasts(df_dislessici_sd$`Condizione linguistica`) <-contr.sdif(2)

#Grafici

se <- function(x) #función para error standard
{
  y <- x[!is.na(x)]
  sqrt(var(as.vector(y))/length(y))
}

df_dislessici_sd <- df_dislessici_sd %>%
  mutate(
    `Condizione linguistica` = as.character(`Condizione linguistica`),
    Cond = as.character(Cond),
    Bias = as.character(`Bias di genere`)
  ) %>%
  mutate(
    cond_cruzada = case_when(
      `Condizione linguistica` == "gender" & Cond == "g_fem" & Bias == "masc" ~ "sesso masc - morf fem",
      `Condizione linguistica` == "gender" & Cond == "g_masc" & Bias == "fem" ~ "sesso fem - morf masc",
      `Condizione linguistica` == "gender" & Cond == "g_fem" & Bias == "fem"  ~ "sesso fem - morf fem",
      `Condizione linguistica` == "gender" & Cond == "g_masc" & Bias == "masc" ~ "sesso masc - morf masc",
      
      # Macro-condizione "cr_pr": mantengo un'etichetta che unisce Cond e Bias
      `Condizione linguistica` == "cr_pr" & Cond %in% c("PR_no","PR_yes") ~ paste(Cond, "-", Bias),

      TRUE ~ "altro"
    )
  )
table(df_dislessici_sd$`Condizione linguistica`, df_dislessici_sd$Cond)
table(df_dislessici_sd$cond_cruzada)
livelli_presenti <- unique(df_dislessici_sd$cond_cruzada)
ordine_preferito <- c(
  "sesso masc - morf fem",
  "sesso fem - morf masc",
  "sesso fem - morf fem",
  "sesso masc - morf masc"
)
# Aggiungi i livelli PR dinamicamente se presenti
pr_levels <- sort(livelli_presenti[grepl("^PR_", livelli_presenti)])
ordine_finale <- c(ordine_preferito, pr_levels, setdiff(livelli_presenti, c(ordine_preferito, pr_levels)))
df_dislessici_sd$cond_cruzada <- factor(df_dislessici_sd$cond_cruzada, levels = ordine_finale)

# --- CALCOLO MEDIE E SE per tutte le regioni (eventualmente separando per 'muestra' se esiste) ---
# Se non hai 'muestra', rimuovi dal group_by
group_vars <- c("cond_cruzada")

# colonne RT da riassumere (adatta se nomi diversi)
rt_cols <- c("rt_determinante", "rt_nome", "rt_verbo1", "rt_pronome", "rt_verbo2")

# Calcolo medie
mean_df <- df_dislessici_sd %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(across(all_of(rt_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(rt_cols), names_to = "RT", values_to = "M")

# Calcolo SE
se_df <- df_dislessici_sd %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(across(all_of(rt_cols), ~ se(.x)), .groups = "drop") %>%
  pivot_longer(cols = all_of(rt_cols), names_to = "RT", values_to = "SE")

# Unisco con left_join (più sicuro di merge)
mean_se <- left_join(mean_df, se_df, by = c(group_vars, "RT"))

# Ordine fattore RT
mean_se <- mean_se %>%
  mutate(RT = factor(RT, levels = rt_cols))

# --- PLOT: profilo RT per cond_cruzada (eventualmente per 'muestra') ---
p <- ggplot(mean_se, aes(x = RT, y = M, color = cond_cruzada, group = cond_cruzada)) +
  geom_line(aes(linetype = cond_cruzada), linewidth = .8) +
  geom_point(aes(shape = cond_cruzada), size = 3) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = .08) +
  labs(x = "", y = "Medie dei tempi di reazione (ms)",
       color = "Condizioni", shape = "Condizioni", linetype = "Condizioni") +
  scale_x_discrete(labels = c(
    rt_determinante = "determinante",
    rt_nome = "nome",
    rt_verbo1 = "verbo clausola 1",
    rt_pronome = "tutti/tutte/che",
    rt_verbo2 = "verbo clausola 2"
  )) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14)

print(p)
getwd()
ggsave('Medie_tempi_dislessici_ogni_condizione.png', plot = p, width = 10, height = 6)


#CALCOLO TEMPO MEDIO PER CONDIZIONE LINGUISTICA

df_dislessici_sd <- df_dislessici_sd %>%
  mutate(tempo_totale = rowSums(across(starts_with("RT_word_")), na.rm = TRUE))

df_summary_generale <- df_dislessici_sd %>%
  group_by(`Condizione linguistica`) %>%
  summarise(
    media_tempo = mean(tempo_totale),
    se_tempo = sd(tempo_totale) / sqrt(n()),
    .groups = "drop"
  )

ggplot(df_summary_generale, aes(x = `Condizione linguistica`, y = media_tempo, fill = `Condizione linguistica`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = media_tempo - se_tempo, ymax = media_tempo + se_tempo),
                position = position_dodge(width = 0.8), width = 0.2) +
  coord_cartesian(ylim = c(min(df_summary_generale$media_tempo) - 500, max(df_summary_generale$media_tempo) + 500)) +
  labs(
    title = "Tempo totale medio per condizione linguistica e combinazione di bias/morfologia",
    x = "Condizione combinata (cond_cruzada)",
    y = "Tempo medio totale (ms)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave('RT_2_cond.png', width = 10, height = 6)

#=============STIMO I MODELLI LINEARI===========================================

# Modello 0: solo intercetta
mod_time_0 <- lmer(log(rt_nome) ~ 1 + (1 | part) + (1 | `ID frase`), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_0)
# Modello 1: effetto della condizione combinata (bias + morfologia)
mod_time_1 <- lmer(log(tempo_totale) ~ `Condizione linguistica` + (1 | part) + (1 | `ID frase`), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_1)

# Modello 2: aggiungo la condizione linguistica (frasi gender vs cr/pr)
mod_time_2 <- lmer(log(tempo_totale) ~ `Condizione linguistica` + Cond + (1 | part) + (1 | `ID frase`), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_2)

# Modello 3: interazione tra condizione linguistica e condizione combinata
mod_time_3 <- lmer(log(tempo_totale) ~ `Condizione linguistica` + Cond + `Bias di genere`+ (1 | part) + (1 | `ID frase`), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_3)

anova(mod_time_0, mod_time_1, mod_time_2, mod_time_3)
AIC(mod_time_0, mod_time_1, mod_time_2, mod_time_3)
summary(mod_time_1)
contrasts(df_dislessici_sd$cond_cruzada) <-contr.sdif(8)
contrasts(df_dislessici_sd$cond_cruzada)

df_summary <- df_dislessici_sd %>%
  group_by(`Condizione linguistica`, cond_cruzada) %>%
  summarise(
    media_tempo = mean(tempo_totale),
    se_tempo = sd(tempo_totale) / sqrt(n()),
    .groups = "drop"
  )


ggplot(df_summary, aes(x = cond_cruzada, y = media_tempo, fill = `Condizione linguistica`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = media_tempo - se_tempo, ymax = media_tempo + se_tempo),
                position = position_dodge(width = 0.8), width = 0.2) +
  coord_cartesian(ylim = c(min(df_summary$media_tempo) - 500, max(df_summary$media_tempo) + 500)) +
  labs(
    title = "Tempo totale medio per condizione linguistica e combinazione di bias/morfologia",
    x = "Condizione combinata (cond_cruzada)",
    y = "Tempo medio totale (ms)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



ggsave('RT_4_cond.png', width = 10, height = 6)


#=============STIMO I MODELLI LINEARI===========================================

# Modello 0: solo intercetta
mod_time_0 <- lmer(log(tempo_totale) ~ 1 + (1 | part), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_0)
# Modello 1: effetto della condizione combinata (bias + morfologia)
mod_time_1 <- lmer(log(tempo_totale) ~ cond_cruzada + (1 | part) + (1 | `ID frase`), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_1)

# Modello 2: aggiungo la condizione linguistica (frasi gender vs cr/pr)
mod_time_2 <- lmer(log(tempo_totale) ~ cond_cruzada * `Condizione linguistica` + (1 | part), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_2)

# Modello 3: interazione tra condizione linguistica e condizione combinata
mod_time_3 <- lmer(log(tempo_totale) ~ cond_cruzada * `Condizione linguistica` * Cond + (1 | part), 
                   data = df_dislessici_sd, REML = FALSE)
summary(mod_time_3)

anova(mod_time_0, mod_time_1, mod_time_2, mod_time_3)
AIC(mod_time_0, mod_time_1, mod_time_2, mod_time_3)

summary(mod_time_1)
library(emmeans)

emmeans(mod_time_1, ~ cond_cruzada) %>%
  as_tibble() %>%
  mutate(tempo_ms = exp(emmean))
emmeans(mod_time_1, ~ `cond_cruzada`) %>% pairs()

table(df_dislessici_sd$`Condizione linguistica`, df_dislessici_sd$cond_cruzada)

# --- ANALISI ACCURATEZZA: GENDER vs CR_PR (SOLO DISLESSICI) ---
df_dislessici <- df_dislessici %>%
  mutate(
    `Condizione linguistica` = as.character(`Condizione linguistica`),
    Cond = as.character(Cond),
    Bias = as.character(`Bias di genere`)
  ) %>%
  mutate(
    cond_cruzada = case_when(
      `Condizione linguistica` == "gender" & Cond == "g_fem" & Bias == "masc" ~ "sesso masc - morf fem",
      `Condizione linguistica` == "gender" & Cond == "g_masc" & Bias == "fem" ~ "sesso fem - morf masc",
      `Condizione linguistica` == "gender" & Cond == "g_fem" & Bias == "fem"  ~ "sesso fem - morf fem",
      `Condizione linguistica` == "gender" & Cond == "g_masc" & Bias == "masc" ~ "sesso masc - morf masc",
      
      # Macro-condizione "cr_pr": mantengo un'etichetta che unisce Cond e Bias
      `Condizione linguistica` == "cr_pr" & Cond %in% c("PR_no","PR_yes") ~ paste(Cond, "-", Bias),
      
      TRUE ~ "altro"
    )
  )
table(df_dislessici$`Condizione linguistica`, df_dislessici$Cond)
table(df_dislessici$cond_cruzada)
livelli_presenti <- unique(df_dislessici$cond_cruzada)
ordine_preferito <- c(
  "sesso masc - morf fem",
  "sesso fem - morf masc",
  "sesso fem - morf fem",
  "sesso masc - morf masc"
)
# Aggiungi i livelli PR dinamicamente se presenti
pr_levels <- sort(livelli_presenti[grepl("^PR_", livelli_presenti)])
ordine_finale <- c(ordine_preferito, pr_levels, setdiff(livelli_presenti, c(ordine_preferito, pr_levels)))
df_dislessici$cond_cruzada <- factor(df_dislessici$cond_cruzada, levels = ordine_finale)

# --- CALCOLO MEDIE E SE per tutte le regioni (eventualmente separando per 'muestra' se esiste) ---
# Se non hai 'muestra', rimuovi dal group_by
group_vars <- c("cond_cruzada")
contrasts(df_dislessici$cond_cruzada) <-contr.sdif(8)
contrasts(df_dislessici$cond_cruzada)


df_dislessici_acc <- df_dislessici %>%
  mutate(
    Corretto_finale = case_when(
      Corretto == "yes" ~ 1,
      Corretto == "no"  ~ 0,
      TRUE ~ NA_real_ # Gestisce eventuali valori mancanti o diversi
    )
  )

# 2. Calcola media, sd, n, e se per l'accuratezza
accuratezza_cond <- df_dislessici_acc %>%
  filter(!is.na(Corretto_finale)) %>% # Rimuovi righe con NA in Corretto_finale se presenti
  group_by(cond_cruzada) %>%
  summarise(
    accuratezza = mean(Corretto_finale, na.rm = TRUE), # Calcola la media (proporzione)
    sd = sd(Corretto_finale, na.rm = TRUE),           # Calcola la deviazione standard
    n = n(),                                         # Conta le osservazioni
    se = sd / sqrt(n),                               # Calcola l'errore standard
    .groups = 'drop' # Rimuove il raggruppamento dopo summarise
  ) %>%
  # Moltiplica per 100 solo DOPO aver calcolato se (se vuoi la % nel grafico)
  # Altrimenti calcola se sulla proporzione (accuratezza)
  mutate(accuratezza_perc = accuratezza * 100,
         se_perc = se * 100) %>%
  arrange(desc(accuratezza_perc)) # Ordina se necessario

ggplot(accuratezza_cond, aes(x = cond_cruzada, y = accuratezza_perc, fill = cond_cruzada)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = accuratezza_perc - se_perc,
                    ymax = accuratezza_perc + se_perc), # Usa le variabili *_perc
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    title = "Accuratezza media (%) per condizione nel gruppo DG",
    x = "Condizione",
    y = "Accuratezza media (%)", # Aggiorna etichetta asse Y
    fill = "Condizione" # Aggiorna etichetta legenda
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('Acc_4_cond.png', width = 10, height = 6)
accuratezza_cond


# Modello nullo: solo intercetta e random effect per soggetto
mod_acc_0 <- glmer(Corretto_finale ~ 1 + (1 | part),
                   data = df_dislessici_acc,
                   family = binomial)

# Modello 1: effetto principale della condizione combinata
mod_acc_1 <- glmer(Corretto_finale ~ cond_cruzada + (1 | part),
                   data = df_dislessici_acc,
                   family = binomial)

# Modello 2: effetto principale del bias di genere (se presente nel dataset)
mod_acc_2 <- glmer(Corretto_finale ~ cond_cruzada + `Bias di genere` + `Cond`+ (1 | part),
                   data = df_dislessici_acc,
                   family = binomial)

# Modello 3: interazione tra cond_cruzada e Bias di genere
mod_acc_3 <- glmer(Corretto_finale ~ cond_cruzada * `Bias di genere` + (1 | part),
                   data = df_dislessici_acc,
                   family = binomial)

anova(mod_acc_0, mod_acc_1, mod_acc_2, mod_acc_3, test = "Chisq")
AIC(mod_acc_0, mod_acc_1, mod_acc_2, mod_acc_3)
summary(mod_acc_1)

df_dislessici %>%
  group_by(cond_cruzada) %>%
  summarise(
    accuratezza = mean(Corretto_finale, na.rm = TRUE) * 100,
    n = n()
  ) %>%
  arrange(desc(accuratezza))
