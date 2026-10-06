
packages <- c(
  "plm",        # modele panel (FE, RE, Hausman)
  "dplyr",      # manipulare date
  "tidyr",      # reshape date
  "ggplot2",    # grafice
  "forecast",   # ARIMA, auto.arima, prognoze
  "tseries",    # testul ADF (adf.test)
  "urca",       # testul ADF extins (ur.df)
  "lmtest",     # teste diagnostice regresie
  "sandwich",   # erori standard robuste
  "car",        # VIF (multicoliniaritate)
  "readxl",     # citire Excel
  "corrplot"    # matrice de corelatie
)

installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

#  Citirea fisierelor
df_cyber <- read.csv("Global_Cybersecurity_Threats_2015-2024.csv",
                     stringsAsFactors = FALSE)

df_macro <- read_excel("date pe tari.xlsx")

# Structura datelor
str(df_cyber)
str(df_macro)

head(df_cyber, 10)
head(df_macro)

# Dimensiuni
dim(df_cyber)   # 3000 obs x 10 variabile
dim(df_macro)   # 12 obs x 5 variabile

# Valori lipsa
colSums(is.na(df_cyber))
colSums(is.na(df_macro))

# Tari si ani unici
unique(df_cyber$Country)
unique(df_cyber$Year)

# Distributia tipurilor de atac
table(df_cyber$Attack.Type)
table(df_cyber$Target.Industry)
table(df_cyber$Attack.Source)
table(df_cyber$Security.Vulnerability.Type)

# Statistici descriptive 

summary(df_cyber[, c(
  "Financial.Loss..in.Million...",
  "Number.of.Affected.Users",
  "Incident.Resolution.Time..in.Hours."
)])

# DATE PANEL

panel <- df_cyber %>%
  group_by(Country, Year) %>%
  summarise(
    total_loss     = sum(Financial.Loss..in.Million...),
    num_incidents  = n(),
    total_users    = sum(Number.of.Affected.Users),
    avg_resolution = mean(Incident.Resolution.Time..in.Hours.),
    .groups = "drop"
  )

View(panel)
dim(panel)   

#  logaritmare

panel <- panel %>%
  mutate(
    log_loss      = log(total_loss),
    log_incidents = log(num_incidents),
    log_users     = log(total_users)
  )



panel_ok <- pdata.frame(panel, index = c("Country", "Year"))

# Verificare structura panel
pdim(panel_ok)

#  Statistici descriptive ale variabilelor panel

summary(panel_ok[, c("log_loss", "log_incidents", "log_users", "avg_resolution")])


#  Evolutia anuala a pierderilor financiare 

evolutie_an <- df_cyber %>%
  group_by(Year) %>%
  summarise(
    total_loss    = sum(Financial.Loss..in.Million...),
    num_incidents = n(),
    .groups = "drop"
  )

ggplot(evolutie_an, aes(x = Year, y = total_loss)) +
  geom_line(color = "#1F3864", linewidth = 1.2) +
  geom_point(color = "#1F3864", size = 3.5, shape = 19) +
  labs(
    title    = "Evolutia anuala a pierderilor financiare cauzate de atacuri cibernetice",
    subtitle = "Agregat pentru 10 tari, 2015-2024",
    x        = "An",
    y        = "Pierderi totale (milioane USD)"
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_bw(base_size = 12)

# Evolutia anuala a numarului de incidente

ggplot(evolutie_an, aes(x = Year, y = num_incidents)) +
  geom_line(color = "#C0392B", linewidth = 1.2) +
  geom_point(color = "#C0392B", size = 3.5, shape = 19) +
  labs(
    title    = "Evolutia anuala a numarului de incidente cibernetice",
    subtitle = "Agregat pentru 10 tari, 2015-2024",
    x        = "An",
    y        = "Numar incidente"
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_bw(base_size = 12)

# Distributia log(pierderi) per tara 

ggplot(panel, aes(x = reorder(Country, log_loss, FUN = median), y = log_loss)) +
  geom_boxplot(fill = "#3B82F6", alpha = 0.6, outlier.colour = "#C0392B") +
  coord_flip() +
  labs(
    title = "Distributia log(pierderi financiare) per tara",
    x     = "Tara",
    y     = "log(Pierderi totale, mil. USD)"
  ) +
  theme_bw(base_size = 12)

#  Distributia tipurilor de atac

df_cyber %>%
  count(Attack.Type) %>%
  ggplot(aes(x = reorder(Attack.Type, n), y = n, fill = Attack.Type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Distributia tipurilor de atacuri cibernetice (2015-2024)",
    x     = "Tip atac",
    y     = "Numar incidente"
  ) +
  theme_bw(base_size = 12)

#  Matrice de corelatie intre variabilele panel

cor_data <- panel[, c("log_loss", "log_incidents", "log_users", "avg_resolution")]
cor_matrix <- cor(cor_data, use = "complete.obs")
print(round(cor_matrix, 4))

corrplot(cor_matrix,
         method  = "color",
         type    = "upper",
         addCoef.col = "black",
         tl.col  = "black",
         title   = "Matricea de corelatie - variabile panel",
         mar     = c(0, 0, 2, 0))

library(reshape2)
library(ggplot2)

cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 4) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = "darkblue",
    midpoint = 0,
    limit = c(-1,1)
  ) +
  theme_minimal() +
  labs(
    title = "Matricea de corelație",
    x = "",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )
colnames(cor_matrix) <- c(
  "Pierderi (log)",
  "Incidente (log)",
  "Utilizatori (log)",
  "Timp rezolvare"
)
rownames(cor_matrix) <- colnames(cor_matrix)

# MODELE ECONOMETRICE DE TIP PANEL
# Pooled OLS

ols <- plm(
  formula = log_loss ~ log_incidents + log_users + avg_resolution,
  data    = panel_ok,
  model   = "pooling",
  index   = c("Country", "Year")
)

summary(ols)

# Testul VIF - verificare multicoliniaritate
# Valori VIF > 10 indica multicoliniaritate severa
vif_model <- lm(log_loss ~ log_incidents + log_users + avg_resolution,
                data = panel)
car::vif(vif_model)

#  Modelul Fixed Effects 

FE <- plm(
  formula = log_loss ~ log_incidents + log_users + avg_resolution,
  data    = panel_ok,
  model   = "within",
  index   = c("Country", "Year")
)

summary(FE)

# Efectele fixe estimate pentru fiecare tara
fixef(FE)

# Testul F: efectele fixe sunt semnificative fata de OLS?
# H0: toate efectele fixe sunt nule (OLS este suficient)
# H1: cel putin un efect fix este nenul (FE este necesar)
pFtest(FE, ols)
#p value>0.05, nu resping ip nula

#  Modelul Random Effects

RE <- plm(
  formula = log_loss ~ log_incidents + log_users + avg_resolution,
  data    = panel_ok,
  model   = "random",
  index   = c("Country", "Year")
)

summary(RE)

#  Testul Hausman
# H0: efectele individuale sunt NECORELATE cu regresorii -> RE preferat
# H1: efectele individuale sunt CORELATE cu regresorii  -> FE preferat
# Daca p-value > 0.05: nu respingem H0, RE este consistent si eficient
# Daca p-value < 0.05: respingem H0, FE este singurul estimator consistent

phtest(FE, RE)
#nu resping ip nula, folosesc random effects
# Testul Breusch-Pagan 
bptest(ols)

# Testul Wooldridge 
pbgtest(FE)
#nu exista autocorelatie

coeftest(FE, vcov = vcovHC(FE, type = "HC3"))
coeftest(RE, vcov = vcovHC(RE, type = "HC3"))


# Agregare anuala globala (toate cele 10 tari)
ts_anual <- df_cyber %>%
  group_by(Year) %>%
  summarise(
    total_loss    = sum(Financial.Loss..in.Million...),
    num_incidents = n(),
    .groups       = "drop"
  ) %>%
  arrange(Year)

print(ts_anual)

# Obiect ts pentru pierderi financiare (date anuale, frecventa = 1)
ts_loss <- ts(ts_anual$total_loss,    start = 2015, frequency = 1)

# Obiect ts pentru numarul de incidente
ts_inc  <- ts(ts_anual$num_incidents, start = 2015, frequency = 1)

# Afisare
print(ts_loss)
print(ts_inc)

# Graficul seriei originale - pierderi
autoplot(ts_loss) +
  labs(
    title    = "Evolutia anuala a pierderilor financiare (2015-2024)",
    subtitle = "Serie de timp - 10 tari, agregat global",
    x        = "An",
    y        = "Pierderi totale (milioane USD)"
  ) +
  theme_bw(base_size = 12)

# Graficul seriei originale - incidente
autoplot(ts_inc) +
  labs(
    title    = "Evolutia anuala a numarului de incidente cibernetice (2015-2024)",
    subtitle = "Serie de timp - 10 tari, agregat global",
    x        = "An",
    y        = "Numar incidente"
  ) +
  theme_bw(base_size = 12)

# ACF si PACF pentru ambele serii
# Interpretare ACF: coeficienti mari si descrescatori lent => nestationaritate
# Interpretare PACF: spike la lag 1 => structura AR(1)
ggtsdisplay(ts_loss,
            main = "Pierderi financiare: serie, ACF si PACF")

ggtsdisplay(ts_inc,
            main = "Numarul de incidente: serie, ACF si PACF")

# 5.3 Testul de stationaritate - Augmented Dickey-Fuller (ADF)

# H0: seria contine o radacina unitara (este NEstationara)
# H1: seria este stationara
# Daca p-value < 0.05: respingem H0, seria este stationara
# Daca p-value > 0.05: nu respingem H0, seria este NEstationara -> diferentiem

cat("=== ADF - Pierderi in NIVEL ===\n")
adf.test(ts_loss, alternative = "stationary")

cat("=== ADF - Incidente in NIVEL ===\n")
adf.test(ts_inc, alternative = "stationary")

# Daca nestationare: aplicam prima diferenta
cat("=== ADF - Pierderi DIFERENTIATE (d=1) ===\n")
adf.test(diff(ts_loss), alternative = "stationary")

cat("=== ADF - Incidente DIFERENTIATE (d=1) ===\n")
adf.test(diff(ts_inc), alternative = "stationary")

# ACF si PACF dupa diferentiere - verifica stationaritatea vizuala
ggtsdisplay(diff(ts_loss),
            main = "Prima diferenta - Pierderi financiare: serie, ACF si PACF")

ggtsdisplay(diff(ts_inc),
            main = "Prima diferenta - Numar incidente: serie, ACF si PACF")


#  Identificarea ordinelor optime ARIMA


cat("=== auto.arima - Pierderi financiare ===\n")
fit_loss_auto <- auto.arima(
  ts_loss,
  seasonal      = FALSE,   
  stepwise      = FALSE,   
  approximation = FALSE
)
summary(fit_loss_auto)

cat("=== auto.arima - Numar incidente ===\n")
fit_inc_auto <- auto.arima(
  ts_inc,
  seasonal      = FALSE,
  stepwise      = FALSE,
  approximation = FALSE
)
summary(fit_inc_auto)

# estimare manuala ARIMA(1,1,0)
# d=1: prima diferenta 
# p=1: componenta AR de ordin 1 
# q=0: fara componenta MA

cat("=== ARIMA(1,1,0) manual - Pierderi financiare ===\n")
fit_loss <- Arima(ts_loss, order = c(1, 1, 0))
summary(fit_loss)

cat("=== ARIMA(1,1,0) manual - Numar incidente ===\n")
fit_inc <- Arima(ts_inc, order = c(1, 1, 0))
summary(fit_inc)


#  Diagnosticarea reziduurilor modelelor ARIMA



cat("=== Diagnostice reziduuri - Pierderi financiare ===\n")
checkresiduals(fit_loss)

cat("=== Diagnostice reziduuri - Numar incidente ===\n")
checkresiduals(fit_inc)

# Testul Ljung-Box 
# H0: reziduurile sunt necorelate (zgomot alb) 
# Daca p-value > 0.05: reziduurile sunt zgomot alb, modelul este adecvat
Box.test(residuals(fit_loss), lag = 5, type = "Ljung-Box")
Box.test(residuals(fit_inc),  lag = 5, type = "Ljung-Box")

#Compararea modelelor prin criterii informationale

cat("=== Comparatie AIC/BIC: Pierderi ===\n")
cat("ARIMA(1,1,0):", AIC(fit_loss), "BIC:", BIC(fit_loss), "\n")
cat("auto.arima:  ", AIC(fit_loss_auto), "BIC:", BIC(fit_loss_auto), "\n")

cat("\n=== Comparatie AIC/BIC: Incidente ===\n")
cat("ARIMA(1,1,0):", AIC(fit_inc), "BIC:", BIC(fit_inc), "\n")
cat("auto.arima:  ", AIC(fit_inc_auto), "BIC:", BIC(fit_inc_auto), "\n")

# Erori de prognoza pe esantionul de antrenare
cat("\n=== Acuratete in-sample: Pierderi ===\n")
round(accuracy(fit_loss), 4)

cat("\n=== Acuratete in-sample: Incidente ===\n")
round(accuracy(fit_inc), 4)


#  Prognoze pentru perioada 2025-2027


# Generare prognoze pe 3 ani inainte
fc_loss <- forecast(fit_loss, h = 3)
fc_inc  <- forecast(fit_inc,  h = 3)

# Afisare valori prognozate cu intervale de incredere 80% si 95%
print(fc_loss)
print(fc_inc)

# Graficul prognozei pentru pierderi financiare
autoplot(fc_loss) +
  labs(
    title    = "Prognoza ARIMA(1,1,0): Pierderi financiare 2025-2027",
    subtitle = "Cu intervale de incredere 80% si 95%",
    x        = "An",
    y        = "Pierderi totale (milioane USD)"
  ) +
  theme_bw(base_size = 12)

# Graficul prognozei pentru numarul de incidente
autoplot(fc_inc) +
  labs(
    title    = "Prognoza ARIMA(1,1,0): Numar incidente cibernetice 2025-2027",
    subtitle = "Cu intervale de incredere 80% si 95%",
    x        = "An",
    y        = "Numar incidente"
  ) +
  theme_bw(base_size = 12)

# Grafic comparativ: valorile observate + prognoza pentru ambele serii (normalizate)
autoplot(ts_loss / max(ts_loss), series = "Pierderi (normalizat)") +
  autolayer(fc_loss$mean / max(ts_loss),
            series = "Prognoza pierderi", linetype = "dashed") +
  autolayer(ts_inc  / max(ts_inc),
            series = "Incidente (normalizat)") +
  autolayer(fc_inc$mean / max(ts_inc),
            series = "Prognoza incidente", linetype = "dashed") +
  labs(
    title  = "Prognoze comparative: pierderi financiare vs. incidente cibernetice",
    subtitle = "Serii normalizate (valoare maxima = 1), prognoze 2025-2027",
    x      = "An",
    y      = "Valoare normalizata"
  ) +
  guides(colour = guide_legend(title = "Serie")) +
  theme_bw(base_size = 12)


# ==============================================================================
# 6. TABEL REZUMAT FINAL
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("  REZUMAT REZULTATE PRINCIPALE\n")
cat("============================================================\n")
cat("\n--- MODELE PANEL ---\n")
cat("OLS Pooled:\n")
print(summary(ols)$coefficients)
cat("\nFixed Effects (within):\n")
print(summary(FE)$coefficients)
cat("\nRandom Effects:\n")
print(summary(RE)$coefficients)
cat("\nTestul Hausman (FE vs RE):\n")
print(phtest(FE, RE))

cat("\n--- SERII DE TIMP ---\n")
cat("ADF Pierderi (nivel): t =", adf.test(ts_loss)$statistic, "\n")
cat("ADF Pierderi (diff1): t =", adf.test(diff(ts_loss))$statistic, "\n")
cat("ADF Incidente (nivel):", adf.test(ts_inc)$statistic, "\n")
cat("ADF Incidente (diff1):", adf.test(diff(ts_inc))$statistic, "\n")
cat("\nARIMA(1,1,0) Pierderi - coef AR(1):", coef(fit_loss)[1],
    "| AIC:", AIC(fit_loss), "\n")
cat("ARIMA(1,1,0) Incidente - coef AR(1):", coef(fit_inc)[1],
    "| AIC:", AIC(fit_inc), "\n")

cat("\n--- PROGNOZE 2025-2027 ---\n")
cat("Pierderi financiare (mil. USD):\n")
cat("  2025:", round(fc_loss$mean[1], 2), "\n")
cat("  2026:", round(fc_loss$mean[2], 2), "\n")
cat("  2027:", round(fc_loss$mean[3], 2), "\n")
cat("Numar incidente:\n")
cat("  2025:", round(fc_inc$mean[1], 1), "\n")
cat("  2026:", round(fc_inc$mean[2], 1), "\n")
cat("  2027:", round(fc_inc$mean[3], 1), "\n")


library(ggplot2)
library(dplyr)

#  Distributia tipurilor de atac
df_cyber %>%
  count(Attack.Type) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Attack.Type, n), y = n, fill = Attack.Type)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 4, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c(
    "#1F3864", "#2E75B6", "#3B82F6",
    "#60A5FA", "#93C5FD", "#BFDBFE"
  )) +
  scale_y_continuous(limits = c(0, 600)) +
  labs(
    title    = "Distribuția incidentelor cibernetice după tipul atacului",
    subtitle = "Perioada 2015-2024",
    x        = "Tip atac",
    y        = "Număr incidente",
    caption  = "Sursa: prelucrare proprie in R"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", face = "italic")
  )

#Distributia industriilor vizate
df_cyber %>%
  count(Target.Industry) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Target.Industry, n), y = n, fill = Target.Industry)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 4, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c(
    "#0D47A1", "#1565C0", "#1976D2",
    "#1E88E5", "#42A5F5", "#64B5F6", "#90CAF9"
  )) +
  scale_y_continuous(limits = c(0, 550)) +
  labs(
    title    = "Distribuția incidentelor cibernetice dupa industria vizată",
    subtitle = "Perioada 2015-2024",
    x        = "Industrie",
    y        = "Număr incidente",
    caption  = "Sursa: prelucrare proprie in R"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", face = "italic")
  )

#Distributia surselor de atac
df_cyber %>%
  count(Attack.Source) %>%
  mutate(pct = round(n / sum(n) * 100, 1),
         label = paste0(n, "\n(", pct, "%)")) %>%
  ggplot(aes(x = reorder(Attack.Source, n), y = n, fill = Attack.Source)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_fill_manual(values = c("#1F3864", "#2E75B6", "#60A5FA", "#BFDBFE")) +
  scale_y_continuous(limits = c(0, 950)) +
  labs(
    title    = "Distributia incidentelor cibernetice dupa sursa atacului",
    subtitle = "Perioada 2015-2024, 10 tari",
    x        = "Sursa atac",
    y        = "Numar incidente",
    caption  = "Sursa: prelucrare proprie in R"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", face = "italic")
  )

# Distributia tipurilor de vulnerabilitate
df_cyber %>%
  count(Security.Vulnerability.Type) %>%
  mutate(pct = round(n / sum(n) * 100, 1),
         label = paste0(n, "\n(", pct, "%)")) %>%
  ggplot(aes(x = reorder(Security.Vulnerability.Type, n), y = n, fill = Security.Vulnerability.Type)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_fill_manual(values = c("#1F3864", "#2E75B6", "#60A5FA", "#BFDBFE")) +
  scale_y_continuous(limits = c(0, 950)) +
  labs(
    title    = "Distributia incidentelor dupa tipul vulnerabilitatii exploatate",
    subtitle = "Perioada 2015-2024, 10 tari",
    x        = "Tip vulnerabilitate",
    y        = "Numar incidente",
    caption  = "Sursa: prelucrare proprie in R"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", face = "italic")
  )

# Heatmap  care combinatii atac industrie sunt cele mai frecvente
df_cyber %>%
  count(Attack.Type, Target.Industry) %>%
  ggplot(aes(x = Target.Industry, y = Attack.Type, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#BFDBFE", high = "#1F3864",
                      name = "Nr. incidente") +
  labs(
    title    = "Frecventa atacurilor cibernetice dupa tip si industrie vizata",
    subtitle = "Perioada 2015-2024, 10 tari",
    x        = "Industrie vizata",
    y        = "Tip atac",
    caption  = "Sursa: prelucrare proprie in R"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", face = "italic"),
    axis.text.x   = element_text(angle = 30, hjust = 1)
  )
library(dplyr)
library(ggplot2)
library(scales)

#distr pierderilor financiare totale pe tari
distributie_tara <- panel %>%
  group_by(Country) %>%
  summarise(
    total_pierderi   = sum(total_loss),
    total_incidente  = sum(num_incidents),
    pierdere_medie   = round(mean(total_loss / num_incidents), 2),
    incidente_medii  = round(mean(num_incidents), 1)
  ) %>%
  # Redenumire tari in romana
  mutate(Country = dplyr::recode(Country,
                          "Australia"  = "Australia",
                          "Brazil"     = "Brazilia",
                          "China"      = "China",
                          "France"     = "Franta",
                          "Germany"    = "Germania",
                          "India"      = "India",
                          "Japan"      = "Japonia",
                          "Russia"     = "Rusia",
                          "UK"         = "Regatul Unit",
                          "USA"        = "SUA"
  ))


ggplot(distributie_tara, 
       aes(x = reorder(Country, total_pierderi), 
           y = total_pierderi)) +
  
  geom_col(fill = "aquamarine4", width = 0.7) +
  
  
  geom_text(aes(label = comma(total_pierderi)),
            hjust = -0.1, size = 3.5) +
  
  coord_flip() +
  
  labs(
    title = "Distribuția pierderilor financiare totale pe țări",
    subtitle = "Valori cumulate pentru perioada analizată",
    x = "Țara",
    y = "Total pierderi (milioane USD)",
    caption = "Sursa: prelucrare proprie"
  ) +
  
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.1))) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# dacă nu ai pachetul
install.packages("lmtest")

# încarci pachetul
library(lmtest)

# test Durbin-Watson pe modelul OLS
dwtest(ols)

install.packages("gt")
library(gt)
library(dplyr)

distributie_tara <- panel %>%
  group_by(Country) %>%
  summarise(
    total_pierderi  = round(sum(total_loss), 2),
    total_incidente = sum(num_incidents),
    pierdere_medie  = round(sum(total_loss) / sum(num_incidents), 2),
    incidente_medii = round(mean(num_incidents), 1)
  ) %>%
  arrange(Country)

distributie_tara %>%
  gt() %>%
  cols_label(
    Country         = "Țara",
    total_pierderi  = "Total pierderi (mil. USD)",
    total_incidente = "Nr. total incidente",
    pierdere_medie  = "Pierdere medie/incident (mil. USD)",
    incidente_medii = "Nr. mediu incidente/an"
  ) %>%
  fmt_number(columns = c(total_pierderi, pierdere_medie), decimals = 2) %>%
  tab_header(
    title    = "Distribuția pierderilor financiare și a incidentelor cibernetice per țară",
    subtitle = "Perioada 2015–2024"
  ) %>%
  tab_source_note("Sursa: prelucrare proprie în R") %>%
  tab_style(
    style = cell_fill(color = "#EBF2FA"),
    locations = cells_body(rows = seq(2, nrow(distributie_tara), 2))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "white"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    column_labels.background.color = "#1F3864",
    table.font.names = "Times New Roman",
    table.font.size  = 12
  )



# Evolutia anuala agregata pentru toate cele 10 tari
evolutie_an <- df_cyber %>%
  group_by(Year) %>%
  summarise(
    total_loss    = round(sum(Financial.Loss..in.Million...), 2),
    num_incidents = n()
  ) %>%
  arrange(Year)

print(evolutie_an)

# Maximul si minimul
evolutie_an %>% filter(total_loss == max(total_loss))
evolutie_an %>% filter(total_loss == min(total_loss))

df_cyber %>%
  count(Attack.Type) %>%
  mutate(pct = round(n / sum(n) * 100, 1),
         label = paste0(Attack.Type, "\n", pct, "%")) %>%
  ggplot(aes(x = "", y = pct, fill = Attack.Type)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#1F3864","#2E75B6","#3B82F6",
                               "#60A5FA","#93C5FD","#BFDBFE")) +
  labs(
    title   = "Distribuția incidentelor cibernetice după tipul atacului",
    subtitle = "Perioada 2015–2024, 10 țări",
    fill    = "Tip atac",
    caption = "Sursa: prelucrare proprie în R"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    plot.caption  = element_text(color = "gray50", face = "italic"),
    legend.position = "right"
  )

df_cyber %>%
  count(Attack.Source) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(n / sum(n) * 100, 1))


df_cyber %>%
  count(Attack.Source) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = "", y = pct, fill = Attack.Source)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#1F3864", "#2E75B6", "#60A5FA", "#BFDBFE")) +
  labs(
    title    = "Distribuția incidentelor cibernetice după sursa atacului",
    subtitle = "Perioada 2015–2024, 10 țări",
    fill     = "Sursa atac",
    caption  = "Sursa: prelucrare proprie în R"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5),
    plot.caption  = element_text(color = "gray50", face = "italic"),
    legend.position = "right"
  )