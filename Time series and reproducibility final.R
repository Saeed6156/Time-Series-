# Dans un premier temps, déterminez le chemin de chargement des données et créez une série chronologique de CO2

setwd("C:/Users/saeid/OneDrive - Université Laval/Uni Course R/Time series and reproducibility")



# Maintenant, lisez les données de hawai.csv
data <- read_csv("hawai.csv")

# Charger les bibliothèques nécessaires
library(tidyverse)
library(readr)
library(lubridate)


data <- read.csv("hawai.csv")

co2_time_series <- ts(data$CO2, start = c(1958, 3), end = c(2001, 12), frequency = 12)

# Affichage des données (données de tracé)
autoplot(co2_time_series, xlab = "Year", ylab = "CO2 Concentration (ppm)")

# Deuxièmement:séparez la série en parties de formation et de test
train_size <- round(length(co2_time_series) * 0.7)
train_data <- window(co2_time_series, end = c(1988, 12))
test_data <- window(co2_time_series, start = c(1989, 1))

# Troisième étape: Créer un modèle de prévision sur les données d'entraînement et projeter la prévision
library(forecast)
model <- auto.arima(train_data)
forecast <- forecast(model, h = length(test_data))

# Quatrième étape: Effectuer une analyse résiduelle
residuals <- residuals(model)
plot(residuals)

# Étape 5 : Commenter la fiabilité du modèle et suggérer des améliorations
# Pour déterminer la fiabilité du modèle, vous pouvez examiner plusieurs facteurs :
# Essayez maintenant de comparer les prévisions aux données de test pour inspecter visuellement l'exactitude
plot(forecast, main = "CO2 Forecast vs. Test Data")
lines(test_data, col = "red")
legend("topleft", legend = c("Forecast data", "Test Part"), col = c("black", "red"), lty = 2, lwd = 1.5, text.font = 1)

# Calcul de métriques telles que l'erreur absolue moyenne (MAE) ou l'erreur quadratique moyenne (RMSE)
accuracy(forecast, test_data)

# Effectuer des diagnostics supplémentaires sur les résidus pour s'assurer qu'il s'agit bien de bruit blanc
# Tracés ACF et PACF des résidus
acf(residuals)
pacf(residuals)



