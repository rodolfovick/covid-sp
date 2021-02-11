library(shiny)

## Get Covid data CSV
fcity <- read.csv("dados_covid_sp.csv", sep=";")
cities <- unique(fcity$nome_munic)
day <- as.Date(fcity[nrow(fcity), "datahora"], "%Y-%m-%d")
n <- length(cities)

file <- read.csv("casos_obitos_doencas_preexistentes.csv", sep=";")

general <- data.frame("casos"=integer(n), "obitos"=integer(n), "mortalidade"=numeric(n))

gender <- data.frame("m_casos"=integer(n), "m_obitos"=integer(n), "m_pcasos"=numeric(n), "m_pobitos"=numeric(n), "m_rate"=numeric(n),
                     "f_casos"=integer(n), "f_obitos"=integer(n), "f_pcasos"=numeric(n), "f_pobitos"=numeric(n), "f_rate"=numeric(n))

age <- data.frame( "aX"=integer(n),  "paX"=numeric(n),  "aX_obitos"=integer(n),  "paX_obitos"=numeric(n),  "paX_rate"=numeric(n),
                   "a0"=integer(n),  "pa0"=numeric(n),  "a0_obitos"=integer(n),  "pa0_obitos"=numeric(n),  "pa0_rate"=numeric(n),
                  "a10"=integer(n), "pa10"=numeric(n), "a10_obitos"=integer(n), "pa10_obitos"=numeric(n), "pa10_rate"=numeric(n), 
                  "a20"=integer(n), "pa20"=numeric(n), "a20_obitos"=integer(n), "pa20_obitos"=numeric(n), "pa20_rate"=numeric(n), 
                  "a30"=integer(n), "pa30"=numeric(n), "a30_obitos"=integer(n), "pa30_obitos"=numeric(n), "pa30_rate"=numeric(n), 
                  "a40"=integer(n), "pa40"=numeric(n), "a40_obitos"=integer(n), "pa40_obitos"=numeric(n), "pa40_rate"=numeric(n), 
                  "a50"=integer(n), "pa50"=numeric(n), "a50_obitos"=integer(n), "pa50_obitos"=numeric(n), "pa50_rate"=numeric(n), 
                  "a60"=integer(n), "pa60"=numeric(n), "a60_obitos"=integer(n), "pa60_obitos"=numeric(n), "pa60_rate"=numeric(n), 
                  "a70"=integer(n), "pa70"=numeric(n), "a70_obitos"=integer(n), "pa70_obitos"=numeric(n), "pa70_rate"=numeric(n), 
                  "a80"=integer(n), "pa80"=numeric(n), "a80_obitos"=integer(n), "pa80_obitos"=numeric(n), "pa80_rate"=numeric(n), 
                  "a90"=integer(n), "pa90"=numeric(n), "a90_obitos"=integer(n), "pa90_obitos"=numeric(n), "pa90_rate"=numeric(n))

medical <- data.frame("asm"=integer(n), "pasm"=numeric(n), "asm_obitos"=integer(n), "pasm_obitos"=numeric(n), "pasm_rate"=numeric(n),
                      "crd"=integer(n), "pcrd"=numeric(n), "crd_obitos"=integer(n), "pcrd_obitos"=numeric(n), "pcrd_rate"=numeric(n),
                      "dia"=integer(n), "pdia"=numeric(n), "dia_obitos"=integer(n), "pdia_obitos"=numeric(n), "pdia_rate"=numeric(n),
                      "hem"=integer(n), "phem"=numeric(n), "hem_obitos"=integer(n), "phem_obitos"=numeric(n), "phem_rate"=numeric(n),
                      "hep"=integer(n), "phep"=numeric(n), "hep_obitos"=integer(n), "phep_obitos"=numeric(n), "phep_rate"=numeric(n),
                      "neu"=integer(n), "pneu"=numeric(n), "neu_obitos"=integer(n), "pneu_obitos"=numeric(n), "pneu_rate"=numeric(n),
                      "ren"=integer(n), "pren"=numeric(n), "ren_obitos"=integer(n), "pren_obitos"=numeric(n), "pren_rate"=numeric(n),
                      "imu"=integer(n), "pimu"=numeric(n), "imu_obitos"=integer(n), "pimu_obitos"=numeric(n), "pimu_rate"=numeric(n),
                      "obs"=integer(n), "pobs"=numeric(n), "obs_obitos"=integer(n), "pobs_obitos"=numeric(n), "pobs_rate"=numeric(n),
                      "pne"=integer(n), "ppne"=numeric(n), "pne_obitos"=integer(n), "ppne_obitos"=numeric(n), "ppne_rate"=numeric(n),
                      "pue"=integer(n), "ppue"=numeric(n), "pue_obitos"=integer(n), "ppue_obitos"=numeric(n), "ppue_rate"=numeric(n),
                      "dow"=integer(n), "pdow"=numeric(n), "dow_obitos"=integer(n), "pdow_obitos"=numeric(n), "pdow_rate"=numeric(n),
                      "out"=integer(n), "pout"=numeric(n), "out_obitos"=integer(n), "pout_obitos"=numeric(n), "pout_rate"=numeric(n),
                      "ncp"=integer(n), "pncp"=numeric(n), "ncp_obitos"=integer(n), "pncp_obitos"=numeric(n), "pncp_rate"=numeric(n),
                      "m0"=integer(n), "pm0"=numeric(n), "m0_obitos"=integer(n), "pm0_obitos"=numeric(n), "pm0_rate"=numeric(n),
                      "m1"=integer(n), "pm1"=numeric(n), "m1_obitos"=integer(n), "pm1_obitos"=numeric(n), "pm1_rate"=numeric(n),
                      "m2"=integer(n), "pm2"=numeric(n), "m2_obitos"=integer(n), "pm2_obitos"=numeric(n), "pm2_rate"=numeric(n),
                      "m3"=integer(n), "pm3"=numeric(n), "m3_obitos"=integer(n), "pm3_obitos"=numeric(n), "pm3_rate"=numeric(n),
                      "m4"=integer(n), "pm4"=numeric(n), "m4_obitos"=integer(n), "pm4_obitos"=numeric(n), "pm4_rate"=numeric(n))

# Get data from all cities
print("0 %")
fs <- 1
for (i in 1:nrow(file)) {
  # File lookup status
  file_status = i / nrow(file)*100
  if (file_status >= 10 * fs ){
    print(paste(as.integer(file_status), "%"))
    fs <- fs + 1
  }

  # City index
  if (identical(file[i, "nome_munic"], "Não Informado")) {
    j <- n
  }
  else {
    j <- match(file[i, "nome_munic"], cities)
  }

  # Personal data
  general[j, "casos"] <- general[j, "casos"] + 1L
  if (file[i, "obito"] == 0) {
    if (identical(file[i, "cs_sexo"], "MASCULINO")) {
      gender[j, "m_casos"] <- gender[j, "m_casos"] + 1L
    }
    else {
      gender[j, "f_casos"] <- gender[j, "f_casos"] + 1L
    }

    # Age ranges
    if (is.na(file[i, "idade"])) {
      age[j, "aX"] <- age[j, "aX"] + 1L
    }
    else if (file[i, "idade"] < 10) {
      age[j, "a0"] <- age[j, "a0"] + 1L
    }
    else if (file[i, "idade"] < 20) {
      age[j, "a10"] <- age[j, "a10"] + 1L
    }
    else if (file[i, "idade"] < 30) {
      age[j, "a20"] <- age[j, "a20"] + 1L
    }
    else if (file[i, "idade"] < 40) {
      age[j, "a30"] <- age[j, "a30"] + 1L
    }
    else if (file[i, "idade"] < 50) {
      age[j, "a40"] <- age[j, "a40"] + 1L
    }
    else if (file[i, "idade"] < 60) {
      age[j, "a50"] <- age[j, "a50"] + 1L
    }
    else if (file[i, "idade"] < 70) {
      age[j, "a60"] <- age[j, "a60"] + 1L
    }
    else if (file[i, "idade"] < 80) {
      age[j, "a70"] <- age[j, "a70"] + 1L
    }
    else if (file[i, "idade"] < 90) {
      age[j, "a80"] <- age[j, "a80"] + 1L
    }
    else {
      age[j, "a90"] <- age[j, "a90"] + 1L
    }

    # Medical conditions
    m <- 0L
    if (identical(file[i, "asma"], "SIM")) {
      medical[j, "asm"] <- medical[j, "asm"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "cardiopatia"], "SIM")) {
      medical[j, "crd"] <- medical[j, "crd"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "diabetes"], "SIM")) {
      medical[j, "dia"] <- medical[j, "dia"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_hematologica"], "SIM")) {
      medical[j, "hem"] <- medical[j, "hem"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_hepatica"], "SIM")) {
      medical[j, "hep"] <- medical[j, "hep"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_neurologica"], "SIM")) {
      medical[j, "neu"] <- medical[j, "neu"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_renal"], "SIM")) {
      medical[j, "ren"] <- medical[j, "ren"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "imunodepressao"], "SIM")) {
      medical[j, "imu"] <- medical[j, "imu"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "obesidade"], "SIM")) {
      medical[j, "obs"] <- medical[j, "obs"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "pneumopatia"], "SIM")) {
      medical[j, "pne"] <- medical[j, "pne"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "puerpera"], "SIM")) {
      medical[j, "pue"] <- medical[j, "pue"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "sindrome_de_down"], "SIM")) {
      medical[j, "dow"] <- medical[j, "dow"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "outros_fatores_de_risco	"], "SIM")) {
      medical[j, "out"] <- medical[j, "out"] + 1L
      m <- m + 1L
    }
    if (m == 0) {
      medical[j, "ncp"] <- medical[j, "ncp"] + 1L
      medical[j, "m0"] <- medical[j, "m0"] + 1L
    }
    else if (m == 1) {
      medical[j, "m1"] <- medical[j, "m1"] + 1L
    }
    else if (m == 2) {
      medical[j, "m2"] <- medical[j, "m2"] + 1L
    }
    else if (m == 3) {
      medical[j, "m3"] <- medical[j, "m3"] + 1L
    }
    else if (m >= 4) {
      medical[j, "m4"] <- medical[j, "m4"] + 1L
    }
  }
  else {
    general[j, "obitos"] <- general[j, "obitos"] + 1L
    if (identical(file[i, "cs_sexo"], "MASCULINO")) {
      gender[j, "m_casos"] <- gender[j, "m_casos"] + 1L
      gender[j, "m_obitos"] <- gender[j, "m_obitos"] + 1L
    }
    else {
      gender[j, "f_casos"] <- gender[j, "f_casos"] + 1L
      gender[j, "f_obitos"] <- gender[j, "f_obitos"] + 1L
    }

    # Age ranges
    if (is.na(file[i, "idade"])) {
      age[j, "aX"] <- age[j, "aX"] + 1L
      age[j, "aX_obitos"] <- age[j, "aX_obitos"] + 1L
    }
    else if (file[i, "idade"] < 10L) {
      age[j, "a0"] <- age[j, "a0"] + 1L
      age[j, "a0_obitos"] <- age[j, "a0_obitos"] + 1L
    }
    else if (file[i, "idade"] < 20) {
      age[j, "a10"] <- age[j, "a10"] + 1L
      age[j, "a10_obitos"] <- age[j, "a10_obitos"] + 1L
    }
    else if (file[i, "idade"] < 30) {
      age[j, "a20"] <- age[j, "a20"] + 1L
      age[j, "a20_obitos"] <- age[j, "a20_obitos"] + 1L
    }
    else if (file[i, "idade"] < 40) {
      age[j, "a30"] <- age[j, "a30"] + 1L
      age[j, "a30_obitos"] <- age[j, "a30_obitos"] + 1L
    }
    else if (file[i, "idade"] < 50) {
      age[j, "a40"] <- age[j, "a40"] + 1L
      age[j, "a40_obitos"] <- age[j, "a40_obitos"] + 1L
    }
    else if (file[i, "idade"] < 60) {
      age[j, "a50"] <- age[j, "a50"] + 1L
      age[j, "a50_obitos"] <- age[j, "a50_obitos"] + 1L
    }
    else if (file[i, "idade"] < 70) {
      age[j, "a60"] <- age[j, "a60"] + 1L
      age[j, "a60_obitos"] <- age[j, "a60_obitos"] + 1L
    }
    else if (file[i, "idade"] < 80) {
      age[j, "a70"] <- age[j, "a70"] + 1L
      age[j, "a70_obitos"] <- age[j, "a70_obitos"] + 1L
    }
    else if (file[i, "idade"] < 90) {
      age[j, "a80"] <- age[j, "a80"] + 1L
      age[j, "a80_obitos"] <- age[j, "a80_obitos"] + 1L
    }
    else {
      age[j, "a90"] <- age[j, "a90"] + 1L
      age[j, "a90_obitos"] <- age[j, "a90_obitos"] + 1L
    }

    # Medical conditions
    m <- 0L
    if (identical(file[i, "asma"], "SIM")) {
      medical[j, "asm"] <- medical[j, "asm"] + 1L
      medical[j, "asm_obitos"] <- medical[j, "asm_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "cardiopatia"], "SIM")) {
      medical[j, "crd"] <- medical[j, "crd"] + 1L
      medical[j, "crd_obitos"] <- medical[j, "crd_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "diabetes"], "SIM")) {
      medical[j, "dia"] <- medical[j, "dia"] + 1L
      medical[j, "dia_obitos"] <- medical[j, "dia_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_hematologica"], "SIM")) {
      medical[j, "hem"] <- medical[j, "hem"] + 1L
      medical[j, "hem_obitos"] <- medical[j, "hem_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_hepatica"], "SIM")) {
      medical[j, "hep"] <- medical[j, "hep"] + 1L
      medical[j, "hep_obitos"] <- medical[j, "hep_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_neurologica"], "SIM")) {
      medical[j, "neu"] <- medical[j, "neu"] + 1L
      medical[j, "neu_obitos"] <- medical[j, "neu_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "doenca_renal"], "SIM")) {
      medical[j, "ren"] <- medical[j, "ren"] + 1L
      medical[j, "ren_obitos"] <- medical[j, "ren_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "imunodepressao"], "SIM")) {
      medical[j, "imu"] <- medical[j, "imu"] + 1L
      medical[j, "imu_obitos"] <- medical[j, "imu_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "obesidade"], "SIM")) {
      medical[j, "obs"] <- medical[j, "obs"] + 1L
      medical[j, "obs_obitos"] <- medical[j, "obs_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "pneumopatia"], "SIM")) {
      medical[j, "pne"] <- medical[j, "pne"] + 1L
      medical[j, "pne_obitos"] <- medical[j, "pne_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "puerpera"], "SIM")) {
      medical[j, "pue"] <- medical[j, "pue"] + 1L
      medical[j, "pue_obitos"] <- medical[j, "pue_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "sindrome_de_down"], "SIM")) {
      medical[j, "dow"] <- medical[j, "dow"] + 1L
      medical[j, "dow_obitos"] <- medical[j, "dow_obitos"] + 1L
      m <- m + 1L
    }
    if (identical(file[i, "outros_fatores_de_risco	"], "SIM")) {
      medical[j, "out"] <- medical[j, "out"] + 1L
      medical[j, "out_obitos"] <- medical[j, "out_obitos"] + 1L
      m <- m + 1L
    }
    if (m == 0) {
      medical[j, "ncp"] <- medical[j, "ncp"] + 1L
      medical[j, "ncp_obitos"] <- medical[j, "ncp_obitos"] + 1L
      medical[j, "m0"] <- medical[j, "m0"] + 1L
      medical[j, "m0_obitos"] <- medical[j, "m0_obitos"] + 1L
    }
    else if (m == 1) {
      medical[j, "m1"] <- medical[j, "m1"] + 1L
      medical[j, "m1_obitos"] <- medical[j, "m1_obitos"] + 1L
    }
    else if (m == 2) {
      medical[j, "m2"] <- medical[j, "m2"] + 1L
      medical[j, "m2_obitos"] <- medical[j, "m2_obitos"] + 1L
    }
    else if (m == 3) {
      medical[j, "m3"] <- medical[j, "m3"] + 1L
      medical[j, "m3_obitos"] <- medical[j, "m3_obitos"] + 1L
    }
    else if (m >= 4) {
      medical[j, "m4"] <- medical[j, "m4"] + 1L
      medical[j, "m4_obitos"] <- medical[j, "m4_obitos"] + 1L
    }
  }
}

for (j in 1:n) {
  # Update percentages
  general[j, "mortalidade"] <- general[j, "obitos"] / general[j, "casos"] * 100
  
  gender[j, "m_pcasos"] <- gender[j, "m_casos"] / general[j, "casos"] * 100
  gender[j, "m_pobitos"] <- gender[j, "m_obitos"] / general[j, "obitos"] * 100
  gender[j, "m_rate"] <- gender[j, "m_obitos"] / gender[j, "m_casos"] * 100

  gender[j, "f_pcasos"] <- gender[j, "f_casos"] / general[j, "casos"] * 100
  gender[j, "f_pobitos"] <- gender[j, "f_obitos"] / general[j, "obitos"] * 100
  gender[j, "f_rate"] <- gender[j, "f_obitos"] / gender[j, "f_casos"] * 100

  age[j, "paX"] <- age[j, "aX"] / general[j, "casos"] * 100
  age[j, "paX_obitos"] <- age[j, "aX_obitos"] / general[j, "obitos"] * 100
  age[j, "paX_rate"] <- age[j, "aX_obitos"] / age[j, "aX"] * 100
  age[j, "pa0"] <- age[j, "a0"] / general[j, "casos"] * 100
  age[j, "pa0_obitos"] <- age[j, "a0_obitos"] / general[j, "obitos"] * 100
  age[j, "pa0_rate"] <- age[j, "a0_obitos"] / age[j, "a0"] * 100
  age[j, "pa10"] <- age[j, "a10"] / general[j, "casos"] * 100
  age[j, "pa10_obitos"] <- age[j, "a10_obitos"] / general[j, "obitos"] * 100
  age[j, "pa10_rate"] <- age[j, "a10_obitos"] / age[j, "a10"] * 100
  age[j, "pa20"] <- age[j, "a20"] / general[j, "casos"] * 100
  age[j, "pa20_obitos"] <- age[j, "a20_obitos"] / general[j, "obitos"] * 100
  age[j, "pa20_rate"] <- age[j, "a20_obitos"] / age[j, "a20"] * 100
  age[j, "pa30"] <- age[j, "a30"] / general[j, "casos"] * 100
  age[j, "pa30_obitos"] <- age[j, "a30_obitos"] / general[j, "obitos"] * 100
  age[j, "pa30_rate"] <- age[j, "a30_obitos"] / age[j, "a30"] * 100
  age[j, "pa40"] <- age[j, "a40"] / general[j, "casos"] * 100
  age[j, "pa40_obitos"] <- age[j, "a40_obitos"] / general[j, "obitos"] * 100
  age[j, "pa40_rate"] <- age[j, "a40_obitos"] / age[j, "a40"] * 100
  age[j, "pa50"] <- age[j, "a50"] / general[j, "casos"] * 100
  age[j, "pa50_obitos"] <- age[j, "a50_obitos"] / general[j, "obitos"] * 100
  age[j, "pa50_rate"] <- age[j, "a50_obitos"] / age[j, "a50"] * 100
  age[j, "pa60"] <- age[j, "a60"] / general[j, "casos"] * 100
  age[j, "pa60_obitos"] <- age[j, "a60_obitos"] / general[j, "obitos"] * 100
  age[j, "pa60_rate"] <- age[j, "a60_obitos"] / age[j, "a60"] * 100
  age[j, "pa70"] <- age[j, "a70"] / general[j, "casos"] * 100
  age[j, "pa70_obitos"] <- age[j, "a70_obitos"] / general[j, "obitos"] * 100
  age[j, "pa70_rate"] <- age[j, "a70_obitos"] / age[j, "a70"] * 100
  age[j, "pa80"] <- age[j, "a80"] / general[j, "casos"] * 100
  age[j, "pa80_obitos"] <- age[j, "a80_obitos"] / general[j, "obitos"] * 100
  age[j, "pa80_rate"] <- age[j, "a80_obitos"] / age[j, "a80"] * 100
  age[j, "pa90"] <- age[j, "a90"] / general[j, "casos"] * 100
  age[j, "pa90_obitos"] <- age[j, "a90_obitos"] / general[j, "obitos"] * 100
  age[j, "pa90_rate"] <- age[j, "a90_obitos"] / age[j, "a90"] * 100

  medical[j, "pasm"] <- medical[j, "asm"] / general[j, "casos"] * 100
  medical[j, "pasm_obitos"] <- medical[j, "asm_obitos"] / general[j, "obitos"] * 100
  medical[j, "pasm_rate"] <- medical[j, "asm_obitos"] / medical[j, "asm"] * 100
  medical[j, "pcrd"] <- medical[j, "crd"] / general[j, "casos"] * 100
  medical[j, "pcrd_obitos"] <- medical[j, "crd_obitos"] / general[j, "obitos"] * 100
  medical[j, "pcrd_rate"] <- medical[j, "crd_obitos"] / medical[j, "crd"] * 100
  medical[j, "pdia"] <- medical[j, "dia"] / general[j, "casos"] * 100
  medical[j, "pdia_obitos"] <- medical[j, "dia_obitos"] / general[j, "obitos"] * 100
  medical[j, "pdia_rate"] <- medical[j, "dia_obitos"] / medical[j, "dia"] * 100
  medical[j, "phem"] <- medical[j, "hem"] / general[j, "casos"] * 100
  medical[j, "phem_obitos"] <- medical[j, "hem_obitos"] / general[j, "obitos"] * 100
  medical[j, "phem_rate"] <- medical[j, "hem_obitos"] / medical[j, "hem"] * 100
  medical[j, "phep"] <- medical[j, "hep"] / general[j, "casos"] * 100
  medical[j, "phep_obitos"] <- medical[j, "hep_obitos"] / general[j, "obitos"] * 100
  medical[j, "phep_rate"] <- medical[j, "hep_obitos"] / medical[j, "hep"] * 100
  medical[j, "pneu"] <- medical[j, "neu"] / general[j, "casos"] * 100
  medical[j, "pneu_obitos"] <- medical[j, "neu_obitos"] / general[j, "obitos"] * 100
  medical[j, "pneu_rate"] <- medical[j, "neu_obitos"] / medical[j, "neu"] * 100
  medical[j, "pren"] <- medical[j, "ren"] / general[j, "casos"] * 100
  medical[j, "pren_obitos"] <- medical[j, "ren_obitos"] / general[j, "obitos"] * 100
  medical[j, "pren_rate"] <- medical[j, "ren_obitos"] / medical[j, "ren"] * 100
  medical[j, "pimu"] <- medical[j, "imu"] / general[j, "casos"] * 100
  medical[j, "pimu_obitos"] <- medical[j, "imu_obitos"] / general[j, "obitos"] * 100
  medical[j, "pimu_rate"] <- medical[j, "imu_obitos"] / medical[j, "imu"] * 100
  medical[j, "pobs"] <- medical[j, "obs"] / general[j, "casos"] * 100
  medical[j, "pobs_obitos"] <- medical[j, "obs_obitos"] / general[j, "obitos"] * 100
  medical[j, "pobs_rate"] <- medical[j, "obs_obitos"] / medical[j, "obs"] * 100
  medical[j, "ppne"] <- medical[j, "pne"] / general[j, "casos"] * 100
  medical[j, "ppne_obitos"] <- medical[j, "pne_obitos"] / general[j, "obitos"] * 100
  medical[j, "ppne_rate"] <- medical[j, "pne_obitos"] / medical[j, "pne"] * 100
  medical[j, "ppue"] <- medical[j, "pue"] / general[j, "casos"] * 100
  medical[j, "ppue_obitos"] <- medical[j, "pue_obitos"] / general[j, "obitos"] * 100
  medical[j, "ppue_rate"] <- medical[j, "pue_obitos"] / medical[j, "pue"] * 100
  medical[j, "pdow"] <- medical[j, "dow"] / general[j, "casos"] * 100
  medical[j, "pdow_obitos"] <- medical[j, "dow_obitos"] / general[j, "obitos"] * 100
  medical[j, "pdow_rate"] <- medical[j, "dow_obitos"] / medical[j, "dow"] * 100
  medical[j, "pout"] <- medical[j, "out"] / general[j, "casos"] * 100
  medical[j, "pout_obitos"] <- medical[j, "out_obitos"] / general[j, "obitos"] * 100
  medical[j, "pout_rate"] <- medical[j, "out_obitos"] / medical[j, "out"] * 100
  medical[j, "pncp"] <- medical[j, "ncp"] / general[j, "casos"] * 100
  medical[j, "pncp_obitos"] <- medical[j, "ncp_obitos"] / general[j, "obitos"] * 100
  medical[j, "pncp_rate"] <- medical[j, "ncp_obitos"] / medical[j, "ncp"] * 100

  medical[j, "pm0"] <- medical[j, "m0"] / general[j, "casos"] * 100
  medical[j, "pm0_obitos"] <- medical[j, "m0_obitos"] / general[j, "obitos"] * 100
  medical[j, "pm0_rate"] <- medical[j, "m0_obitos"] / medical[j, "m0"] * 100
  medical[j, "pm1"] <- medical[j, "m1"] / general[j, "casos"] * 100
  medical[j, "pm1_obitos"] <- medical[j, "m1_obitos"] / general[j, "obitos"] * 100
  medical[j, "pm1_rate"] <- medical[j, "m1_obitos"] / medical[j, "m1"] * 100
  medical[j, "pm2"] <- medical[j, "m2"] / general[j, "casos"] * 100
  medical[j, "pm2_obitos"] <- medical[j, "m2_obitos"] / general[j, "obitos"] * 100
  medical[j, "pm2_rate"] <- medical[j, "m2_obitos"] / medical[j, "m2"] * 100
  medical[j, "pm3"] <- medical[j, "m3"] / general[j, "casos"] * 100
  medical[j, "pm3_obitos"] <- medical[j, "m3_obitos"] / general[j, "obitos"] * 100
  medical[j, "pm3_rate"] <- medical[j, "m3_obitos"] / medical[j, "m3"] * 100
  medical[j, "pm4"] <- medical[j, "m4"] / general[j, "casos"] * 100
  medical[j, "pm4_obitos"] <- medical[j, "m4_obitos"] / general[j, "obitos"] * 100
  medical[j, "pm4_rate"] <- medical[j, "m4_obitos"] / medical[j, "m4"] * 100
}

# State data
s_general <- data.frame("Casos"=sum(general$casos), 
                        "Óbitos"=sum(general$obitos), 
                        "Mortalidade %"=sum(general$obitos)/sum(general$casos)*100, 
                        check.names=FALSE)
s_gender <- data.frame(row.names = c("Masculino", "Feminino"),
                       "Casos"=c(sum(gender$m_casos), sum(gender$f_casos)),
                       'Casos %'=c(sum(gender$m_casos)*100/s_general$Casos, sum(gender$f_casos)*100/s_general$Casos),
                       "Óbitos"=c(sum(gender$m_obitos), sum(gender$f_obitos)),
                       'Óbitos %'=c(sum(gender$m_obitos)*100/s_general$Óbitos, sum(gender$f_obitos)*100/s_general$Óbitos),
                       'Mortalidade %'=c(sum(gender$m_obitos)*100/sum(gender$m_casos), sum(gender$f_obitos)*100/sum(gender$m_casos)),
                       check.names=FALSE)
s_age <- data.frame(row.names = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80 a 89", "90 ou mais", "Sem idade declarada"),
           "Casos"=c(sum(age$a0), sum(age$a10), sum(age$a20), sum(age$a30), sum(age$a40), sum(age$a50), sum(age$a60), sum(age$a70), sum(age$a80), sum(age$a90), sum(age$aX)), 
           'Casos %'=c(sum(age$a0)*100/s_general$Casos, sum(age$a10)*100/s_general$Casos, sum(age$a20)*100/s_general$Casos, sum(age$a30)*100/s_general$Casos, sum(age$a40)*100/s_general$Casos, sum(age$a50)*100/s_general$Casos, sum(age$a60)*100/s_general$Casos, sum(age$a70)*100/s_general$Casos, sum(age$a80)*100/s_general$Casos, sum(age$a90)*100/s_general$Casos, sum(age$aX)*100/s_general$Casos),
           "Óbitos"=c(sum(age$a0_obitos), sum(age$a10_obitos), sum(age$a20_obitos), sum(age$a30_obitos), sum(age$a40_obitos), sum(age$a50_obitos), sum(age$a60_obitos), sum(age$a70_obitos), sum(age$a80_obitos), sum(age$a90_obitos), sum(age$aX_obitos)),
           'Óbitos %'=c(sum(age$a0_obitos)*100/s_general$Óbitos, sum(age$a10_obitos)*100/s_general$Óbitos, sum(age$a20_obitos)*100/s_general$Óbitos, sum(age$a30_obitos)*100/s_general$Óbitos, sum(age$a40_obitos)*100/s_general$Óbitos, sum(age$a50_obitos)*100/s_general$Óbitos, sum(age$a60_obitos)*100/s_general$Óbitos, sum(age$a70_obitos)*100/s_general$Óbitos, sum(age$a80_obitos)*100/s_general$Óbitos, sum(age$a90_obitos)*100/s_general$Óbitos, sum(age$aX_obitos)*100/s_general$Óbitos),
           'Mortalidade %'=c(sum(age$a0_obitos)*100/sum(age$a0), sum(age$a10_obitos)*100/sum(age$a10), sum(age$a20_obitos)*100/sum(age$a20), sum(age$a30_obitos)*100/sum(age$a30), sum(age$a40_obitos)*100/sum(age$a40), sum(age$a50_obitos)*100/sum(age$a50), sum(age$a60_obitos)*100/sum(age$a60), sum(age$a70_obitos)*100/sum(age$a70), sum(age$a80_obitos)*100/sum(age$a80), sum(age$a90_obitos)*100/sum(age$a90), sum(age$aX_obitos)*100/sum(age$aX0)),
           check.names=FALSE)

s_medical <- data.frame(row.names = c("Asma", "Cardiopatia", "Diabetes", "Doença Hematológica", "Doença Hepática", "Doença Neurológica", "Doença Renal", "Imunodepressão", "Obesidade", "Pneumopatia", "Puérpera", "Síndrome de Down", "Outros", "Nenhuma condição pré-existente"),
                        "Casos"=c(sum(medical$asm), sum(medical$crd), sum(medical$dia), sum(medical$hem), sum(medical$hep), sum(medical$neu), sum(medical$ren), sum(medical$imu), sum(medical$obs), sum(medical$pne), sum(medical$pue), sum(medical$dow), sum(medical$out), sum(medical$ncp)), 
                        'Casos %'=c(sum(medical$asm)*100/s_general$Casos, sum(medical$crd)*100/s_general$Casos, sum(medical$dia)*100/s_general$Casos, sum(medical$hem)*100/s_general$Casos, sum(medical$hep)*100/s_general$Casos, sum(medical$neu)*100/s_general$Casos, sum(medical$ren)*100/s_general$Casos, sum(medical$imu)*100/s_general$Casos, sum(medical$obs)*100/s_general$Casos, sum(medical$pne)*100/s_general$Casos, sum(medical$pue)*100/s_general$Casos, sum(medical$dow)*100/s_general$Casos, sum(medical$out)*100/s_general$Casos, sum(medical$ncp)*100/s_general$Casos),
                        "Óbitos"=c(sum(medical$asm_obitos), sum(medical$crd_obitos), sum(medical$dia_obitos), sum(medical$hem_obitos), sum(medical$hep_obitos), sum(medical$neu_obitos), sum(medical$ren_obitos), sum(medical$imu_obitos), sum(medical$obs_obitos), sum(medical$pne_obitos), sum(medical$pue_obitos), sum(medical$dow_obitos), sum(medical$out_obitos), sum(medical$ncp_obitos)),
                        'Óbitos %'=c(sum(medical$asm_obitos)*100/s_general$Óbitos, sum(medical$crd_obitos)*100/s_general$Óbitos, sum(medical$dia_obitos)*100/s_general$Óbitos, sum(medical$hem_obitos)*100/s_general$Óbitos, sum(medical$hep_obitos)*100/s_general$Óbitos, sum(medical$neu_obitos)*100/s_general$Óbitos, sum(medical$ren_obitos)*100/s_general$Óbitos, sum(medical$imu_obitos)*100/s_general$Óbitos, sum(medical$obs_obitos)*100/s_general$Óbitos, sum(medical$pne_obitos)*100/s_general$Óbitos, sum(medical$pue_obitos)*100/s_general$Óbitos, sum(medical$dow_obitos)*100/s_general$Óbitos, sum(medical$out_obitos)*100/s_general$Óbitos, sum(medical$ncp_obitos)*100/s_general$Óbitos), 
                        'Mortalidade %'=c(sum(medical$asm_obitos)*100/sum(medical$asm), sum(medical$crd_obitos)*100/sum(medical$crd), sum(medical$dia_obitos)*100/sum(medical$dia), sum(medical$hem_obitos)*100/sum(medical$hem), sum(medical$hep_obitos)*100/sum(medical$hep), sum(medical$neu_obitos)*100/sum(medical$neu), sum(medical$ren_obitos)*100/sum(medical$ren), sum(medical$imu_obitos)*100/sum(medical$imu), sum(medical$obs_obitos)*100/sum(medical$obs), sum(medical$pne_obitos)*100/sum(medical$pne), sum(medical$pue_obitos)*100/sum(medical$pue), sum(medical$dow_obitos)*100/sum(medical$dow), sum(medical$out_obitos)*100/sum(medical$out), sum(medical$ncp_obitos)*100/sum(medical$ncp)), 
                        check.names=FALSE)

s_nmedical <- data.frame(row.names = c("Nenhuma", "1", "2", "3", "4 ou mais"),
                         "Casos"=c(sum(medical$m0), sum(medical$m1), sum(medical$m2), sum(medical$m3), sum(medical$m4)), 
                         'Casos %'=c(sum(medical$m0)*100/s_general$Casos, sum(medical$m1)*100/s_general$Casos, sum(medical$m2)*100/s_general$Casos, sum(medical$m3)*100/s_general$Casos, sum(medical$m4)*100/s_general$Casos),
                         "Óbitos"=c(sum(medical$m0_obitos), sum(medical$m1_obitos), sum(medical$m2_obitos), sum(medical$m3_obitos), sum(medical$m4_obitos)),
                         'Óbitos %'=c(sum(medical$m0_obitos)*100/s_general$Óbitos, sum(medical$m1_obitos)*100/s_general$Óbitos, sum(medical$m2_obitos)*100/s_general$Óbitos, sum(medical$m3_obitos)*100/s_general$Óbitos, sum(medical$m4_obitos)*100/s_general$Óbitos), 
                         'Mortalidade %'=c(sum(medical$m0_obitos)*100/sum(medical$m0), sum(medical$m1_obitos)*100/sum(medical$m1), sum(medical$m2_obitos)*100/sum(medical$m2), sum(medical$m3_obitos)*100/sum(medical$m3), sum(medical$m4_obitos)*100/sum(medical$m4)),
                         check.names=FALSE)

ui <- fluidPage(
  titlePanel("Covid-19 SP - Estatísticas"),
  selectInput("city", "Selecione a localidade", choices = c("Estado", cities), selected = "Estado"),
  
  textOutput("title", container = h2),
  tableOutput("general"),
  br(),
  
  textOutput("t_gender", container = h3),
  tableOutput("gender"),
  br(),
  
  textOutput("t_age", container = h3),
  tableOutput("age"),
  br(),
  
  textOutput("t_medical", container = h3),
  tableOutput("medical"),
  br(),
  
  textOutput("t_medical2", container = h3),
  tableOutput("medical2"),
  br(),
  
  hr(),
  p("2020 - Rodolfo Vick - rodolfo.vick at gmail.com"),
  br()
)

server <- function(input, output, session) {
  observe({
    #General
    if (identical(input$city, "Estado")){
      j <- 1
      
      output$title <- renderText({
        paste("Dados totais do Estado de São Paulo em ", as.character(day, "%d/%m/%Y"))
      })
      output$general <- renderTable({
        s_general
      })
      
      # Gender
      output$t_gender <- renderText({
        "Dados por sexo"
      })
      output$gender <- renderTable({
        s_gender
      }, rownames=TRUE)
      
      # Age
      output$t_age <- renderText({
        "Dados por idade"
      })
      output$age <- renderTable({
        s_age
      }, rownames=TRUE)
      
      # Medical information
      output$t_medical <- renderText({
        "Condições médicas pré-existentes"
      })
      output$medical <- renderTable({
        s_medical
      }, rownames=TRUE)
      
      output$t_medical2 <- renderText({
        "Número de condições médicas pré-existentes"
      })
      output$medical2 <- renderTable({
        s_nmedical
      }, rownames=TRUE)
    }
    else {
      # Get data
      j <- match(input$city, cities)
      
      # General
      output$title <- renderText({
        paste("Dados de ", input$city, " em ", as.character(day, "%d/%m/%Y"))
      })
      output$general <- renderTable({
        data.frame("Casos"=general[j, "casos"], 
                   "Óbitos"=general[j, "obitos"], 
                   "Mortalidade %"=general[j, "mortalidade"],
                   check.names=FALSE)
      })
      
      # Gender
      output$t_gender <- renderText({
        "Dados por sexo"
      })
      output$gender <- renderTable({
        data.frame(row.names = c("Masculino", "Feminino"), 
                   "Casos"=c(gender[j, "m_casos"], gender[j, "f_casos"]), 
                   'Casos %'=c(gender[j, "m_pcasos"], gender[j, "f_pcasos"]),
                   "Óbitos"=c(gender[j, "m_obitos"], gender[j, "f_obitos"]),
                   'Óbitos %'=c(gender[j, "m_pobitos"], gender[j, "f_pobitos"]),
                   'Mortalidade %'=c(gender[j, "m_rate"], gender[j, "f_rate"]), 
                   check.names=FALSE)
      }, rownames=TRUE)
      
      # Age
      output$t_age <- renderText({
        "Dados por idade"
      })
      output$age <- renderTable({
        data.frame(row.names = c("0 a 9", "10 a 19", "20 a 29", "30 a 39", "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80 a 89", "90 ou mais", "Sem idade declarada"),
                   "Casos"=c(age[j, "a0"], age[j, "a10"], age[j, "a20"], age[j, "a30"], age[j, "a40"], age[j, "a50"], age[j, "a60"], age[j, "a70"], age[j, "a80"], age[j, "a90"], age[j, "aX"]), 
                   'Casos %'=c(age[j, "pa0"], age[j, "pa10"], age[j, "pa20"], age[j, "pa30"], age[j, "pa40"], age[j, "pa50"], age[j, "pa60"], age[j, "pa70"], age[j, "pa80"], age[j, "pa90"], age[j, "paX"]),
                   "Óbitos"=c(age[j, "a0_obitos"], age[j, "a10_obitos"], age[j, "a20_obitos"], age[j, "a30_obitos"], age[j, "a40_obitos"], age[j, "a50_obitos"], age[j, "a60_obitos"], age[j, "a70_obitos"], age[j, "a80_obitos"], age[j, "a90_obitos"], age[j, "aX_obitos"]),
                   'Óbitos %'=c(age[j, "pa0_obitos"], age[j, "pa10_obitos"], age[j, "pa20_obitos"], age[j, "pa30_obitos"], age[j, "pa40_obitos"], age[j, "pa50_obitos"], age[j, "pa60_obitos"], age[j, "pa70_obitos"], age[j, "pa80_obitos"], age[j, "pa90_obitos"], age[j, "paX_obitos"]),
                   'Mortalidade %'=c(age[j, "pa0_rate"], age[j, "pa10_rate"], age[j, "pa20_rate"], age[j, "pa30_rate"], age[j, "pa40_rate"], age[j, "pa50_rate"], age[j, "pa60_rate"], age[j, "pa70_rate"], age[j, "pa80_rate"], age[j, "pa90_rate"], age[j, "paX_rate"]),
                   check.names=FALSE)
      }, rownames=TRUE)
      
      # Medical information
      output$t_medical <- renderText({
        "Condições médicas pré-existentes"
      })
      output$medical <- renderTable({
        data.frame(row.names = c("Asma", "Cardiopatia", "Diabetes", "Doença Hematológica", "Doença Hepática", "Doença Neurológica", "Doença Renal", "Imunodepressão", "Obesidade", "Pneumopatia", "Puérpera", "Síndrome de Down", "Outros", "Nenhuma condição pré-existente"),
                   "Casos"=c(medical[j, "asm"], medical[j, "crd"], medical[j, "dia"], medical[j, "hem"], medical[j, "hep"], medical[j, "neu"], medical[j, "ren"], medical[j, "imu"], medical[j, "obs"], medical[j, "pne"], medical[j, "pue"], medical[j, "dow"], medical[j, "out"], medical[j, "ncp"]), 
                   'Casos %'=c(medical[j, "pasm"], medical[j, "pcrd"], medical[j, "pdia"], medical[j, "phem"], medical[j, "phep"], medical[j, "pneu"], medical[j, "pren"], medical[j, "pimu"], medical[j, "pobs"], medical[j, "ppne"], medical[j, "ppue"], medical[j, "pdow"], medical[j, "pout"], medical[j, "pncp"]),
                   "Óbitos"=c(medical[j, "asm_obitos"], medical[j, "crd_obitos"], medical[j, "dia_obitos"], medical[j, "hem_obitos"], medical[j, "hep_obitos"], medical[j, "neu_obitos"], medical[j, "ren_obitos"], medical[j, "imu_obitos"], medical[j, "obs_obitos"], medical[j, "pne_obitos"], medical[j, "pue_obitos"], medical[j, "dow_obitos"], medical[j, "out_obitos"], medical[j, "ncp_obitos"]),
                   'Óbitos %'=c(medical[j, "pasm_obitos"], medical[j, "pcrd_obitos"], medical[j, "pdia_obitos"], medical[j, "phem_obitos"], medical[j, "phep_obitos"], medical[j, "pneu_obitos"], medical[j, "pren_obitos"], medical[j, "pimu_obitos"], medical[j, "pobs_obitos"], medical[j, "ppne_obitos"], medical[j, "ppue_obitos"], medical[j, "pdow_obitos"], medical[j, "pout_obitos"], medical[j, "pncp_obitos"]), 
                   'Mortalidade %'=c(medical[j, "pasm_rate"], medical[j, "pcrd_rate"], medical[j, "pdia_rate"], medical[j, "phem_rate"], medical[j, "phep_rate"], medical[j, "pneu_rate"], medical[j, "pren_rate"], medical[j, "pimu_rate"], medical[j, "pobs_rate"], medical[j, "ppne_rate"], medical[j, "ppue_rate"], medical[j, "pdow_rate"], medical[j, "pout_rate"], medical[j, "pncp_rate"]), 
                   check.names=FALSE)
      }, rownames=TRUE)
      
      output$t_medical2 <- renderText({
        "Número de condições médicas pré-existentes"
      })
      output$medical2 <- renderTable({
        data.frame(row.names = c("Nenhuma", "1", "2", "3", "4 ou mais"),
                   "Casos"=c(medical[j, "m0"], medical[j, "m1"], medical[j, "m2"], medical[j, "m3"], medical[j, "m4"]), 
                   'Casos %'=c(medical[j, "pm0"], medical[j, "pm1"], medical[j, "pm2"], medical[j, "pm3"], medical[j, "pm4"]),
                   "Óbitos"=c(medical[j, "m0_obitos"], medical[j, "m1_obitos"], medical[j, "m2_obitos"], medical[j, "m3_obitos"], medical[j, "m4_obitos"]),
                   'Óbitos %'=c(medical[j, "pm0_obitos"], medical[j, "pm1_obitos"], medical[j, "pm2_obitos"], medical[j, "pm3_obitos"], medical[j, "pm4_obitos"]), 
                   'Mortalidade %'=c(medical[j, "pm0_rate"], medical[j, "pm1_rate"], medical[j, "pm2_rate"], medical[j, "pm3_rate"], medical[j, "pm4_rate"]),
                   check.names=FALSE)
      }, rownames=TRUE)
    }
  })
}

app <- shinyApp(ui, server)
runApp(app, port = 3072, host = "0.0.0.0")
