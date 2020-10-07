#! /bin/bash

pid1=0
pid2=0
last=""
now=""

function start {
  # Download CSVs
  rm dados_covid_sp.csv
  wget https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv

  rm plano_sp_leitos_internacoes.csv
  wget https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes.csv

  rm casos_obitos_doencas_preexistentes.csv
  wget https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/casos_obitos_doencas_preexistentes.csv.zip

  dir=$PWD
  cp casos_obitos_doencas_preexistentes.csv.zip /tmp
  cd /tmp
  unzip casos_obitos_doencas_preexistentes.csv.zip
  cp home/lasagna/dados-covid-sp/data/casos_obitos_doencas_preexistentes.csv $dir
  rm casos_obitos_doencas_preexistentes.csv.zip
  rm -r home
  cd $dir
  rm casos_obitos_doencas_preexistentes.csv.zip
  
  # Start main script
  Rscript covid-sp.R &
  pid1=$!
  
  # Start stats script
  Rscript covid-sp-stat.R &
  pid2=$!
  
  echo 
  echo PID 1: $pid1
  echo PID 2: $pid2
  echo
}

function stop {
  # Stop processes
  if [[ "%pid1" != "0" ]]; then
    kill -9 $pid1
  fi
  if [[ "%pid2" != "0" ]]; then
    kill -9 $pid2
  fi
}

function date {
  a=`curl -s "https://api.github.com/repos/seade-R/dados-covid-sp/commits?path=data/dados_covid_sp.csv&page=1&per_page=1" | grep date | sed -n 2p`
  echo $a | cut -f2 -d" "
}

start
last=`date`

echo
echo Initial commit date: $last
echo

# Loop forever
while true; do
  now=`date`
  if [[ "$last" != "$now" ]]; then
    stop
    start
    last=$now
    echo
    echo Last commit date: $last
    echo
  fi
  sleep 1800
done