#!/usr/bin/env bash
set -e
CERT=${1}
REMOTE_HOST=${2}
REMOTE_USER=${3}
if [ "${4}" == "reload" ]; then
  RELOAD_RESOURCES=true
else
  RELOAD_RESOURCES=false
fi

echo =====================================
echo Releasing to ${REMOTE_USER}@${REMOTE_HOST}. Cert: ${CERT}
echo Reload resources = ${RELOAD_RESOURCES}
echo =====================================

read -p "Press enter to continue"

REMOTE_FOLDER="~/api"

API_SERVICE_NAME="resmat-api.service"

JAR_FILE="/Users/amozh/Documents/prj/knuca/resmat/target/scala-2.11/resmat-assembly-1.0.jar"
RESOURCES_FOLDER="/Users/amozh/Documents/prj/knuca/resmat/src/main/resources"
RESOURCES=(
  "application.conf"
  "flyway.conf"
  "logback.xml"
)

function apiService {
    echo APIService ${1}
    runSSH "sudo systemctl ${1} ${API_SERVICE_NAME}"
}

function runSSH {
    COMMAND_TO_RUN="${1}"
    ssh -i ${CERT} ${REMOTE_USER}@${REMOTE_HOST} ${COMMAND_TO_RUN}
}

function scpToRemote {
  LOCAL_FILE=${1}
  REMOTE_FOLDER_TO_PLACE_FILE=${2}
  SCP_OPTS=${3}
  scp -i ${CERT} ${SCP_OPTS} ${LOCAL_FILE} ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_FOLDER_TO_PLACE_FILE}
}

function prepare {
  if [ ${RELOAD_RESOURCES} = true ]; then
    echo "Clearing out RESOURCES in" ${REMOTE_FOLDER}
    for i in ${RESOURCES[@]}; do
      local RM_FILE_COMMAND='rm -f '${REMOTE_FOLDER}"/"${i}
      runSSH "${RM_FILE_COMMAND}"
    done
  fi
}

runSSH "mkdir -p "${REMOTE_FOLDER}

echo Reloading JAR file
scpToRemote ${JAR_FILE} ${REMOTE_FOLDER}

apiService stop

prepare

if [ ${RELOAD_RESOURCES} = true ]; then
    echo "Uploading RESOURCES into" ${REMOTE_FOLDER}
    for i in ${RESOURCES[@]}; do
      scpToRemote ${RESOURCES_FOLDER}/${i} ${REMOTE_FOLDER}
    done
fi

apiService start

exit
