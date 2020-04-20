#!/usr/bin/env bash
set -e

ENV=${1} #environment vm/aws
RELEASE_PARAM=${2} #reload resources true/false

case ${ENV} in
    vm)
        CERT=~/.ssh/vm_ssh.pem
        REMOTE_HOST=192.168.2.146
        REMOTE_USER=resmat
    ;;
    knuca)
        CERT=~/.ssh/aws_key_pair.pem
        REMOTE_HOST=ec2-52-57-195-49.eu-central-1.compute.amazonaws.com
        REMOTE_USER=ubuntu
    ;;
    zmi)
        CERT=~/.ssh/aws_key_pair.pem
        REMOTE_HOST=ec2-3-120-209-125.eu-central-1.compute.amazonaws.com
        REMOTE_USER=ubuntu
    ;;
    *)
        echo Invalid env ${ENV}
        exit 0
    ;;
esac

./release.sh ${CERT} ${REMOTE_HOST} ${REMOTE_USER} ${RELEASE_PARAM}

exit
