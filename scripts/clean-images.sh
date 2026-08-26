#!/bin/bash
if which docker; then
  docker ps -a | awk '{print $1}' | xargs --no-run-if-empty docker rm
  docker rmi $(docker images -f dangling=true -q)
fi

if which podman; then
  podman ps -a | awk '{print $1}' | xargs --no-run-if-empty podman rm
  podman rmi $(podman images -f dangling=true -q)
fi
