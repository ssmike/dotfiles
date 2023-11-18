ENV_PATH=$1

PIP=$ENV_PATH/bin/pip

$PIP list --outdated --format=freeze | "grep" -v '^\-e' | cut -d = -f 1 | xargs -n1 $PIP install -U
