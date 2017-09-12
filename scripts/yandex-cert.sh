#!/bin/bash

container=$1
allCAs=$2

if [ ! -e "$container" ] || [ ! -e "$allCAs" ]; then
    echo "usage: [pfx container] [allCAs.pem]"
    exit
fi

cp $allCAs allCAs.pem

echo "container pass"
read cont_pass

echo "Import pass"
read pass

echo -n $pass > pass

openssl pkcs12 -in $container -clcerts -out certificate.pem -passin pass:"$cont_pass" -passout pass:"$pass"
openssl pkcs12 -in $container -nocerts -out private-key.key -passin pass:"$cont_pass" -passout pass:"$pass"

chmod 400 *
