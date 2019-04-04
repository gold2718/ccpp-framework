#! /bin/bash

lname="caplocal.txt"
if [ -f "${lname}" ]; then
  rm ${lname}
fi
touch ${lname}
for file in `cat ${1}`; do
  echo "`basename ${file} .F90`.o" >> ${lname}
done
