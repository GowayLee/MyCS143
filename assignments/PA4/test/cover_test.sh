#!/bin/bash
declare main_path=$(dirname $0)"/.."
declare cool_path=$(dirname $0)"/../../../examples"
echo "Start testing for sement with cool files in "$cool_path


for i in $(ls $cool_path | grep .cl)
do
  file_path=$cool_path"/"$i
  echo "Testing "$file_path
  err_count=$($main_path"/mysemant" $file_path | grep -E "line" | wc -l)
  if ((err_count > 0 | $? != 0)); then
    echo -e "\tError found in "$file_path
  else
    echo -e "\tNo error found in "$file_path
  fi
  echo
done