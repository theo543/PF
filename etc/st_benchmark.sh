set -e

printf "Benchmarking st...\n"

while true
do
    printf "\nBegin iteration...\n"
    ./st --pure < doubles.txt
    ./st --st < doubles.txt
done
