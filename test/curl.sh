my_req="https://www.google.com/search?q=quit%20vim%20site%3Astackoverflow.com%20OR%20site%3Astackexchange.com%20OR%20site%3Asuperuser.com%20OR%20site%3Aserverfault.com%20OR%20site%3Aaskubuntu.com&hl=en"


my_agent=("Mozilla/5.0 (X11; Linux i686; rv:109.0) Gecko/20100101 Firefox/118.0")

# my_agent=("foo" "bar")

# echo ${my_agent[4]}

for i in ${!my_agent[@]}; do
    echo $i ${my_agent[$i]}
   curl -k -X GET $my_req -A "${my_agent[$i]}" > google.$i.html
done 

# curl -k -X GET $my_req -A "${my_agent[0]}" >> google.0.html
# curl -k -X GET $my_req -A $my_agent[1] >> google.1.html
# curl -k -X GET $my_req -A $my_agent[2] >> google.2.html
# curl -k -X GET $my_req -A $my_agent[3] >> google.3.html
# curl -k -X GET $my_req -A $my_agent[4] >> google.4.html
# curl -k -X GET $my_req -A $my_agent[5] >> google.5.html


# bellow are not for test but for investigate
my_single_agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:11.0) Gecko/20100101 Firefox/11.0"

for i in {1..5}; do
    curl -k -X GET $my_req -A "${my_single_agent}" > google.single.$i.html
done


my_chrome_agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_4) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.46 Safari/536.5"

for i in {1..5}; do
    curl -k -X GET $my_req -A "${my_chrome_agent}" > google.chrome.$i.html
done

my_latest_chrome_agent="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36"

for i in {1..5}; do
    curl -k -X GET $my_req -A "${my_latest_chrome_agent}" > google.latest_chrome.$i.html
done
