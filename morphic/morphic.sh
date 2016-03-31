erlc -I../lib *.erl
if [ $? -eq 0 ]
then
  erl -noshell -pa '../lib' -s morphic init
fi

