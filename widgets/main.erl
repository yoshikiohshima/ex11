-module(main). 
-author(skvamme).
-export([start/0,init/0,loop/0]).
-import(ex11_lib,[xColor/2,xCreateSimpleWindow/7,eMapWindow/1,xDo/2,xFlush/1,xSetScreenSaver/2]).
-include("ex11_lib.hrl").
-define(WT,480).
-define(HT,800).

start() -> spawn(?MODULE,init,[]).

init() ->
	Pid = self(),
	{ok, Display} = ex11_lib:xStart("3.1"),
    xSetScreenSaver(Display,0),
	Win = xCreateSimpleWindow(Display,0,0,?WT,?HT,?XC_arrow,xColor(Display,?black)),
	xDo(Display, eMapWindow(Win)),
	xFlush(Display),
%	circleslider:make(Pid,Display,Win,100,100),
%	barcode:make(Pid,Display,Win,20,20),
	dialerbutton:make(Pid, Display,Win,  20,  80,"1"),
	loop().

loop() ->
    receive
		Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
		?MODULE:loop()
	end.


