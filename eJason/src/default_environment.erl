-module(default_environment).

-compile(export_all).

sum(A,B)->
    smart_env:sum(A,B).


read()->
    io:read("").

read(Prompt)->
    try
	Res = io:read(Prompt),%,
%	io:format("Res: ~p~n",[Res]),
	Res
    catch
	A:B->
	    io:format("Read error: ~p:~p~n",[A,B]),
	    false
    end.


makelist(A,B,C)->
    [A,B,C].


readfile(File)->
%    io:format("File: ~p~n",[File]),
    file:read_file(File).

format(File,Name,First,Second,Third,AgentName)->
    io_lib:format(replace_cr(File), 
		  [Name,AgentName,First,Second,Third,AgentName]).
  

replace_cr(List)when is_list(List)->
    lists:map(fun rep/1,List).


rep($\n)->
    $\r;
rep(A) ->
    A.




mail(Address,Message,Subject)->
        Sub = 
	case Subject of
	    _ when is_atom(Subject)->
		string:strip(atom_to_list(Subject),both,$');
	    _ when is_list(Subject) ->
		Subject
	end,

    Res = os:cmd("echo "++
		 Message++" | mutt -s '"++
	  Sub++
	   "' "++
	   string:strip(atom_to_list(Address),both,$')),
    io:format("Res: ~p~n",[Res]).
  


prueba(A,B,C,D,E)->
    io:format("A: ~p~nB: ~p~nC: ~p~nD: ~p~nE: ~p~n",[A,B,C,D,E]).
