-module(scanner).

-compile(export_all).

-include("include/macros.hrl").

getTokens(File)->
    {ok, RawTokens} =
	case file:path_open(code:get_path(),File,[read]) of
	    {ok, F, _Path} ->
		scan(F);
	    {error, enoent} ->
		io:format("File: ~s cannot be found.~n",[File]),
		exit(asl_file_not_found)
	end,
    %% io:format("RAW TOKENS: ~p~n",[lists:flatten(RawTokens)]),
    {ok, lex(lists:flatten(RawTokens))}.

    

scan(Inport) ->
    scan(Inport, '', 1).


scan(Inport, Prompt, Line1) ->
    case catch io:scan_erl_form(Inport, Prompt, Line1) of
	{eof, Line2} ->
	    {eof, Line2};
	{ok, Tokens, Line2} ->
	    %%io:format("RAW TOKENS: ~p~n",[Tokens]),

	    case Tokens of
		[] ->
		    scan(Inport, Prompt, Line2);
		_ ->
		    %%Toks = lex(Tokens),
		    ResultScan = scan(Inport,Prompt,Line2),
		    %%io:format("Result Scan: ~p~n",[ResultScan]),
		    {Stat,Scan} = ResultScan,
		    case Stat of
			ok ->
			    {ok,[[Tokens]|[Scan]]};
			eof ->
			    {ok,Tokens};			    
			_ ->
			    {Stat,Scan}
		    end
	    end;
	{error, Descriptor, Line2} ->
	    {error, Descriptor, Line2};
	{'EXIT', Why} ->
	    io:format('yeccscan: Error scanning input line ~w~n', [Line1]),
	    exit(Why)
    end.


lex([]) ->
    [];
lex([Token | Tokens]) ->
%  io:format("Token: ~p~n",[Token]),
    case Token of
	{'fun',Line} ->
	    [{atom,Line,'fun'}|lex(Tokens)];
        {'-',Line}->
	    case Tokens of
	
		%% [{integer,Line2,Int}|MoreTokens] ->
		%%     %% -Int -> + (-1*Int)
		%%     [{'+',Line2},{'(',Line2},{number,Line2,-1},{'*',Line2},
		%%      {number, Line2, Int}, {')',Line2}|lex(MoreTokens)];
		%% [{float,Line2,Float}|MoreTokens] ->
		%%     %% -Float -> + (-1*Float)
		%%     [{'+',Line2},{'(',Line2},{number,Line2,-1},{'*',Line2},
		%%      {number, Line2, Float}, {')',Line2}|lex(MoreTokens)];
	
		%% [{var,Line2,Var}|MoreTokens] ->
		%%     %% -Var -> + (-1*Var)
		%%     [{'+',Line2},{'(',Line2},{number,Line2,-1},{'*',Line2},
		%%      {var, Line2, Var}, {')',Line2}|lex(MoreTokens)];
		[{integer,_Line2,Int}|MoreTokens] ->
		    [{neg_number, Line, -Int}|lex(MoreTokens)];
		[{float,_Line2,Float}|MoreTokens] ->	   
		    [{neg_number, Line, -Float}|lex(MoreTokens)];

		%% [{var,Line2,Var}|MoreTokens] ->
		%%     [Token, {arith_var, Line2, Var}|lex(MoreTokens)];

		[{'+',_Line2}|MoreTokens] ->
		    [{'-+', Line}|lex(MoreTokens)];
		_ ->
		    [{'-', Line} | lex(Tokens)]
	    end;


	{'--',Line}->
	    lex([{'-',Line},{'-',Line}|Tokens]);
	
	{':', Line} ->
	    case Tokens of
		[{'-',_Line2}|MoreTokens] ->
		    [{':-', Line}|lex(MoreTokens)];
		_ ->
		    [{':', Line} | lex(Tokens)]
	    end;



	{'!', Line} ->
	    case Tokens of
		[{'!',_Line2}|MoreTokens] ->
		    [{'!!', Line}|lex(MoreTokens)];
		_ ->
		    [{'!', Line} | lex(Tokens)]
	    end;
	{'?', Line} ->
	    case Tokens of
		[{'?',_Line2}|MoreTokens] ->
		    [{'??', Line}|lex(MoreTokens)];
		_ ->
		    [{'?', Line} | lex(Tokens)]
	    end;
	{'{', Line} ->
	    case Tokens of
		[{'{',_Line2}|MoreTokens] ->
		    [{'{{', Line}|lex(MoreTokens)];
		_ ->
		    [{'{', Line} | lex(Tokens)]
	    end;
	{'}', Line} ->
	    case Tokens of
		[{'}',_Line2}|MoreTokens] ->
		    [{'}}', Line}|lex(MoreTokens)];
		_ ->
		    [{'}', Line} | lex(Tokens)]
	    end;


	{'\\', Line} ->
	    case Tokens of
		[{'==',_Line2}|MoreTokens] ->
		    [{'\\==', Line}|lex(MoreTokens)];
		
		_ ->
		    [{'\\', Line} | lex(Tokens)]
	    end;
	{'/',Line}->
	    case Tokens of
		[{'/',_Line}|MoreTokens]->
		    lex(skipLine(Line,MoreTokens));
		[{'*',_Line}|MoreTokens]->
		    lex(skipSome(MoreTokens));
		_ ->
		    [{'/',Line}|lex(Tokens)]
	    end;
	{'*', Line} ->
	    case Tokens of
		[{'*',_Line2}|MoreTokens] ->
		    [{'**', Line}|lex(MoreTokens)];
		[ {'-',_Line1},{integer,_Line2,Int}|MoreTokens]->
		    [{'*', Line},{number,_Line2,-Int}|lex(MoreTokens)];
		[ {'-',_Line1},{float,_Line2,Float}|MoreTokens]->
		    [{'*', Line},{number,_Line2,-Float}|lex(MoreTokens)];
		_ ->
		    [{'*', Line} | lex(Tokens)]
	    end;
	{'=', Line} ->
	    case Tokens of
		[{'..',_Line2}|MoreTokens] ->
		    [{'=..', Line}|lex(MoreTokens)];
		[{string,SomeLine,String}|MoreTokens]->
		    [{'=', Line},{rel_string,SomeLine,String}|
		     lex(MoreTokens)];		     
		_ ->
		    [{'=', Line} | lex(Tokens)]
	    end;
	{var, Line, VarName}->
	    NewVarName = 
		case VarName of
		    '_' ->
			?UNDERSCORE;
		    _ ->
			list_to_atom(lists:flatten("Ejason_"++
						   atom_to_list(VarName)))
		end,
	     % io:format("NewVarName: ~p~n",[NewVarName]), 
	    case Tokens of
		
		[{'[',_OtherLine}|_] ->
		    [{var_label,Line,NewVarName}|
		     lex(Tokens)];
		_ ->
		    [{var,Line,NewVarName}|
		     lex(Tokens)]
	    end;
        %% {atom, Line, true} ->
	%%     [{true,Line}| lex(Tokens)];
	%% {atom, Line, false} ->
	%%     [{false,Line}| lex(Tokens)];
	{atom, Line, mod} ->
	    [{mod,Line}| lex(Tokens)];
	{reserved_word, Line, 'div'}->
	    [{reserved_word_div,Line}| lex(Tokens)];
	{integer, Line, Int}->
	    [{number,Line, Int}| lex(Tokens)];
	{float, Line, Float}->
	    [{number,Line,Float}| lex(Tokens)];

	{Category, Line, Symbol} ->
	    [{Category, Line, Symbol} | lex(Tokens)];
	{Other, Line} ->
            %% Cat = case erl_scan:reserved_word(Other) of
            %%           true -> reserved_word;
            %%           false -> reserved_symbol
            %%       end,
            [{Other, Line} | lex(Tokens)]
    end.


%% Line skipped due to comments symbol
skipLine(Line,[_Skipped = {_,Line}|MoreTokens]) ->
    %% io:format("Skipping: ~p~n",[Skipped]),
    skipLine(Line,MoreTokens);
skipLine(Line,[_Skipped = {_,Line,_}|MoreTokens]) ->
    %% io:format("Skipping: {~p, ~n",[Skipped]),
    skipLine(Line,MoreTokens);
skipLine(_Line,MoreTokens) ->
    %% io:format("Stopped skipping at: ~p~n",[MoreTokens]),
    MoreTokens.


%% Skipping several elements between symbos '/*' '*/'

skipSome([])->
    [];
skipSome([{'*',Line},{'/',Line}|MoreTokens])->
    MoreTokens;
skipSome([_Token|MoreTokens]) ->
    %io:format("More: ~p~n",[MoreTokens]),
    skipSome(MoreTokens).
