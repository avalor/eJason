-module(scanner).

-compile(export_all).

-import(file).

getTokens(File)->
    {ok,F} = file:open(File,[read]),
    scan(F).
    

scan(Inport) ->
    scan(Inport, '', 1).


scan(Inport, Prompt, Line1) ->
    case catch io:scan_erl_form(Inport, Prompt, Line1) of
	{eof, Line2} ->
	    {eof, Line2};
	{ok, Tokens, Line2} ->
	    case Tokens of
		[] ->
		    scan(Inport, Prompt, Line2);
		_ ->
		    Toks = lex(Tokens),
		    {Stat,Scan} = scan(Inport,Prompt,Line2),
		    case Stat of
			ok ->
			    {ok,[[Toks]|[Scan]]};
			eof ->
			    {ok,Toks};			    
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
%    io:format("Token: ~p~n",[Token]),
    case Token of
	{'fun',Line} ->
	    [{atom,Line,'fun'}|lex(Tokens)];
        {'-',Line}->
	    case Tokens of
		[{integer,_Line2,Int}|MoreTokens] ->
		    [{number, Line, -Int}|lex(MoreTokens)];
		[{float,_Line2,Float}|MoreTokens] ->
		    [{number, Line, -Float}|lex(MoreTokens)];
		%% [{var,Line2,Var}|MoreTokens] ->
		%%     [Token, {arith_var, Line2, Var}|lex(MoreTokens)];
		[{'+',_Line2}|MoreTokens] ->
		    [{'-+', Line}|lex(MoreTokens)];
		_ ->
		    [{'-', Line} | lex(Tokens)]
	    end;


%	{'[', Line} ->
%	    {Items,Rest} = lex(Tokens),
%	    [{list, Line} | lex(Tokens)]
%	    end;



	{'!', Line} ->
	    case Tokens of
		[{'!',_Line2}|MoreTokens] ->
		    [{'!!', Line}|lex(MoreTokens)];
		_ ->
		    [{'!', Line} | lex(Tokens)]
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
		[{'/',Line}|MoreTokens]->
		    lex(skipLine(Line,MoreTokens));
		[{'*',Line}|MoreTokens]->
		    lex(skipSome(MoreTokens));
		_ ->
		    [{'/',Line}|lex(Tokens)]
	    end;
	{'*', Line} ->
	    case Tokens of
		[{'*',_Line2}|MoreTokens] ->
		    [{'**', Line}|lex(MoreTokens)];
		_ ->
		    [{'*', Line} | lex(Tokens)]
	    end;
	{'=', Line} ->
	    case Tokens of
		[{'..',_Line2}|MoreTokens] ->
		    [{'=..', Line}|lex(MoreTokens)];
		_ ->
		    [{'=', Line} | lex(Tokens)]
	    end;
	{var, Line, VarName}->
	    case Tokens of
		[{'(',_OtherLine}|_] ->
		    [{var_fun,Line,VarName}|
		     lex(Tokens)];
		[{'[',_OtherLine}|_] ->
		    [{var_label,Line,VarName}|
		     lex(Tokens)];
		_ ->
		    [{var,Line,VarName}|
		    lex(Tokens)]
	    end;
        {atom, Line, true} ->
	    [{true,Line}| lex(Tokens)];
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
skipLine(Line,[{_,Line}|MoreTokens]) ->
    skipLine(Line,MoreTokens);
skipLine(Line,[{_,Line,_}|MoreTokens]) ->
    skipLine(Line,MoreTokens);
skipLine(_Line,MoreTokens) ->
    MoreTokens.


%% Skipping several elements between symbos '/*' '*/'

skipSome([])->
    [];
skipSome([{'*',Line},{'/',Line}|MoreTokens])->
    MoreTokens;
skipSome([_Token|MoreTokens]) ->
    %io:format("More: ~p~n",[MoreTokens]),
    skipSome(MoreTokens).
