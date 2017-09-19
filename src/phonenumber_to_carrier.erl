-module(phonenumber_to_carrier).
-author("silviu.caragea").

-behaviour(gen_server).

-define(PLUS_SIGN_DIGIT, 43).

-export([start_link/0, carrier_for_number/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    carrier_dict
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

carrier_for_number(Number, Lang) ->
    gen_server:call(?MODULE, {carrier_for_number, number_to_bin(Number), Lang}).

init([]) ->
    {ok, Path} = path(),
    {ok, AllLanguages0} = file:list_dir(Path),

    AllLanguages = lists:map(fun(X) -> list_to_binary(X) end, AllLanguages0),

    FunLang = fun(Lang, Acc) ->
        LangPath = <<Path/binary, "/", Lang/binary>>,

        case load_carrier_mapping(LangPath) of
            {ok, Trie} ->
                dict:store(Lang, Trie, Acc);
            _ ->
                Acc
        end
    end,

    Dict = lists:foldl(FunLang, dict:new(), AllLanguages),

    {ok, #state{carrier_dict = Dict}}.

handle_call({carrier_for_number, Number, Lang}, _From, #state {carrier_dict = Dict} = State) ->
    Carrier = case dict:find(Lang, Dict) of
        {ok, Trie} ->
            case btrie:find_prefix_longest(Number, Trie) of
                {ok, _Prefix, C} ->
                    C;
                _ ->
                    <<>>
            end;
        _ ->
            <<>>
    end,
    {reply, Carrier, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

load_carrier_mapping(Path) ->
    case file:list_dir(Path) of
        {ok, AllFiles} ->
            FunFile = fun(Filename, Acc) ->
                FilenameBin = list_to_binary(Filename),
                case binary:match(FilenameBin, <<".txt">>,[]) of
                    nomatch ->
                        Acc;
                    _ ->
                        FilePath = <<Path/binary, "/", FilenameBin/binary>>,
                        {ok, Device} = file:open(FilePath, [read]),
                        get_lines(Device) ++ Acc
                end
            end,

            AllMappings = lists:foldl(FunFile, [], AllFiles),
            {ok, btrie:new(AllMappings)};
        _ ->
            false
    end.

get_lines(Device) ->
    get_lines(Device, []).

get_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  ->
            file:close(Device),
            Accum;
        Line ->
            LineBin = list_to_binary(Line),

            case LineBin of
                <<"#", _/binary>> ->
                    get_lines(Device, Accum);
                _ ->
                    case binary:split(LineBin, <<"|">>, [global]) of
                        [Prefix, Carrier0] ->
                            Carrier = re:replace(Carrier0, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]),
                            get_lines(Device, [{Prefix, Carrier}|Accum]);
                        _ ->
                            get_lines(Device, Accum)
                    end
            end
    end.

path() ->
    case code:priv_dir(elibphonenumber) of
        {error, bad_name} = Error ->
            Error;
        Dir ->
            {ok, list_to_binary(filename:join(Dir, "carrier"))}
    end.

number_to_bin(Nr) when is_binary(Nr) ->
    trim_plus_sign(Nr);
number_to_bin(Nr) ->
    trim_plus_sign(phonenumber_util:format(Nr, e164)).

trim_plus_sign(P) when is_binary(P) ->
    case binary:first(P) of
        ?PLUS_SIGN_DIGIT ->
            <<_:1/binary, Value/binary>> = P,
            Value;
        _ ->
            P
    end.