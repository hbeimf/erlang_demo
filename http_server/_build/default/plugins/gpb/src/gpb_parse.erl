-module(gpb_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 353).

-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").

-export([post_process_one_file/2]).
-export([post_process_all_files/2]).
-export([format_post_process_error/1]).
-export([fetch_imports/1]).

-type defs() :: [def()].
-type def() :: {{msg, Name::atom()}, [field()]} |
               {{enum, Name::atom()}, [{Sym::atom(), Value::integer()}]} |
               {{service, Name::atom()}, [#?gpb_rpc{}]} |
               {package, Name::atom()} |
               {syntax, string()} | % "proto2" | "proto3"
               {{extensions, MsgName::atom()}, [field_number_extension()]} |
               {{extend, MsgName::atom()}, MoreFields::[field()]} |
               {proto3_msgs, [MsgName::atom()]} |
               {{msg_containment, ProtoName::string()},[MsgName::atom()]} |
               {{reserved_numbers, MsgName::atom()}, [integer()]} |
               {{reserved_names, MsgName::atom()}, [FieldName::atom()]} |
               {import, ProtoFile::string()}.
-type field() :: #?gpb_field{} | #gpb_oneof{}.
-type field_number_extension() :: {Lower::integer(), Upper::integer() | max}.

-export_type([defs/0, def/0]).
-export_type([field/0]).


verify_syntax({str_lit, _Line, "proto2"}) ->
    {syntax, "proto2"};
verify_syntax({str_lit, _Line, "proto3"}) ->
    {syntax, "proto3"};
verify_syntax({str_lit, Line, "proto"++_ = Unsupported}) ->
    return_error(Line, "Unsupported proto version: " ++ Unsupported);
verify_syntax({str_lit, Line, Unsupported}) ->
    return_error(Line, "Unsupported proto syntax: " ++ Unsupported).

identifier_name({identifier, _Line, Name}) -> list_to_atom(Name).

kw_to_identifier({Kw, Line}) ->
    {identifier, Line, atom_to_list(Kw)}.

literal_value({_TokenType, _Line, Value}) -> Value.

post_process_one_file(Defs, Opts) ->
    case find_package_def(Defs, Opts) of
        {ok, Package} ->
            {ok, handle_proto_syntax_version_one_file(
                   convert_default_values(
                     flatten_qualify_defnames(Defs, Package)))};
        {error, Reasons} ->
            {error, Reasons}
    end.

post_process_all_files(Defs, Opts) ->
    case resolve_names(Defs) of
        {ok, Defs2} ->
            {ok, normalize_msg_field_options(
                   handle_proto_syntax_version_all_files(
                     possibly_prefix_suffix_msgs(
                       enumerate_msg_fields(
                         reformat_names(
                           extend_msgs(Defs2))),
                       Opts)))};
        {error, Reasons} ->
            {error, Reasons}
    end.

%% -> {ok, Defs} | {error, [Reason]}
resolve_names(Defs) ->
    case resolve_refs(Defs) of
        {ok, RDefs} ->
            case verify_defs(RDefs) of
                ok ->
                    {ok, RDefs};
                {error, Reasons} ->
                    {error, Reasons}
            end;
        {error, Reasons} ->
            {error, Reasons}
    end.

%% Find any package specifier. At most one such package specifier
%% may exist, and it can exist anywhere (top-level) in the proto file,
%% yet it still applies to the whole file.
find_package_def(Defs, Opts) ->
    case proplists:get_bool(use_packages, Opts) of
        true ->
            case [Pkg || {package, Pkg} <- Defs] of
                [] ->
                    {ok, empty_pkg_root()};
                [Pkg] ->
                    {ok, ['.' | Pkg]};
                Pkgs when length(Pkgs) >= 2 ->
                    PrettyPkgs = [reformat_name(Pkg) || Pkg <- Pkgs],
                    {error, [{multiple_pkg_specifiers, PrettyPkgs}]}
            end;
        false ->
            {ok, empty_pkg_root()}
    end.

empty_pkg_root() ->
    ['.'].

%% For nested message definitions such as
%% ```
%%    message m1 {
%%      required uint32 f1 = 1;
%%      message m2 { ... }
%%      enum e2 { ... }
%%    };",
%% '''
%% the parser will produce a nested structure, such as:
%% ```
%%   [{{msg,M1},[#field{},
%%               {{msg,M2}, [...]},
%%               {{enum,E2}, [...]}]}]
%% '''
%% Flattening means to lift the nested m2 and e2 definition to the top-level,
%% so the above turns into:
%% ```
%%   [{{msg,M1},[#field{}]},
%%    {{msg,M2}, [...]},
%%    {{enum,E2}, [...]}]
%% '''
%%
%% During this process, the message and enum names and similar get
%% fully qualified into absolute rooted name-paths. In the example
%% above, this applies to m1, m2 and e2. Note that at this stage,
%% nothing is done to resolve reference to names, such as message
%% types for fields. A name-path is a list of path components,
%% separated by the dot-atom, '.', and an absolute rooted name-path is
%% a path that begins with the dot-atom, '.', much like a slash or a
%% backslash in a file name path.
flatten_qualify_defnames(Defs, Root) ->
    lists:reverse(
      lists:foldl(
        fun({{msg,Name}, FieldsOrDefs}, Acc) ->
                FullName = prepend_path(Root, Name),
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, FullName),
                [{{msg,FullName},Fields2} | Defs2] ++ Acc;
           ({{enum,Name}, ENs}, Acc) ->
                FullName = prepend_path(Root, Name),
                [{{enum,FullName}, ENs} | Acc];
           ({extensions,Exts}, Acc) ->
                [{{extensions,Root},Exts} | Acc];
           ({{extend,{eref1,Name}}, FieldsOrDefs}, Acc) ->
                FullNameCandidates =
                    compute_roots(prepend_path(Root, Name)) ++
                    compute_roots(prepend_path(empty_pkg_root(), Name)),
                {Fields2, Defs2} = flatten_fields(FieldsOrDefs, Root),
                [{{extend,{eref2,FullNameCandidates}},Fields2} | Defs2] ++ Acc;
           ({{service, Name}, RPCs}, Acc) ->
                FullName = prepend_path(Root, Name),
                [{{service,FullName}, RPCs} | Acc];
           (OtherElem, Acc) ->
                [OtherElem | Acc]
        end,
        [],
        Defs)).

flatten_fields(FieldsOrDefs, FullName) ->
    {RFields2, Defs2} =
        lists:foldl(fun(#?gpb_field{}=F, {Fs,Ds}) ->
                            {[F | Fs], Ds};
                       (#gpb_oneof{}=O, {Fs,Ds}) ->
                            {[O | Fs], Ds};
                       ({{extend, _Ref},_}=Def, {Fs,Ds}) ->
                            QDefs = flatten_qualify_defnames([Def], FullName),
                            {Fs, QDefs ++ Ds};
                       ({reserved_numbers, Ns}, {Fs,Ds}) ->
                            Def = {{reserved_numbers,FullName}, Ns},
                            {Fs, [Def | Ds]};
                       ({reserved_names, Ns}, {Fs,Ds}) ->
                            Def = {{reserved_names,FullName}, Ns},
                            {Fs, [Def | Ds]};
                       (Def, {Fs,Ds}) ->
                            QDefs = flatten_qualify_defnames([Def], FullName),
                            {Fs, QDefs++Ds}
                    end,
                    {[],[]},
                    FieldsOrDefs),
    {lists:reverse(RFields2), Defs2}.

%% Resolve any refs
resolve_refs(Defs) ->
    Root = ['.'],
    {ResolvedRefs, Reasons} =
        lists:mapfoldl(
          fun({{msg,FullName}, Fields}, Acc) ->
                  {NewFields, Acc2} =
                      resolve_field_refs(Fields, Defs, Root, FullName, Acc),
                  {{{msg,FullName}, NewFields}, Acc2};
             ({{service,FullName}, Rpcs}, Acc) ->
                  {NewRPCs, Acc2} =
                      resolve_rpc_refs(Rpcs, Defs, Root, FullName, Acc),
                  {{{service,FullName}, NewRPCs}, Acc2};
             ({{extend,ExtendeeCandidates}, Fields}, Acc) ->
                  {Extendee, NewFields, Acc2} =
                      resolve_extend_refs(ExtendeeCandidates, Fields, Defs,
                                          Root, Acc),
                  {{{extend,Extendee}, NewFields}, Acc2};
             (OtherElem, Acc) ->
                  {OtherElem, Acc}
          end,
          [],
          Defs),
    if Reasons == [] -> {ok, ResolvedRefs};
       Reasons /= [] -> {error, lists:reverse(Reasons)}
    end.



resolve_field_refs(Fields, Defs, Root, FullName, Reasons) ->
    lists:mapfoldl(
      fun(#?gpb_field{name=FName, type={ref,Ref}}=Field, Acc) ->
              case resolve_ref(Defs, Ref, Root, FullName) of
                  {found, TypeName} ->
                      {Field#?gpb_field{type=TypeName}, Acc};
                  not_found ->
                      Reason = {ref_to_undefined_msg_or_enum,
                                {{FullName, FName}, Ref}},
                      {Field, [Reason | Acc]}
              end;
         (#?gpb_field{name=FName, type={map,KeyType,{ref,Ref}}}=Field, Acc) ->
              case resolve_ref(Defs, Ref, Root, FullName) of
                  {found, TypeName} ->
                      {Field#?gpb_field{type={map,KeyType,TypeName}}, Acc};
                  not_found ->
                      Reason = {ref_to_undefined_msg_or_enum,
                                {{FullName, FName}, Ref}},
                      {Field, [Reason | Acc]}
              end;
         (#?gpb_field{}=Field, Acc) ->
              {Field, Acc};
         (#gpb_oneof{fields=OFields1}=Oneof, Acc) ->
              {OFields2, Acc2} =
                  resolve_field_refs(OFields1, Defs, Root, FullName, Acc),
              {Oneof#gpb_oneof{fields=OFields2}, Acc2}
      end,
      Reasons,
      Fields).

resolve_rpc_refs(Rpcs, Defs, Root, FullName, Reasons) ->
    lists:mapfoldl(
      fun({RpcName, {Arg, ArgIsStream}, {Return, ReturnIsStream}, Opts}=Rpc,
          Acc) ->
              case resolve_ref(Defs, Arg, Root, FullName) of
                  {found, {msg, MArg}} ->
                      case resolve_ref(Defs, Return, Root, FullName) of
                          {found, {msg, MReturn}} ->
                              NewOpts = [{reformat_name(Name), Value}
                                         || {option,Name,Value} <- Opts],
                              NewRpc = #?gpb_rpc{name=RpcName,
                                                 input=MArg,
                                                 input_stream=ArgIsStream,
                                                 output=MReturn,
                                                 output_stream=ReturnIsStream,
                                                 opts=NewOpts},
                              {NewRpc, Acc};
                          {found, {BadType, MReturn}} ->
                              Reason = {rpc_return_ref_to_non_msg,
                                        {{FullName, RpcName, Return},
                                         BadType, MReturn}},
                              {Rpc, [Reason | Acc]};
                          not_found ->
                              Reason = {rpc_return_ref_to_undefined_msg,
                                        {{FullName, RpcName}, Return}},
                              {Rpc, [Reason | Acc]}
                      end;
                  {found, {BadType, MArg}} ->
                      Reason = {rpc_arg_ref_to_non_msg,
                                {{FullName, RpcName, Arg}, BadType, MArg}},
                      {Rpc, [Reason | Acc]};
                  not_found ->
                      Reason = {rpc_arg_ref_to_undefined_msg,
                                {{FullName, RpcName}, Arg}},
                      {Rpc, [Reason | Acc]}
              end
      end,
      Reasons,
      Rpcs).

resolve_extend_refs({eref2, ExtendeeCandidates}, Fields, Defs, Root, Acc) ->
    case resolve_ref_candidates(Defs, ExtendeeCandidates) of
        {found, {msg,NewToBeExtended}} ->
            {NewFields, Acc2} =
                resolve_field_refs(Fields, Defs, Root, ['.'], Acc),
            {NewToBeExtended, NewFields, Acc2};
        not_found ->
            Reason = {extend_ref_to_undefined_msg, hd(ExtendeeCandidates)},
            {hd(ExtendeeCandidates), Fields, [Reason | Acc]}
    end.

%% -> {found, {msg,FullName}|{enum,FullName}} | not_found
resolve_ref(Defs, Ref, Root, FullName) ->
    case is_absolute_ref(Ref) of
        true  ->
            FullRef = ensure_path_prepended(Root, Ref),
            find_typename(FullRef, Defs);
        false ->
            PossibleRoots = compute_roots(FullName),
            find_ref_rootwards(PossibleRoots, Ref, Defs)
    end.

resolve_ref_candidates(Defs, [Cand1 | Rest]) ->
    case find_typename(Cand1, Defs) of
        {found, TypeName} -> {found, TypeName};
        not_found -> resolve_ref_candidates(Defs, Rest)
    end;
resolve_ref_candidates(_Defs, []) ->
    not_found.

find_ref_rootwards([PossibleRoot | Rest], Ref, Defs) ->
    FullRef = ensure_path_prepended(PossibleRoot, Ref),
    case find_typename(FullRef, Defs) of
        {found, TypeName} -> {found, TypeName};
        not_found -> find_ref_rootwards(Rest, Ref, Defs)
    end;
find_ref_rootwards([], _Ref, _Defs) ->
    not_found.

is_absolute_ref(['.' | _]) -> true;
is_absolute_ref(_Other)    -> false.

find_typename(Name, [{{enum,Name}, _Values} | _])  -> {found, {enum,Name}};
find_typename(Name, [{{msg,Name}, _SubElems} | _]) -> {found, {msg,Name}};
find_typename(Name, [_ | Rest])                    -> find_typename(Name, Rest);
find_typename(_Name,[])                            -> not_found.

%% Turn ['.',m1,'.',m2,'.',m3]
%% into [['.',m1,'.',m2,'.',m3],
%%       ['.',m1,'.',m2],
%%       ['.',m1],
%%       ['.']]
compute_roots(['.']) -> [['.']];
compute_roots(DeeperPath) ->
    [DeeperPath | compute_roots(drop_last_level(DeeperPath))].

drop_last_level(['.']) -> ['.'];
drop_last_level(['.', X]) when is_atom(X) -> ['.'];
drop_last_level(DeeperPath) when length(DeeperPath) >= 3 ->
    [_X, '.' | RestReversed] = lists:reverse(DeeperPath),
    lists:reverse(RestReversed).

prepend_path(['.'], Id) when is_atom(Id)           -> ['.', Id];
prepend_path(['.'], SubPath) when is_list(SubPath) -> ['.' | SubPath];
prepend_path(Path,  Id) when is_atom(Id)           -> Path ++ ['.', Id];
prepend_path(Path,  SubPath) when is_list(SubPath) -> Path ++ ['.' | SubPath].

ensure_path_prepended(Pkg, Path)   ->
    case lists:prefix(Pkg, Path) of
        false -> prepend_path(Pkg, Path);
        true ->  Path
    end.

convert_default_values(Defs) ->
    lists:map(
      fun({{msg,Name},Fields}) ->
              Fields2 = lists:map(fun convert_default_values_field/1, Fields),
              {{msg,Name},Fields2};
         (Other) ->
              Other
      end,
      Defs).

convert_default_values_field(#?gpb_field{type=Type, opts=Opts}=Field) ->
    case {Type, lists:keyfind(default, 1, Opts)} of
        {bytes, {default, Default}} when is_list(Default) ->
            %% Default values for type bytes are written as a string
            Default2 = list_to_binary(Default),
            Opts2 = lists:keyreplace(default, 1, Opts, {default, Default2}),
            Field#?gpb_field{opts=Opts2};
        _ ->
            Field
    end;
convert_default_values_field(#gpb_oneof{fields=OFs}=Field) ->
    OFs2 = lists:map(fun convert_default_values_field/1, OFs),
    Field#gpb_oneof{fields=OFs2}.

handle_proto_syntax_version_one_file(Defs) ->
    case proplists:get_value(syntax, Defs) of
        undefined -> handle_proto2_1(Defs);
        "proto2"  -> handle_proto2_1(Defs);
        "proto3"  -> handle_proto3_1(Defs)
    end.

handle_proto2_1(Defs) ->
    Defs.

handle_proto3_1(Defs) ->
    %% FIXME: Verify no 'extensions' or 'extend'
    %% FIXME: Verify no 'required' occurrences
    %% FIXME: Verify enums start with 0

    %% Remember which msgs were defined using proto3 syntax,
    %% so we can treat them differently later on.
    anno_msgs_proto3_origin(Defs).

anno_msgs_proto3_origin(Defs) ->
    anno_msgs_proto3_origin_2(Defs, []).

anno_msgs_proto3_origin_2([{{msg,Msg},_Fields}=Def | Rest], P3Msgs) ->
    [Def | anno_msgs_proto3_origin_2(Rest, [Msg | P3Msgs])];
anno_msgs_proto3_origin_2([Def | Rest], Acc) ->
    [Def | anno_msgs_proto3_origin_2(Rest, Acc)];
anno_msgs_proto3_origin_2([], Acc) ->
    [{proto3_msgs,lists:reverse(Acc)}].

handle_proto_syntax_version_all_files(Defs) ->
    P3Items = [X || {proto3_msgs,_}=X <- Defs],
    if P3Items == [] ->
            Defs;
       P3Items /= [] ->
            Proto3Msgs = lists:append([Msgs || {proto3_msgs,Msgs} <- P3Items]),
            Defs1 = Defs -- P3Items,
            Defs2 = Defs1 ++ [{proto3_msgs, lists:sort(Proto3Msgs)}],

            %% The protobuf language guide for proto3 says: "In proto3,
            %% repeated fields of scalar numeric types use packed encoding by
            %% default."
            default_repeated_to_packed(Defs2, Proto3Msgs)
    end.

default_repeated_to_packed(Defs, P3Msgs) ->
    lists:map(
      fun({{msg,MsgName},Fields}=MsgDef) ->
              case lists:member(MsgName, P3Msgs) of
                  true ->
                      Fields1 = default_repeated_fields_to_packed(Fields),
                      {{msg,MsgName}, Fields1};
                  false ->
                      MsgDef
              end;
         (Other) ->
              Other
      end,
      Defs).

default_repeated_fields_to_packed(Fields) ->
    lists:map(
      fun(#?gpb_field{occurrence=repeated, opts=Opts, type=Type}=F) ->
              case {proplists:get_value(packed, Opts),
                    is_scalar_numeric(Type)} of
                  {undefined, true} ->
                      NewOpts = [{packed, true} | Opts],
                      F#?gpb_field{opts=NewOpts};
                  _ ->
                      F
              end;
         (F) ->
              F
      end,
      Fields).

is_scalar_numeric(int32)    -> true;
is_scalar_numeric(int64)    -> true;
is_scalar_numeric(uint32)   -> true;
is_scalar_numeric(uint64)   -> true;
is_scalar_numeric(sint32)   -> true;
is_scalar_numeric(sint64)   -> true;
is_scalar_numeric(fixed32)  -> true;
is_scalar_numeric(fixed64)  -> true;
is_scalar_numeric(sfixed32) -> true;
is_scalar_numeric(sfixed64) -> true;
is_scalar_numeric(bool)     -> true;
is_scalar_numeric(float)    -> true;
is_scalar_numeric(double)   -> true;
is_scalar_numeric({enum,_}) -> true;
is_scalar_numeric(_)        -> false. % not: string | bytes | msg | map

%% Find inconsistencies
%%
%% Prerequisites:
%% `Defs' is expected to be flattened and may or may not be reformatted.
verify_defs(Defs) ->
    collect_errors(Defs,
                   [{msg,     [fun verify_field_defaults/2]},
                    {extend,  [fun verify_extend/2]},
                    {service, [fun verify_service/2]},
                    {'_',     [fun(_Def, _AllDefs) -> ok end]}]).

collect_errors(Defs, VerifiersList) ->
    collect_errors(Defs, Defs, VerifiersList, ok).

collect_errors([{{ElemType,_},_}=Def | Rest], AllDefs, VerifiersList, Acc) ->
    Result = lists:foldl(
               fun(Verifier, A) -> add_acc(A, Verifier(Def, AllDefs)) end,
               Acc,
               find_verifiers(ElemType, VerifiersList)),
    collect_errors(Rest, AllDefs, VerifiersList, Result);
collect_errors([_OtherDef | Rest], AllDefs, VerifiersList, Acc) ->
    %% Example: import, package, ...
    collect_errors(Rest, AllDefs, VerifiersList, Acc);
collect_errors([], _AllRefs, _VerifiersList, Acc) ->
    case Acc of
        ok                       -> ok;
        {error, ReasonsReversed} -> {error, lists:reverse(ReasonsReversed)}
    end.

add_acc(AnyPreviousResult, ok)         -> AnyPreviousResult;
add_acc(ok,                {error, R}) -> {error, add_reason([], R)};
add_acc({error, Reasons},  {error, R}) -> {error, add_reason(Reasons, R)}.

add_reason(Reasons, Reason) when not is_list(Reason) ->
    [Reason | Reasons];
add_reason(Reasons, MoreReasons) when is_list(MoreReasons) ->
    lists:reverse(MoreReasons, Reasons).

find_verifiers(Type,  [{Type, Verifiers} | _]) -> Verifiers;
find_verifiers(_Type, [{'_', Verifiers} | _])  -> Verifiers;
find_verifiers(Type,  [_Other | Rest])         -> find_verifiers(Type, Rest).

verify_field_defaults({{msg,M}, Fields}, AllDefs) ->
    lists:foldl(fun(#?gpb_field{name=Name, type=Type, opts=FOpts}, Acc) ->
                        Res = case lists:keysearch(default, 1, FOpts) of
                                  {value, {default, Default}} ->
                                      verify_scalar_default_if_present(
                                        M, Name, Type, Default, AllDefs);
                                  false ->
                                      ok
                              end,
                        add_acc(Acc, Res);
                   (#gpb_oneof{fields=OFields}, Acc) ->
                        Res = verify_field_defaults({{msg,M},OFields}, AllDefs),
                        add_acc(Acc, Res)
                end,
                ok,
                Fields).

verify_scalar_default_if_present(MsgName, FieldName, Type, Default, AllDefs) ->
    case Type of
        {enum,Ref} ->
            case lists:keysearch({enum, Ref}, 1, AllDefs) of
                {value, {{enum,Ref}, Enumerators}} ->
                    case lists:keysearch(Default, 1, Enumerators) of
                        {value, {Default, _Value}} ->
                            ok;
                        false ->
                            {error,
                             {{invalid_default_enum_value, Default},
                              {name_to_dstr(MsgName), atom_to_list(FieldName)}}}
                    end;
                false ->
                    ok %% caught by another verification step
            end;
        ScalarType when is_atom(ScalarType) ->
            case gpb:check_scalar(Default, ScalarType) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {Reason, {name_to_dstr(MsgName),
                                      atom_to_list(FieldName)}}}
            end
    end.

verify_extend(_, _AllDefs) ->
    %% FIXME
    ok.

verify_service(_, _AllDefs) ->
    %% FIXME
    ok.

name_to_absdstr(['.' | Name]) -> "." ++ name_to_dstr(Name);
name_to_absdstr(Name) -> name_to_dstr(Name).

name_to_dstr(Name) when is_list(Name) ->
    string:join([atom_to_list(P) || P <- Name, P /= '.'],
                ".");
name_to_dstr(Name) when is_atom(Name) ->
    atom_to_list(Name).

format_post_process_error({error, Reasons}) ->
    lists:flatten([[fmt_err(Reason),"\n"] || Reason <- Reasons]).

-define(f(F, A), io_lib:format(F, A)).

fmt_err({multiple_pkg_specifiers, Pkgs}) ->
    ?f("package specified more than once: ~s~n",
       [string:join([atom_to_list(Pkg) || Pkg <- Pkgs], ", ")]);
fmt_err({ref_to_undefined_msg_or_enum, {{Msg, Field}, To}}) ->
    ?f("in msg ~s, field ~s: undefined reference  ~s",
       [name_to_dstr(Msg), name_to_dstr(Field), name_to_absdstr(To)]);
fmt_err({extend_ref_to_undefined_msg, Msg}) ->
    ?f("extend of unknown message ~s", [name_to_absdstr(Msg)]);
fmt_err({rpc_return_ref_to_non_msg,
         {{FullName, RpcName, Return}, BadType, MReturn}}) ->
    ?f("in service ~s, rpc ~s, the return type, ~s, refers to "
       " a ~p, ~s, instead of to a message",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Return),
        BadType, name_to_dstr(MReturn)]);
fmt_err({rpc_return_ref_to_undefined_msg, {{FullName, RpcName}, Ret}}) ->
    ?f("in service ~s, rpc ~s, return: undefined reference ~s",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Ret)]);
fmt_err({rpc_arg_ref_to_non_msg, {{FullName, RpcName, Arg}, BadType, MArg}}) ->
    ?f("in service ~s, rpc ~s, the arg type, ~s, refers to "
       " a ~p, ~s, instead of to a message",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Arg),
        BadType, name_to_dstr(MArg)]);
fmt_err({rpc_arg_ref_to_undefined_msg, {{FullName, RpcName}, Arg}}) ->
    ?f("in service ~s, rpc ~s, arg: undefined reference ~s",
       [name_to_dstr(FullName), name_to_dstr(RpcName), name_to_absdstr(Arg)]);
fmt_err({{invalid_default_enum_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: undefined enumerator in default value ~s",
       [Msg, Field, Default]);
fmt_err({{{value_out_of_range, Signedness, Bits}, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: default value ~p out of range for ~p ~p bit int",
       [Msg, Field, Default, Signedness, Bits]);
fmt_err({{{bad_integer_value, Signedness, Bits}, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for ~p ~p bit int",
       [Msg, Field, Default, Signedness, Bits]);
fmt_err({{bad_floating_point_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad floating point default value ~p",
       [Msg, Field, Default]);
fmt_err({{bad_boolean_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for boolean",
       [Msg, Field, Default]);
fmt_err({{bad_unicode_string, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for string",
       [Msg, Field, Default]);
fmt_err({{bad_binary_value, Default}, {Msg, Field}}) ->
    ?f("in msg ~s, field ~s: bad default value ~p for bytes",
       [Msg, Field, Default]).

%% Rewrites for instance ['.','m1','.',m2] into 'm1.m2'
%% Example: {{msg,['.','m1','.',m2]}, [#field{type={msg,['.','m1','.',m3]}}]}
%% becomes: {{msg,'m1.m2'},           [#field{type={msg,'m1.m3'}}]}
%%
%% Prerequisites:
%% `Defs' is expected to be flattened and names and references
%% are expected to have been resolved
reformat_names(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{msg_containment, ProtoName}, Msgs}) ->
                      {{msg_containment,ProtoName},
                       [reformat_name(N) || N <- Msgs]};
                 ({{enum,Name}, ENs}) ->
                      {{enum,reformat_name(Name)}, reformat_enum_opt_names(ENs)};
                 ({{extensions,Name}, Exts}) ->
                      {{extensions,reformat_name(Name)}, Exts};
                 ({{extend,Name}, Fields}) ->
                      %% FIXME: extend
                      {{extend,reformat_name(Name)}, reformat_fields(Fields)};
                 ({{service,Name}, RPCs}) ->
                      {{service,reformat_name(Name)}, reformat_rpcs(RPCs)};
                 ({package, Name}) ->
                      {package, reformat_name(Name)};
                 ({proto3_msgs,Names}) ->
                      {proto3_msgs,[reformat_name(Name) || Name <- Names]};
                 ({{reserved_numbers,Name}, Ns}) ->
                      {{reserved_numbers,reformat_name(Name)}, Ns};
                 ({{reserved_names,Name}, FieldNames}) ->
                      {{reserved_names,reformat_name(Name)}, FieldNames};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

reformat_fields(Fields) ->
    lists:map(
      fun(#?gpb_field{type={T,Nm}}=F) ->
              F#?gpb_field{type={T,reformat_name(Nm)}};
         (#?gpb_field{type={map,KeyType,{T,Nm}}}=F) ->
              F#?gpb_field{type={map,KeyType,{T,reformat_name(Nm)}}};
         (#?gpb_field{}=F) ->
              F;
         (#gpb_oneof{fields=Fs}=O) ->
              O#gpb_oneof{fields=reformat_fields(Fs)}
      end,
      Fields).

%% `Defs' is expected to be parsed.
reformat_enum_opt_names(Def) ->
    [case Item of
         {option, Name, Value} ->
             {option, reformat_name(Name), Value};
         Other ->
             Other
     end
     || Item <- Def].

reformat_name(Name) ->
    list_to_atom(string:join([atom_to_list(P) || P <- Name,
                                                 P /= '.'],
                             ".")).

reformat_rpcs(RPCs) ->
    lists:map(fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}=R) ->
                      R#?gpb_rpc{name=RpcName,
                                 input=reformat_name(Arg),
                                 output=reformat_name(Return)}
              end,
              RPCs).

%% `Defs' is expected to be flattened and may or may not be reformatted
%% `Defs' is expected to be verified, to not extend missing messages
extend_msgs(Defs0) ->
    Extendings = [E || {{extend,_MsgToExtend},_Mor91eFields}=E <- Defs0],
    lists:foldl(fun possibly_extend_msg/2, Defs0, Extendings).


possibly_extend_msg({{extend,Msg}, MoreFields}=Extending, Defs) ->
    case lists:keyfind({msg,Msg}, 1, Defs) of
        {{msg,Msg}, OrigFields} ->
            NewDef = {{msg,Msg}, OrigFields ++ MoreFields},
            lists:keyreplace({msg,Msg}, 1, Defs, NewDef) -- [Extending];
        false ->
            Defs
    end.

%% `Defs' is expected to be flattened
enumerate_msg_fields(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg, Name}, enumerate_fields(Fields)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

enumerate_fields(Fields) ->
    lists:map(fun({I, #?gpb_field{}=F}) ->
                      F#?gpb_field{rnum=I};
                 ({I, #gpb_oneof{fields=Fs}=O}) ->
                      NewFields = [F#?gpb_field{rnum=I} || F <- Fs],
                      O#gpb_oneof{rnum=I, fields=NewFields}
              end,
              index_seq(2, Fields)).

index_seq(_Start, []) -> [];
index_seq(Start, L)   -> lists:zip(lists:seq(Start, length(L) + Start - 1), L).

%% `Defs' is expected to be parsed.
normalize_msg_field_options(Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      {{msg, Name}, normalize_field_options(Fields)};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

normalize_field_options(Fields) ->
    lists:map(fun(#?gpb_field{type={map,_KeyType,_ValueType}, opts=Opts}=F) ->
                      Opts1    = normalize_field_options_2(Opts),
                      Opts2    = Opts1 -- [packed],
                      F#?gpb_field{opts = Opts2};
                 (#?gpb_field{opts=Opts}=F) ->
                      Opts1    = normalize_field_options_2(Opts),
                      F#?gpb_field{opts = Opts1};
                 (#gpb_oneof{fields=Fs}=O) ->
                      O#gpb_oneof{fields=normalize_field_options(Fs)}
              end,
              Fields).

normalize_field_options_2(Opts) ->
    Opts1 = opt_tuple_to_atom_if_defined_true(packed, Opts),
    opt_tuple_to_atom_if_defined_true(deprecated, Opts1).

opt_tuple_to_atom_if_defined_true(Opt, Opts) ->
    case proplists:get_bool(Opt, Opts) of
        false -> lists:keydelete(Opt, 1, Opts);
        true  -> [Opt | lists:keydelete(Opt, 1, Opts)]
    end.

possibly_prefix_suffix_msgs(Defs, Opts) ->
    Prefix = proplists:get_value(msg_name_prefix, Opts, ""),
    Suffix = proplists:get_value(msg_name_suffix, Opts, ""),
    ToLower = case proplists:get_value(msg_name_to_lower, Opts, false) of
                  false ->
                      false;
                  true ->
                      to_lower
              end,
    ToLowerOrSnake =
        case proplists:get_value(msg_name_to_snake_case, Opts, ToLower) of
            true ->
                snake_case;
            T ->
                T
        end,

    if Prefix == "", Suffix == "", ToLowerOrSnake == false ->
            Defs;
       true ->
            prefix_suffix_msgs(Prefix, Suffix, ToLowerOrSnake, Defs)
    end.

find_proto(_, []) ->
    undefined;
find_proto(Name, [{{msg_containment, Proto}, Msgs} | Rest]) ->
      case lists:member(Name, Msgs) of
          true ->
              Proto;
          false ->
              find_proto(Name, Rest)
      end;
find_proto(Name, [_ | Rest]) ->
    find_proto(Name, Rest).

maybe_prefix_by_proto(Name, {by_proto, PrefixList}, Defs) ->
    case find_proto(Name, Defs) of
        undefined ->
            "";
        ProtoName ->
            proplists:get_value(list_to_atom(ProtoName), PrefixList, "")
    end;
maybe_prefix_by_proto(_Name, Prefix, _Defs) ->
    Prefix.

prefix_suffix_msgs(Prefix, Suffix, ToLowerOrSnake, Defs) ->
    lists:map(fun({{msg,Name}, Fields}) ->
                      Prefix1 = maybe_prefix_by_proto(Name, Prefix, Defs),
                      {{msg,prefix_suffix_name(Prefix1, Suffix,
                                             ToLowerOrSnake, Name)},
                        prefix_suffix_fields(Prefix, Suffix,
                                             ToLowerOrSnake, Fields, Defs)};
                 ({{extensions,Name}, Exts}) ->
                      Prefix1 = maybe_prefix_by_proto(Name, Prefix, Defs),
                      {{extensions,
                        prefix_suffix_name(Prefix1, Suffix,
                                           ToLowerOrSnake, Name)},
                       Exts};
                 ({{service,Name}, RPCs}) ->
                      {{service, maybe_tolower_or_snake_name(Name,
                                                             ToLowerOrSnake)},
                        prefix_suffix_rpcs(Prefix, Suffix,
                                           ToLowerOrSnake, RPCs, Defs)};
                 ({package,Name}) ->
                      {package, maybe_tolower_or_snake_name(Name,
                                                            ToLowerOrSnake)};
                 ({proto3_msgs,Names}) ->
                      {proto3_msgs,
                       [begin
                            Prefix1 = maybe_prefix_by_proto(Name, Prefix, Defs),
                            prefix_suffix_name(Prefix1, Suffix,
                                               ToLowerOrSnake, Name)
                        end || Name <- Names]};
                 (OtherElem) ->
                      OtherElem
              end,
              Defs).

prefix_suffix_fields(Prefix, Suffix, ToLowerOrSnake, Fields, Defs) ->
    lists:map(
      fun(#?gpb_field{type={msg,MsgName}}=F) ->
              Prefix1 = maybe_prefix_by_proto(MsgName, Prefix, Defs),
              NewMsgName = prefix_suffix_name(Prefix1, Suffix,
                                              ToLowerOrSnake, MsgName),
              F#?gpb_field{type={msg,NewMsgName}};
         (#?gpb_field{type={map,KeyType,{msg,MsgName}}}=F) ->
              Prefix1 = maybe_prefix_by_proto(MsgName, Prefix, Defs),
              NewMsgName = prefix_suffix_name(Prefix1, Suffix,
                                              ToLowerOrSnake, MsgName),
              F#?gpb_field{type={map,KeyType,{msg,NewMsgName}}};
         (#gpb_oneof{fields=Fs}=F) ->
              Fs2 = prefix_suffix_fields(Prefix, Suffix,
                                         ToLowerOrSnake, Fs, Defs),
              F#gpb_oneof{fields=Fs2};
         (#?gpb_field{}=F) ->
              F
      end,
      Fields).

prefix_suffix_name(Prefix, Suffix, ToLowerOrSnake, Name) ->
    Name1 = maybe_tolower_or_snake_name(Name, ToLowerOrSnake),
    Name2 = lists:concat([Prefix, Name1, Suffix]),
    list_to_atom(Name2).

maybe_tolower_or_snake_name(Name, false) -> Name;
maybe_tolower_or_snake_name(Name, to_lower) ->
    list_to_atom(string:to_lower(atom_to_list(Name)));
maybe_tolower_or_snake_name(Name, snake_case) ->
    NameString = atom_to_list(Name),
    Snaked = lists:foldl(fun(RE, Snaking) ->
                             re:replace(Snaking, RE, "\\1_\\2", [{return, list},
                                                                 global])
                         end, NameString, [%% uppercase followed by lowercase
                                          "(.)([A-Z][a-z]+)",
                                          %% any consecutive digits
                                          "(.)([0-9]+)",
                                          %% uppercase with lowercase
                                          %% or digit before it
                                          "([a-z0-9])([A-Z])"]),
    list_to_atom(string:to_lower(Snaked)).

prefix_suffix_rpcs(Prefix, Suffix, ToLowerOrSnake, RPCs, Defs) ->
    lists:map(fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}=R) ->
                      PrefixArg = maybe_prefix_by_proto(Arg, Prefix, Defs),
                      PrefixReturn = maybe_prefix_by_proto(Return,Prefix,Defs),
                      NewArg = prefix_suffix_name(PrefixArg, Suffix,
                                                  ToLowerOrSnake, Arg),
                      NewReturn = prefix_suffix_name(PrefixReturn, Suffix,
                                                     ToLowerOrSnake, Return),
                      R#?gpb_rpc{name=RpcName,
                                 input=NewArg,
                                 output=NewReturn}
              end,
              RPCs).

%% @doc Fetch the `import'ed files.
%% @end
%% `Defs' is expected to be parsed, but not necessarily post_processed.
-spec fetch_imports(defs()) -> [ProtoFile::string()].
fetch_imports(Defs) ->
    [Path || {import,Path} <- Defs].

-file("/usr/local/erlang_1.8.3/lib/erlang/lib/parsetools-2.1.1/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 1086).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, import, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, package, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, service, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, syntax, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_10(_S, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, import, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, package, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, service, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccpars2_288(_S, Cat, [1 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
yeccpars2_3(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_element(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_proto(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, import, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, package, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, service, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2_287(_S, Cat, [11 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_12(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, import, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, package, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, service, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccpars2_286(_S, Cat, [12 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
yeccpars2_13(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_14/7}).
yeccpars2_14(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_15/7}).
yeccpars2_15(S, str_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_16/7}).
yeccpars2_16(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_17: see yeccpars2_14

%% yeccpars2_18: see yeccpars2_14

-dialyzer({nowarn_function, yeccpars2_19/7}).
yeccpars2_19(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_20/7}).
yeccpars2_20(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).
yeccpars2_21(S, str_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_22/7}).
yeccpars2_22(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_syntax_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
yeccpars2_24(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_25(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, rpc, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccpars2_26(26, Cat, [25 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_26/7}).
yeccpars2_26(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_27(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, rpc, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccpars2_62(_S, Cat, [27 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_28(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, rpc, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccpars2_61(_S, Cat, [28 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).
yeccpars2_29(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).
yeccpars2_30(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_31/7}).
yeccpars2_31(S, returns, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, stream, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_33/7}).
yeccpars2_33(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_name(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
yeccpars2_35(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_36(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_identifiers(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_37: see yeccpars2_14

-dialyzer({nowarn_function, yeccpars2_38/7}).
yeccpars2_38(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_rpc_arg(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_40: see yeccpars2_35

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_identifiers(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_name(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_rpc_arg(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
yeccpars2_44(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_45/7}).
yeccpars2_45(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_46/7}).
yeccpars2_46(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, stream, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_47/7}).
yeccpars2_47(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_48: see yeccpars2_14

-dialyzer({nowarn_function, yeccpars2_49/7}).
yeccpars2_49(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_rpc_ret(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_rpc_ret(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_rpc_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2_55(55, Cat, [53 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
yeccpars2_54(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_55/7}).
yeccpars2_55(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_56(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccpars2_57(_S, Cat, [56 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_m_opts(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_rpc_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_59(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccpars2_60(_S, Cat, [59 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_m_opts(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_rpc_defs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_rpc_defs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_service_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_64/7}).
yeccpars2_64(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_package_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_66/7}).
yeccpars2_66(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_67(S, bool_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, float_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, str_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_constant(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_constant(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_option_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_constant(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_constant(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_constant(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(S, str_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_string_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_string_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_79/7}).
yeccpars2_79(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_80(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, extensions, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, oneof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, optional, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, repeated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, reserved, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_80_(Stack),
 yeccpars2_86(86, Cat, [80 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_81/7}).
yeccpars2_81(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, bool_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, deprecated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, extensions, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, import, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, max, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, optional, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, package, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, packed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, repeated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, reserved, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, returns, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, rpc, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, service, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, stream, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, syntax, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, to, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_msg_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_msg_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
yeccpars2_84(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_86/7}).
yeccpars2_86(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_87(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, extensions, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, oneof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, optional, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, repeated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, reserved, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 yeccpars2_236(_S, Cat, [87 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_msg_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_89: see yeccpars2_81

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_msg_elem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_msg_elem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, extensions, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, oneof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, optional, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, repeated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, reserved, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 yeccpars2_227(_S, Cat, [92 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_96: see yeccpars2_14

-dialyzer({nowarn_function, yeccpars2_97/7}).
yeccpars2_97(S, dec_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, hex_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, oct_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_103/7}).
yeccpars2_103(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_104/7}).
yeccpars2_104(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_occurrence(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_occurrence(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_occurrence(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_108(S, str_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_116(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_res_names(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_reserved_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_118(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_res_numbers(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_reserved_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_120(S, to, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_res_number(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_121: see yeccpars2_97

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 yeccgoto_res_number(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_123: see yeccpars2_97

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_124_(Stack),
 yeccgoto_res_numbers(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_125/7}).
yeccpars2_125(S, str_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_res_names(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_127/7}).
yeccpars2_127(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_128: see yeccpars2_84

%% yeccpars2_129: see yeccpars2_81

-dialyzer({nowarn_function, yeccpars2_130/7}).
yeccpars2_130(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_131(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_oneof_elems(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_oneof_elems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_oneof_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_134/7}).
yeccpars2_134(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_141_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_143_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_161_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_164_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_166_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_169_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_fidentifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_174/7}).
yeccpars2_174(S, dec_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_175/7}).
yeccpars2_175(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_oneof_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_177(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, deprecated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, packed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_177_(Stack),
 yeccpars2_178(178, Cat, [177 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_178/7}).
yeccpars2_178(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_179(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 yeccgoto_field_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_opt_field_opts(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccgoto_field_opts(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_182/7}).
yeccpars2_182(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_183(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_183_(Stack),
 yeccgoto_field_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_184(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_field_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_185/7}).
yeccpars2_185(S, bool_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_field_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_187/7}).
yeccpars2_187(S, bool_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_188_(Stack),
 yeccgoto_field_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_189: see yeccpars2_67

yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_190_(Stack),
 yeccgoto_field_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_191/7}).
yeccpars2_191(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, deprecated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, packed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_field_opts(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_193: see yeccpars2_67

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_field_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_195/7}).
yeccpars2_195(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_195(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_196_(Stack),
 yeccgoto_oneof_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_197/7}).
yeccpars2_197(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_198/7}).
yeccpars2_198(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_200_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_205_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_map_key_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_211: see yeccpars2_84

-dialyzer({nowarn_function, yeccpars2_212/7}).
yeccpars2_212(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_map_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_214(S, to, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_ext(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_215/7}).
yeccpars2_215(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_216(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_exts(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_217: see yeccpars2_97

yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_exts(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_extensions_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_220(S, max, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_ext(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_ext(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_223/7}).
yeccpars2_223(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_224(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, extensions, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, oneof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, optional, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, repeated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, reserved, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_224_(Stack),
 yeccpars2_225(225, Cat, [224 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_225/7}).
yeccpars2_225(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_msg_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_msg_elems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_228/7}).
yeccpars2_228(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_229/7}).
yeccpars2_229(S, dec_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_230/7}).
yeccpars2_230(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_231_(Stack),
 yeccgoto_msg_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_232(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, deprecated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, packed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 yeccpars2_233(233, Cat, [232 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_233/7}).
yeccpars2_233(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_234/7}).
yeccpars2_234(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_msg_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_msg_elems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_message_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_238: see yeccpars2_81

-dialyzer({nowarn_function, yeccpars2_239/7}).
yeccpars2_239(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_240/7}).
yeccpars2_240(S, dec_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_241/7}).
yeccpars2_241(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_242_(Stack),
 yeccgoto_msg_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_243(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, deprecated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, packed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccpars2_244(244, Cat, [243 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_244/7}).
yeccpars2_244(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_245/7}).
yeccpars2_245(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_246_(Stack),
 yeccgoto_msg_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_247/7}).
yeccpars2_247(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_248/7}).
yeccpars2_248(S, dec_lit, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_249/7}).
yeccpars2_249(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_msg_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_251(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(S, deprecated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(S, packed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_251_(Stack),
 yeccpars2_252(252, Cat, [251 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_252/7}).
yeccpars2_252(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_253/7}).
yeccpars2_253(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_msg_elem(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_255/7}).
yeccpars2_255(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_256_(Stack),
 yeccgoto_import_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_257/7}).
yeccpars2_257(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_257(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_258(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, bytes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, extend, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, extensions, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, fixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, fixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, int32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, int64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, message, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, oneof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, optional, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, repeated, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, reserved, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, sfixed32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, sfixed64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, sint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, sint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, uint32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(S, uint64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_258_(Stack),
 yeccpars2_259(259, Cat, [258 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_259/7}).
yeccpars2_259(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_260_(Stack),
 yeccgoto_extend_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_261/7}).
yeccpars2_261(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_262(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_262_(Stack),
 yeccpars2_264(264, Cat, [262 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_263(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_263_(Stack),
 yeccpars2_285(_S, Cat, [263 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_264/7}).
yeccpars2_264(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_265(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_265_(Stack),
 yeccpars2_283(_S, Cat, [265 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_266(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(S, option, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_266_(Stack),
 yeccpars2_282(_S, Cat, [266 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_267/7}).
yeccpars2_267(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_267(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_268: see yeccpars2_97

-dialyzer({nowarn_function, yeccpars2_269/7}).
yeccpars2_269(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_270_(Stack),
 yeccgoto_enum_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_271(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, identifier, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_(Stack),
 yeccpars2_272(272, Cat, [271 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_272/7}).
yeccpars2_272(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_273/7}).
yeccpars2_273(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_273(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_opt_enum_opts(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_275(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr);
yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_275_(Stack),
 yeccgoto_enum_opts(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_276: see yeccpars2_14

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_277_(Stack),
 yeccgoto_enum_opts(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_278: see yeccpars2_67

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_enum_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_280/7}).
yeccpars2_280(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_enum_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_282_(Stack),
 yeccgoto_enum_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_283_(Stack),
 yeccgoto_enum_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_284_(Stack),
 yeccgoto_enum_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_285_(Stack),
 yeccgoto_enum_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_286_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_287_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_288_(Stack),
 yeccgoto_proto(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_constant(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constant(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constant(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constant(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_element(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_element(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_elements(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_field(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(265, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_field(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(265, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_field(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(265, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_field(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(265, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_fields(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_fields(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_fields(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_fields(266=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_opt(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(275, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_opt(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(275, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_opts(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_opts(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ext(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ext(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(216, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extend_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extend_def(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extend_def(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extend_def(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extensions_def(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extensions_def(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extensions_def(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extensions_def(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extensions_def(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_exts(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exts(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fidentifier(81, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(247, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fidentifier(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fidentifier(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fidentifier(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(239, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_opt(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opt(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opt(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opt(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opt(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(181, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_opts(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opts(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opts(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opts(243=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_opts(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_identifiers(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(84=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(243=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identifiers(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_import_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_def(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_def(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_import_def(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_integer(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(121=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(120, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(269, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_integer(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_m_opts(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_m_opts(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_m_opts(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_map_key_type(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_map_type(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(89, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_message_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_message_def(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_msg_elem(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elem(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elem(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elem(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elem(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_msg_elems(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elems(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elems(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elems(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(225, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_msg_elems(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(259, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_name(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(257, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(84=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(273, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_name(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(273, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_occurrence(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_occurrence(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_occurrence(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_occurrence(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_occurrence(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oneof_def(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oneof_def(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oneof_def(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oneof_def(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oneof_def(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oneof_elem(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oneof_elem(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oneof_elems(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oneof_elems(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_opt_enum_opts(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(272, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_opt_field_opts(177, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_opt_field_opts(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(233, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_opt_field_opts(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(244, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_opt_field_opts(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(252, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_option_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_option_def(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_package_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_package_def(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_package_def(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_package_def(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_proto(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_res_names(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_res_names(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_res_number(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_res_number(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_res_numbers(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_res_numbers(123=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_reserved_def(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reserved_def(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reserved_def(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reserved_def(224=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reserved_def(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rpc_arg(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rpc_def(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(27, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rpc_def(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(27, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rpc_def(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(27, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rpc_defs(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(26, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rpc_defs(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rpc_defs(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rpc_ret(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_service_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_service_def(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_service_def(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_service_def(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_string_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_string_expr(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_string_expr(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_string_expr(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_string_expr(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_string_expr(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_string_expr(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_syntax_def(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(84, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(238, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 78).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_1_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 78).
yeccpars2_1_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_11_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 78).
yeccpars2_11_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_12_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 78).
yeccpars2_12_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_23_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 74).
yeccpars2_23_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   verify_syntax ( __3 )
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 330).
yeccpars2_25_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_27_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 330).
yeccpars2_27_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_28_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 330).
yeccpars2_28_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_36_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 95).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ identifier_name ( __1 ) ]
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 338).
yeccpars2_39_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __3 , true }
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 93).
yeccpars2_41_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ identifier_name ( __1 ) , '.'
    | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 90).
yeccpars2_42_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ '.' | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 337).
yeccpars2_43_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , false }
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 341).
yeccpars2_50_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __3 , true }
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 340).
yeccpars2_51_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , false }
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 333).
yeccpars2_52_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { identifier_name ( __2 ) , __3 , __5 , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 345).
yeccpars2_53_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_56_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 345).
yeccpars2_56_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_57_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 344).
yeccpars2_57_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 335).
yeccpars2_58_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { identifier_name ( __2 ) , __3 , __5 , __7 }
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 345).
yeccpars2_59_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_60_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 343).
yeccpars2_60_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 329).
yeccpars2_61_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 328).
yeccpars2_62_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 326).
yeccpars2_63_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { service , identifier_name ( __2 ) } , __4 }
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 88).
yeccpars2_65_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { package , __2 }
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 99).
yeccpars2_70_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { option , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 270).
yeccpars2_71_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   literal_value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 272).
yeccpars2_72_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   literal_value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 268).
yeccpars2_73_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   literal_value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 274).
yeccpars2_74_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   literal_value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 266).
yeccpars2_75_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   identifier_name ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 273).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   literal_value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 278).
yeccpars2_77_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   literal_value ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 277).
yeccpars2_78_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   literal_value ( __1 ) ++ __2
  end | __Stack].

-compile({inline,yeccpars2_80_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 128).
yeccpars2_80_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_85_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 248).
yeccpars2_85_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { ref , __1 }
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 128).
yeccpars2_87_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_90_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 168).
yeccpars2_90_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { extensions , lists : sort ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 128).
yeccpars2_92_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 245).
yeccpars2_93_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bool
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 247).
yeccpars2_94_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bytes
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 233).
yeccpars2_95_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   double
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 241).
yeccpars2_98_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   fixed32
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 242).
yeccpars2_99_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   fixed64
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 234).
yeccpars2_100_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   float
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 235).
yeccpars2_101_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   int32
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 236).
yeccpars2_102_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   int64
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 230).
yeccpars2_105_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   optional
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 231).
yeccpars2_106_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   repeated
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 229).
yeccpars2_107_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   required
  end | __Stack].

-compile({inline,yeccpars2_109_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 243).
yeccpars2_109_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sfixed32
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 244).
yeccpars2_110_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sfixed64
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 239).
yeccpars2_111_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sint32
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 240).
yeccpars2_112_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sint64
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 246).
yeccpars2_113_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   string
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 237).
yeccpars2_114_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   uint32
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 238).
yeccpars2_115_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   uint64
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 299).
yeccpars2_116_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 289).
yeccpars2_117_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { reserved_numbers , __2 }
  end | __Stack].

-compile({inline,yeccpars2_118_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 293).
yeccpars2_118_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 290).
yeccpars2_119_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { reserved_names , __2 }
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 296).
yeccpars2_122_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_124_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 292).
yeccpars2_124_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 298).
yeccpars2_126_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 306).
yeccpars2_131_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 305).
yeccpars2_132_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 302).
yeccpars2_133_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # gpb_oneof { name = identifier_name ( __2 ) ,
    fields = __4 }
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 194).
yeccpars2_135_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 197).
yeccpars2_136_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( literal_value ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 196).
yeccpars2_137_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 198).
yeccpars2_138_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 209).
yeccpars2_139_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 182).
yeccpars2_140_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_141_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 177).
yeccpars2_141_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 202).
yeccpars2_142_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 201).
yeccpars2_143_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 190).
yeccpars2_144_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 191).
yeccpars2_145_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 183).
yeccpars2_146_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_148_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 199).
yeccpars2_148_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 184).
yeccpars2_149_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 185).
yeccpars2_150_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 211).
yeccpars2_151_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 203).
yeccpars2_152_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 178).
yeccpars2_153_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 200).
yeccpars2_154_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 180).
yeccpars2_155_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_156_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 175).
yeccpars2_156_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 208).
yeccpars2_157_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 181).
yeccpars2_158_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 179).
yeccpars2_159_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 212).
yeccpars2_160_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 206).
yeccpars2_161_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_162_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 205).
yeccpars2_162_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 176).
yeccpars2_163_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 192).
yeccpars2_164_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 193).
yeccpars2_165_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 188).
yeccpars2_166_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 189).
yeccpars2_167_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 207).
yeccpars2_168_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_169_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 195).
yeccpars2_169_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 210).
yeccpars2_170_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 204).
yeccpars2_171_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 186).
yeccpars2_172_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 187).
yeccpars2_173_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   kw_to_identifier ( __1 )
  end | __Stack].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 4990).
-compile({inline,yeccpars2_176_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 309).
yeccpars2_176_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = optional ,
    type = __1 ,
    name = identifier_name ( __2 ) ,
    fnum = literal_value ( __4 ) ,
    opts = [ ] }
  end | __Stack].

-compile({inline,yeccpars2_177_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 215).
yeccpars2_177_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_179_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 226).
yeccpars2_179_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { identifier_name ( __1 ) , true }
  end | __Stack].

-compile({inline,yeccpars2_181_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 219).
yeccpars2_181_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_183_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 224).
yeccpars2_183_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { deprecated , true }
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 222).
yeccpars2_184_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { packed , true }
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 223).
yeccpars2_186_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { packed , literal_value ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_188_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 225).
yeccpars2_188_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { deprecated , literal_value ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_190_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 221).
yeccpars2_190_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { default , __3 }
  end | __Stack].

-compile({inline,yeccpars2_192_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 218).
yeccpars2_192_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_194_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 227).
yeccpars2_194_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { identifier_name ( __1 ) , __3 }
  end | __Stack].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 5082).
-compile({inline,yeccpars2_196_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 315).
yeccpars2_196_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = optional ,
    type = __1 ,
    name = identifier_name ( __2 ) ,
    fnum = literal_value ( __4 ) ,
    opts = __6 }
  end | __Stack].

-compile({inline,yeccpars2_199_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 262).
yeccpars2_199_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bool
  end | __Stack].

-compile({inline,yeccpars2_200_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 258).
yeccpars2_200_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   fixed32
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 259).
yeccpars2_201_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   fixed64
  end | __Stack].

-compile({inline,yeccpars2_202_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 252).
yeccpars2_202_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   int32
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 253).
yeccpars2_203_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   int64
  end | __Stack].

-compile({inline,yeccpars2_204_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 260).
yeccpars2_204_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sfixed32
  end | __Stack].

-compile({inline,yeccpars2_205_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 261).
yeccpars2_205_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sfixed64
  end | __Stack].

-compile({inline,yeccpars2_206_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 256).
yeccpars2_206_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sint32
  end | __Stack].

-compile({inline,yeccpars2_207_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 257).
yeccpars2_207_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sint64
  end | __Stack].

-compile({inline,yeccpars2_208_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 263).
yeccpars2_208_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   string
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 254).
yeccpars2_209_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   uint32
  end | __Stack].

-compile({inline,yeccpars2_210_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 255).
yeccpars2_210_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   uint64
  end | __Stack].

-compile({inline,yeccpars2_213_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 250).
yeccpars2_213_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { map , __3 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 285).
yeccpars2_214_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { __1 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_216_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 283).
yeccpars2_216_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_218_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 282).
yeccpars2_218_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_219_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 280).
yeccpars2_219_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 286).
yeccpars2_221_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 287).
yeccpars2_222_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , max }
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 128).
yeccpars2_224_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_226_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 171).
yeccpars2_226_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { extend , { eref1 , __2 } } , __4 }
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 127).
yeccpars2_227_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 5270).
-compile({inline,yeccpars2_231_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 155).
yeccpars2_231_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = repeated ,
    type = __1 ,
    name = identifier_name ( __2 ) ,
    fnum = literal_value ( __4 ) }
  end | __Stack].

-compile({inline,yeccpars2_232_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 215).
yeccpars2_232_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 5289).
-compile({inline,yeccpars2_235_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 160).
yeccpars2_235_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = repeated ,
    type = __1 ,
    name = identifier_name ( __2 ) ,
    fnum = literal_value ( __4 ) ,
    opts = __6 }
  end | __Stack].

-compile({inline,yeccpars2_236_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 126).
yeccpars2_236_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 124).
yeccpars2_237_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { msg , identifier_name ( __2 ) } , __4 }
  end | __Stack].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 5318).
-compile({inline,yeccpars2_242_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 131).
yeccpars2_242_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = __1 ,
    type = __2 ,
    name = identifier_name ( __3 ) ,
    fnum = literal_value ( __5 ) ,
    opts = [ ] }
  end | __Stack].

-compile({inline,yeccpars2_243_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 215).
yeccpars2_243_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 5338).
-compile({inline,yeccpars2_246_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 137).
yeccpars2_246_(__Stack0) ->
 [__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = __1 ,
    type = __2 ,
    name = identifier_name ( __3 ) ,
    fnum = literal_value ( __5 ) ,
    opts = __7 }
  end | __Stack].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 5351).
-compile({inline,yeccpars2_250_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 143).
yeccpars2_250_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = optional ,
    type = __1 ,
    name = identifier_name ( __2 ) ,
    fnum = literal_value ( __4 ) ,
    opts = [ ] }
  end | __Stack].

-compile({inline,yeccpars2_251_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 215).
yeccpars2_251_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.erl", 5371).
-compile({inline,yeccpars2_254_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 149).
yeccpars2_254_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # ? gpb_field { occurrence = optional ,
    type = __1 ,
    name = identifier_name ( __2 ) ,
    fnum = literal_value ( __4 ) ,
    opts = __6 }
  end | __Stack].

-compile({inline,yeccpars2_256_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 97).
yeccpars2_256_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { import , literal_value ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_258_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 128).
yeccpars2_258_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_260_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 322).
yeccpars2_260_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { extend , { eref1 , __2 } } , __4 }
  end | __Stack].

-compile({inline,yeccpars2_262_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 107).
yeccpars2_262_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_263_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 107).
yeccpars2_263_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_265_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 107).
yeccpars2_265_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_266_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 107).
yeccpars2_266_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_270_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 110).
yeccpars2_270_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { identifier_name ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_271_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 115).
yeccpars2_271_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_275_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 118).
yeccpars2_275_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_277_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 117).
yeccpars2_277_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_279_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 120).
yeccpars2_279_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_281_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 112).
yeccpars2_281_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { identifier_name ( __1 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_282_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 106).
yeccpars2_282_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_283_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 104).
yeccpars2_283_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_284_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 102).
yeccpars2_284_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { enum , identifier_name ( __2 ) } , __4 }
  end | __Stack].

-compile({inline,yeccpars2_285_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 105).
yeccpars2_285_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_286_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 77).
yeccpars2_286_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_287_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 76).
yeccpars2_287_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_288_/1}).
-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 72).
yeccpars2_288_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].


-file("/web/yaf/socketio/http_server/_build/default/plugins/gpb/src/gpb_parse.yrl", 1261).
