-module(c_erlang).

-export_type([stack_item/0]).

-type stack_item() ::
        {module(),
         Function :: atom(),
         arity() | (Args :: [term()]),
         Location :: [{file, string()} | {line, pos_integer()}]}.
