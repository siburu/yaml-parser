%token <string> STRING
%token COLON
%token HYPHEN
%token INDENT
%token DEDENT
%token <string> COMMENT
%token EMPTY_LINE
%token EOF

%start <Yaml.elem option> prog
%%

prog:
        |EOF {None}
        |seq = sequence prog {Some (`Sequence seq)}
        |map = mapping prog {Some (`Mapping map)}
        |c = COMMENT prog {Some (`Comment c)}
        |EMPTY_LINE prog {print_endline "empty line recognized"; Some (`Comment "empty line")}
        ;

value:
        |INDENT; seq = sequence; DEDENT {`Sequence seq}
        |INDENT; map = mapping; DEDENT {`Mapping map}
        |str = STRING {`Scalar str}
        ;

sequence:
        obj = rev_sequence {print_endline "seq complete!!!"; List.rev obj};

rev_sequence:
        |HYPHEN; v = value {print_endline "seq!!!"; [v]}
        |seq = rev_sequence; HYPHEN; v = value {print_endline "seq!!!"; v::seq}
        ;

mapping:
        obj = rev_mapping {print_endline "map complete!!!"; List.rev obj};

rev_mapping:
        |{[]}
        |map = rev_mapping; s = STRING; COLON; v = value {print_endline ("map!!!" ^ s); (`Scalar s,v)::map}
        ;
        (*|s = STRING; COLON; v = value {print_endline ("map!!!" ^ s); [(`Scalar s,v)]}*)
