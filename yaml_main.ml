open Core.Std
open Lexing

let flatten_lexer lexer =
        let queue = Queue.create () in
        fun lexbuf ->
                if Queue.is_empty queue then
                        List.iter (List.rev (lexer lexbuf)) ~f:(fun t -> Queue.enqueue queue t);
                match Queue.dequeue queue with
                |Some x -> x
                |None -> exit(-2)

let print_position outx lexbuf =
        let pos = lexbuf.lex_curr_p in
        fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
        try Yaml_parser.prog (flatten_lexer Yaml_lexer.read) lexbuf with
        |Yaml_lexer.SyntaxError msg ->
                        fprintf stderr "%a: %s\n" print_position lexbuf msg;
                        None
        |Yaml_parser.Error ->
                        fprintf stderr "%a: syntax error\n" print_position lexbuf;
                        exit(-1)

let rec parse_and_print lexbuf =
        match parse_with_error lexbuf with
        |Some value ->
                        printf "%a\n" Yaml.output_elem value;
                        parse_and_print lexbuf
        |None -> 
                        printf "Parser output nothing\n"

let loop filename () =
        let inx = In_channel.create filename in
        let lexbuf = Lexing.from_channel inx in
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename };
        parse_and_print lexbuf;
        In_channel.close inx

let () =
        Command.basic ~summary:"Parse and display YAML"
                Command.Spec.(empty +> anon ("filename" %: file))
                loop
        |> Command.run
