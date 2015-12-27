open Core.Std
open Yaml_parser
open Lexing

let flatten_lexer lexer =
        let queue = Queue.create () in
        fun lexbuf ->
                if Queue.is_empty queue then
                        List.iter (List.rev (lexer lexbuf)) ~f:(fun t -> Queue.enqueue queue t);
                match Queue.dequeue queue with
                |Some x -> x
                |None -> print_endline "error in lexer"; EOF

let lex_and_print lexbuf =
        let lexer = flatten_lexer Yaml_lexer.read in
        let rec lex_and_print_rec lexbuf =
                let token = lexer lexbuf in
                begin
                        match token with
                        |EOF -> print_endline "EOF"
                        |STRING s -> print_endline ("STRING " ^ s)
                        |INDENT -> print_endline "INDENT"
                        |HYPHEN -> print_endline "HYPHEN"
                        |EMPTY_LINE -> print_endline "EMPTY_LINE"
                        |DEDENT -> print_endline "DEDENT"
                        |COMMENT c-> print_endline ("COMMENT " ^ c)
                        |COLON -> print_endline "COLON"
                end;
                if token = EOF then () else lex_and_print_rec lexbuf
        in try lex_and_print_rec lexbuf with
        |_ -> print_endline "error"

let loop filename () =
        let inx = In_channel.create filename in
        let lexbuf = Lexing.from_channel inx in
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename };
        lex_and_print lexbuf;
        In_channel.close inx

let () =
        Command.basic ~summary:"Parse and display YAML"
                Command.Spec.(empty +> anon ("filename" %: file))
                loop
        |> Command.run
