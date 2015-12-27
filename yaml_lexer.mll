{
        open Core.Std
        open Lexing
        open Yaml_parser

        exception SyntaxError of string

        let next_line lexbuf =
                let pos = lexbuf.lex_curr_p in
                lexbuf.lex_curr_p <-
                        {pos with pos_bol = lexbuf.lex_curr_pos;
                        pos_lnum = pos.pos_lnum + 1
                        }

        let indent_stack = Stack.create ()

        let proc_indent indent =
                let dedents n =
                        let rec dedents_rec l = function
                                |0 -> l
                                |n -> dedents_rec (DEDENT::l) (n-1)
                        in dedents_rec [] n
                in
                let rec proc_indent_rec indent dedent_n =
                        let top = Stack.top_exn indent_stack in
                        if top <= indent then begin
                                if top = indent then
                                        dedents dedent_n
                                else begin
                                        Stack.push indent_stack indent;
                                        INDENT::(dedents dedent_n)
                                end
                        end else begin
                                ignore (Stack.pop indent_stack);
                                proc_indent_rec indent (dedent_n + 1)
                        end
                in if Stack.is_empty indent_stack
                                then
                                        begin
                                                Stack.push indent_stack 0;
                                                []
                                        end
                                else proc_indent_rec indent 0
}

let space = [' ''\t']*
(* let nonspace = [^' ''\t']+ *)
let nonspace = ['a'-'z''A'-'Z''0'-'9''_']+
let newline = '\r' | '\n' | "\r\n"
(* let key = [^':''-'space newline]+ *)
let key = ['a'-'z''A'-'Z''0'-'9''_']+

rule read =
        parse
                                |space
                                {let indents = proc_indent (String.length(Lexing.lexeme lexbuf))
                                in List.concat [read_left lexbuf; indents]}
                                |eof {[EOF]}
and read_left =
        parse
                                |space {read_left lexbuf}
                                |newline {next_line lexbuf; [EMPTY_LINE]}
                                |'-' {List.concat [read_right lexbuf; [HYPHEN]]}
                                |key
                                {let key = [STRING (Lexing.lexeme lexbuf)]
                                in List.concat [read_right lexbuf; key]}
                                |eof {[EOF]}
and read_right =
        parse
                                |space {read_right lexbuf}
                                |newline {next_line lexbuf; []}
                                |':' {List.concat [read_right lexbuf; [COLON]]}
                                |nonspace
                                {let str = [STRING (Lexing.lexeme lexbuf)]
                                in List.concat [read_right lexbuf; str]}
                                |eof {[EOF]}
