# YAML parser written in OCaml
This is just a toy.

## Build
To build the parser:
```shell
$ make
```

To build the lexer:
```shell
$ make yaml_only_lex.native
```

## Usage
To parse a YAML file and print a result:
```shell
$ ./yaml_main.native <yaml file path>
```
To lex a YAML and print a result:
```shell
$ ./yaml_only_lex.native <yaml file path>
```
