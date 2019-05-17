(**************************************************************************)
(*     Sail                                                               *)
(*                                                                        *)
(*  Copyright (c) 2013-2017                                               *)
(*    Kathyrn Gray                                                        *)
(*    Shaked Flur                                                         *)
(*    Stephen Kell                                                        *)
(*    Gabriel Kerneis                                                     *)
(*    Robert Norton-Wright                                                *)
(*    Christopher Pulte                                                   *)
(*    Peter Sewell                                                        *)
(*    Alasdair Armstrong                                                  *)
(*    Brian Campbell                                                      *)
(*    Thomas Bauereiss                                                    *)
(*    Anthony Fox                                                         *)
(*    Jon French                                                          *)
(*    Dominic Mulligan                                                    *)
(*    Stephen Kell                                                        *)
(*    Mark Wassell                                                        *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  This software was developed by the University of Cambridge Computer   *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems  *)
(*  (REMS) project, funded by EPSRC grant EP/K008528/1.                   *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*     notice, this list of conditions and the following disclaimer.      *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*     notice, this list of conditions and the following disclaimer in    *)
(*     the documentation and/or other materials provided with the         *)
(*     distribution.                                                      *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''    *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED     *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A       *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR   *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,          *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT      *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF      *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND   *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,    *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT    *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF    *)
(*  SUCH DAMAGE.                                                          *)
(**************************************************************************)

open Buffer

type doc =
  | String of string
  | Empty
  | Hardline of int
  | Group of doc
  | Concat of doc * doc
  | Break of int

let (^^) doc1 doc2 = Concat (doc1, doc2)
let (^/^) doc1 doc2 = doc1 ^^ Break 1 ^^ doc2

let break n = Break n
let string str = String str
let space = string " "
let spaces n = string (String.make n ' ')
let hardline = Hardline 0
let dedent n = Hardline (- n)
let group doc = Group doc
let align doc = Group doc
let empty = Empty

let concat = function
  | [] -> Empty
  | doc :: docs ->
     List.fold_left (fun doc1 doc2 -> doc1 ^^ doc2) doc docs

let separate sep = function
  | [] -> Empty
  | doc :: docs ->
     List.fold_left (fun doc1 doc2 -> doc1 ^^ sep ^^ doc2) doc docs

let separate_map sep f = function
  | [] -> Empty
  | x :: xs ->
     List.fold_left (fun doc y -> doc ^^ sep ^^ f y) (f x) xs

let parens doc = string "(" ^^ doc ^^ string ")"
let braces doc = string "{" ^^ doc ^^ string "}"
let brackets doc = string "[" ^^ doc ^^ string "]"
let dquotes doc = string "\"" ^^ doc ^^ string "\""
let enclose left right doc = left ^^ doc ^^ right

let infix mid left right = left ^^ space ^^ mid ^^ space ^^ right

let surround indent sp left doc right =
  match doc with
  | Empty ->
     left ^^ spaces sp ^^ right
  | _ ->
     left ^^ hardline
     ^^ spaces indent ^^ group doc ^^ hardline
     ^^ right

let nest indent doc =
  spaces indent ^^ group doc

let (^//^) doc1 doc2 =
  doc1 ^^ break 1 ^^ (nest 2 doc2)

let hang n doc =
  hardline ^^ nest n doc

let prefix n m doc1 doc2 =
  doc1 ^^ break m ^^ (nest n doc2)

let twice doc = doc ^^ doc

type ctx = {
    group : int;
    max_width : int;
  }

let rec format ctx w buf = function
  | String str -> add_string buf str; w + String.length str
  | Empty -> w
  | Hardline n ->
     add_char buf '\n';
     add_string buf (String.make (max (ctx.group + n) 0) ' ');
     ctx.group
  | Group doc -> format { ctx with group = w } w buf doc
  | Concat (doc1, doc2) ->
     let w' = format ctx w buf doc1 in
     format ctx w' buf doc2
  | Break n ->
     if w >= ctx.max_width && ctx.group < ctx.max_width then (
       add_char buf '\n';
       ctx.group
     ) else (
       add_string buf (String.make n ' ');
       w + n
     )

let to_string max_width doc =
  let buf = create max_width in
  ignore (format { group = 0; max_width = max_width } 0 buf doc);
  contents buf

let to_channel max_width chan doc =
  let buf = create max_width in
  ignore (format { group = 0; max_width = max_width } 0 buf doc);
  output_buffer chan buf

let comma = string ","
let semi = string ";"
let lparen = string "("
let rparen = string ")"
let lbrace = string "{"
let rbrace = string "}"
let lbracket = string "["
let rbracket = string "]"
let colon = string ":"
let bar = string "|"
let dot = string "."
let equals = string "="
let arrow = string "->"
let underscore = string "_"
let dquote = string "\""
let plus = string "+"
let minus = string "-"
let caret = string "^"
let star = string "*"
let ampersand = string "&"
let bquote = string "`"
let squote = string "'"
let bang = string "!"
