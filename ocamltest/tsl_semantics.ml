(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of TSL blocks and operations on test trees *)

open Tsl_ast

let apply_modifiers env modifiers_name =
  let name = modifiers_name.node in
  let modifier = Environments.Include name in
  Environments.apply_modifier env modifier

(* `decl` is true iff the variable is being assigned with `set`. *)
let add_to_env decl variable_name value env =
  let var = Variables.from_name variable_name in
  let builtin = Option.is_some (Variables.find_variable variable_name) in
  let defined = Environments.is_variable_defined var env in
  let known = builtin || defined in
  if decl then begin
    (* Defining a new variable with `set var = value` *)
    if known then raise (Variables.Variable_already_registered variable_name)
  end else begin
    (* Changing the value of an existing variable with `var = value` *)
    if not known then raise (Variables.No_such_variable variable_name)
  end;
  Environments.add var value env

let append_to_env variable_name value env =
  let variable = Variables.from_name variable_name in
  let builtin = Option.is_some (Variables.find_variable variable_name) in
  let defined = Environments.is_variable_defined variable env in
  if builtin || defined then
    Environments.append variable value env
  else
    raise (Variables.No_such_variable variable_name)

let interpret_environment_statement env statement = match statement.node with
  | Assignment (decl, var, value) ->
      add_to_env decl var.node value.node env
  | Append (var, value) ->
      append_to_env var.node value.node env
  | Include modifiers_name ->
      apply_modifiers env modifiers_name
  | Unset var ->
      Environments.unsetenv (Variables.from_name var.node) env

exception No_such_test_or_action of string

let lookup_test located_name =
  let name = located_name.node in
  match Tests.lookup name with
  | None ->
    begin match Actions.lookup name with
    | None -> raise (No_such_test_or_action name)
    | Some action ->
      Tests.test_of_action action
    end
  | Some test -> test

open Printf

let print_tsl_ast ~compact oc ast =
  let pr fmt (*args*) = fprintf oc fmt (*args*) in

  let rec print_ast indent (Ast (stmts, subs)) =
    print_statements indent stmts;
    print_forest indent subs;

  and print_sub indent ast =
    pr "{\n";
    print_ast (indent ^ "  ") ast;
    pr "%s}" indent;

  and print_statements indent stmts =
    match stmts with
    | Test (name, mods) :: tl ->
      pr "%s%s" indent name.node;
      begin match mods with
      | m :: tl ->
        pr " with %s" m.node;
        List.iter (fun m -> pr ", %s" m.node) tl;
      | [] -> ()
      end;
      pr ";\n";
      if tl <> [] && not compact then pr "\n";
      print_statements indent tl;
    | Environment_statement env :: tl->
      print_env indent env;
      print_statements indent tl;
    | [] -> ()

  and print_forest indent subs =
    if subs <> [] then begin
      pr "%s" indent;
      List.iter (print_sub indent) subs;
      pr "\n";
    end

  and print_env indent e =
    match e.node with
    | Assignment (set, variable, value) ->
      pr "%s" indent;
      if set then pr "set ";
      pr "%s = \"%s\";\n" variable.node value.node;
    | Append (variable, value) ->
      pr "%s%s += \"%s\";\n" indent variable.node value.node;
    | Include ls ->
      pr "%sinclude %s;\n" indent ls.node;
    | Unset ls ->
      pr "%sunset %s;\n" indent ls.node;
  in
  print_ast " " ast;
