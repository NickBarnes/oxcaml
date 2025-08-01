(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2020 OCamlPro SAS                                    *)
(*   Copyright 2018--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Simple = Int_ids.Simple

type t =
  { param : Variable.t;
    uid : Flambda_debug_uid.t;
    kind : Flambda_kind.With_subkind.t
  }

include Container_types.Make (struct
  type nonrec t = t

  let compare { param = param1; kind = kind1; uid = uid1 }
      { param = param2; kind = kind2; uid = uid2 } =
    let c = Variable.compare param1 param2 in
    if c <> 0
    then c
    else
      let c = Flambda_kind.With_subkind.compare kind1 kind2 in
      if c <> 0 then c else Flambda_debug_uid.compare uid1 uid2

  let equal t1 t2 = compare t1 t2 = 0

  let hash { param; kind; uid } =
    Hashtbl.hash
      ( Variable.hash param,
        Flambda_kind.With_subkind.hash kind,
        Flambda_debug_uid.hash uid )

  let print_debug_uid ppf duid =
    if !Clflags.dump_debug_uids
    then Format.fprintf ppf "%@{%a}" Flambda_debug_uid.print duid

  let [@ocamlformat "disable"] print ppf { param; kind; uid } =
    Format.fprintf ppf "@[(%t%a%a%t @<1>\u{2237} %a)@]"
      Flambda_colours.parameter
      Variable.print param
      print_debug_uid uid
      Flambda_colours.pop
      Flambda_kind.With_subkind.print kind
end)

let create param kind uid = { param; kind; uid }

let var t = t.param

let var_and_uid t = t.param, t.uid

let name t = Name.var (var t)

let simple t = Simple.var (var t)

let kind t = t.kind

let with_kind t kind = { t with kind }

let rename t = { t with param = Variable.rename t.param }

let is_renamed_version_of t t' =
  Flambda_kind.With_subkind.equal t.kind t'.kind
  && Variable.is_renamed_version_of t.param t'.param

let free_names { param; kind = _; uid = _ } =
  Name_occurrences.singleton_variable param Name_mode.normal

let apply_renaming { param; kind; uid } renaming =
  let param = Renaming.apply_variable renaming param in
  create param kind uid

let ids_for_export { param; kind = _; uid = _ } =
  Ids_for_export.add_variable Ids_for_export.empty param
