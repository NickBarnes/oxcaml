let () =
  let enabled_if =
    (* CR xclerc: on arm64, we currently get different results for a couple of
       cases (e.g. probes) *)
    {|(enabled_if (and (= %{context_name} "main") (= %{architecture} "amd64")) )|}
  in
  let buf = Buffer.create 1000 in
  let print_test ?(extra_flags = "-zero-alloc-check default") deps =
    let subst = function
      | "enabled_if" -> enabled_if
      | "deps" -> deps
      | "extra_flags" -> extra_flags
      | _ -> "assert false"
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(rule
 (alias   runtest)
 ${enabled_if}
 (deps ${deps})
 (action (run %{bin:ocamlopt.opt} %{deps} -g -c ${extra_flags} -dcse -dzero-alloc -dump-into-file -O3 -warn-error +a)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  let print_cmi_target name =
    let name_cmi = Filename.chop_extension name ^ ".cmi" in
    let subst = function
      | "enabled_if" -> enabled_if
      | "name" -> name
      | "name_cmi" -> name_cmi
      | _ -> "assert false"
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(rule
 (alias runtest)
 (deps ${name})
 (target ${name_cmi})
 ${enabled_if}
 (action (run %{bin:ocamlopt.opt} ${name} -g -c -opaque -stop-after typing -O3 -warn-error +a)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  let print_test_expected_output ?(filter = "filter.sh")
      ?(extra_flags = "-zero-alloc-check default") ?output ~cutoff ~extra_dep
      ~exit_code name =
    let extra_deps =
      match extra_dep with
      | None -> Printf.sprintf {|(:ml %s.ml)|} name
      | Some s ->
        if String.ends_with ~suffix:".ml" s || String.ends_with ~suffix:".mli" s
        then Printf.sprintf {|(:ml %s %s.ml)|} s name
        else Printf.sprintf {|%s (:ml %s.ml)|} s name
    in
    let output = Option.value output ~default:(name ^ ".output") in
    let subst = function
      | "enabled_if" -> enabled_if
      | "name" -> name
      | "output" -> output
      | "extra_deps" -> extra_deps
      | "exit_code" -> string_of_int exit_code
      | "cutoff" -> string_of_int cutoff
      | "extra_flags" -> extra_flags
      | "filter" -> filter
      | _ -> assert false
    in
    Buffer.clear buf;
    Buffer.add_substitute buf subst
      {|
(rule
 ${enabled_if}
 (targets ${output}.corrected)
 (deps ${extra_deps} ${filter})
 (action
   (with-outputs-to ${output}.corrected
    (pipe-outputs
    (with-accepted-exit-codes ${exit_code}
     (run %{bin:ocamlopt.opt} %{ml} -g -color never -error-style short -c
          -zero-alloc-checker-details-extra
          ${extra_flags} -zero-alloc-checker-details-cutoff ${cutoff} -O3))
    (run "./${filter}")
   ))))

(rule
 (alias   runtest)
 ${enabled_if}
 (deps ${output} ${output}.corrected)
 (action (diff ${output} ${output}.corrected)))
|};
    Buffer.output_buffer Out_channel.stdout buf
  in
  let default_cutoff = 20 in
  print_test "s.ml t.ml";
  print_test "test_flambda2_invalid.ml";
  print_test "t5.ml test_assume.ml";
  print_test "test_match_on_mutable_state.ml";
  print_test "test_flambda.ml";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail1";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail2";
  print_test_expected_output ~cutoff:0 ~extra_dep:(Some "t3.ml") ~exit_code:2
    "fail3";
  print_test_expected_output ~cutoff:0 ~extra_dep:(Some "t4.ml") ~exit_code:2
    "fail4";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail5";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail6";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail7";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:0 "fail8";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail9";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail10";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "fail12";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail13";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail14";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail15";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail16";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail17";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2 "fail18";
  (* test printing detailed error message only in flambda because the exact
     output depends on optimization level. *)
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:(Some "dep19.ml")
    ~exit_code:2 "fail19";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "fail20";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "fail21";
  print_test_expected_output ~cutoff:0 ~extra_dep:None ~exit_code:2
    "test_attribute_error_duplicate";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_attr_unused";
  (* Checks that the warning is printed and compilation is successful. *)
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:0
    "t6";
  (* Check that entry function and functors are ignored with [@@@zero_alloc
     all] *)
  print_test "t7.ml";
  (* Check that compiler generated stubs are ignored with [@@@zero_alloc all] *)
  print_test "test_stub_dep.ml test_stub.ml";
  (* generates an indirect call. *)
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "t1";
  (* deleting dead functions works *)
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_dep:(Some "test_warning199.mli") ~exit_code:0 "test_warning199";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:0
    "test_never_returns_normally";
  print_test_expected_output ~extra_flags:"-zero-alloc-check opt"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail22";
  print_test_expected_output ~extra_flags:"-zero-alloc-check opt"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail23";
  print_test ~extra_flags:"-zero-alloc-check opt" "test_zero_alloc_opt1.ml";
  print_test ~extra_flags:"-zero-alloc-check opt" "test_zero_alloc_opt2.ml";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_fail" ~extra_flags:"-directory repo/root/relative/dir";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_on_call" ~extra_flags:"-no-zero-alloc-checker-details-extra";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_misplaced_assume";
  print_test_expected_output ~extra_flags:"-zero-alloc-check all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_attr_check";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_attr_check_all";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_attr_check_opt";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:0
    "test_attr_check_none";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "fail24";
  print_test
    ~extra_flags:"-zero-alloc-check default -function-layout topological"
    "test_raise_message.ml";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:
      "-zero-alloc-check default -disable-precise-zero-alloc-checker \
       -function-layout source"
    ~extra_dep:None ~exit_code:2 "fail25";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:
      "-zero-alloc-check default -disable-zero-alloc-checker -function-layout \
       source"
    ~extra_dep:None ~exit_code:2 "fail26";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check default" ~extra_dep:None ~exit_code:2
    "test_all_opt";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check all" ~extra_dep:None ~exit_code:2
    "test_all_opt2";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check opt" ~extra_dep:None ~exit_code:2
    "test_all_opt3";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_arity";
  print_cmi_target "stop_after_typing.ml";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_signatures_functors";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_signatures_first_class_modules";
  print_cmi_target "test_signatures_separate_a.ml";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_signatures_separate.output"
    ~extra_dep:(Some "test_signatures_separate_a.cmi") ~exit_code:2
    "test_signatures_separate_b";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_signatures_separate.opt.output"
    ~extra_flags:"-zero-alloc-check all"
    ~extra_dep:(Some "test_signatures_separate_a.cmi") ~exit_code:2
    "test_signatures_separate_b";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_inlining";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_error";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_stub";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check default -zero-alloc-checker-join -2"
    ~extra_dep:None ~exit_code:2 ~filter:"filter_fatal_error.sh"
    "test_bounded_join";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check default -zero-alloc-checker-join 2"
    ~extra_dep:None ~exit_code:2 "test_bounded_join2";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check default -zero-alloc-checker-join 0"
    ~extra_dep:None ~exit_code:2 "test_bounded_join3";
  print_test_expected_output ~cutoff:3
    ~extra_flags:"-zero-alloc-check default -zero-alloc-checker-join 0"
    ~extra_dep:None ~exit_code:2 "test_bounded_join4";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_inference.output" ~extra_dep:(Some "test_inference.mli")
    ~exit_code:2 "test_inference";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_inference.opt.output" ~extra_dep:(Some "test_inference.mli")
    ~extra_flags:"-zero-alloc-check all" ~exit_code:2 "test_inference";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_remove_inferred_assume";
  print_test "test_remove_inferred_assume_workaround.ml";
  print_test_expected_output ~extra_flags:"-zero-alloc-assert all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail27";
  print_test_expected_output
    ~extra_flags:"-zero-alloc-assert all_opt -zero-alloc-check all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail27_opt";
  print_test_expected_output
    ~extra_flags:"-zero-alloc-assert default -zero-alloc-check all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail27_default";
  print_test_expected_output ~extra_flags:"-zero-alloc-check default"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail28";
  print_test_expected_output ~extra_flags:"-zero-alloc-check all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "fail28_opt";
  print_cmi_target "fail29.mli";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_dep:(Some "fail29.cmi") ~exit_code:2 "fail29";
  print_cmi_target "fail29_opt.mli";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_dep:(Some "fail29_opt.cmi") ~exit_code:2 "fail29_opt";
  print_cmi_target "fail29_opt2.mli";
  print_test_expected_output ~cutoff:default_cutoff
    ~extra_flags:"-zero-alloc-check all" ~extra_dep:(Some "fail29_opt2.cmi")
    ~exit_code:2 "fail29_opt2";
  print_test_expected_output ~extra_flags:"-zero-alloc-check all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2 "test_assume_unless_opt";
  print_test "test_assume_unless_opt.ml";
  print_test_expected_output ~extra_flags:"-zero-alloc-check default"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:0
    "test_assume_unless_opt_sig";
  print_test_expected_output ~extra_flags:"-zero-alloc-check all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_unless_opt_sig2";
  print_test_expected_output ~extra_flags:"-zero-alloc-check default"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_unless_opt_on_call";
  print_test_expected_output ~extra_flags:"-zero-alloc-check all"
    ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_assume_unless_opt_on_call2";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_custom_error_msg.output" ~extra_dep:None
    ~extra_flags:"-zero-alloc-check default" ~exit_code:2
    "test_custom_error_msg";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_custom_error_msg.opt.output" ~extra_dep:None
    ~extra_flags:"-zero-alloc-check all" ~exit_code:2 "test_custom_error_msg";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_custom_error_msg_sig.output"
    ~extra_dep:(Some "test_custom_error_msg_sig.mli")
    ~extra_flags:"-zero-alloc-check default" ~exit_code:2
    "test_custom_error_msg_sig";
  print_test_expected_output ~cutoff:default_cutoff
    ~output:"test_custom_error_msg_sig.opt.output"
    ~extra_dep:(Some "test_custom_error_msg_sig.mli")
    ~extra_flags:"-zero-alloc-check all" ~exit_code:2
    "test_custom_error_msg_sig";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_arch_specific_witness";
  print_test_expected_output ~cutoff:default_cutoff ~extra_dep:None ~exit_code:2
    "test_sort_witnesses";
  ()
